module Onchain (mintSerialized, referenceSerialized, lockSerialized) where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as Scripts
import qualified Plutus.V2.Ledger.Api as Api
import Plutus.V2.Ledger.Contexts as Api
import Plutus.V1.Ledger.Value as V
import Ledger.Value as V
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (String)
import qualified Plutus.MerkleTree as MT

-- | Data and Redeemer ------------------------------------------------------------------

data DatumMetadata = DatumMetadata { metadata :: BuiltinData, version :: Integer, extra :: BuiltinData }

data ContractDetails = ContractDetails { 
                       extraOref          :: Api.TxOutRef
                     , royaltyName        :: Api.TokenName
                     , ipName             :: Api.TokenName
                     , oldCs              :: Api.CurrencySymbol
                     , merkleRoot         :: MT.Hash
                     , refAddress         :: Api.ValidatorHash
                     , lockAddress        :: Api.ValidatorHash
                     , nonce              :: Integer -- A magic number with 0 utility
                     }

data Action = MintNFT [MT.Proof] | BurnNFT | MintExtra

data RefAction = Burn | Move

instance Eq DatumMetadata where
    {-# INLINABLE (==) #-}
    DatumMetadata m v e == DatumMetadata m' v' e' = m == m' && v == v' && e == e'

labelLength = 4

-- | The primary minting policy for the NFT collection ------------------------------------------------------------------
-- MintNFT: Mints a pair of user token and reference NFT according to CIP-0068.
-- BurnNFT: Destroys this pair again. Only the holder of the NFT is allowed to proceed with that action.
-- MintExtra: Mints royalty and IP NFT
{-# INLINEABLE mintValidate #-}
mintValidate :: ContractDetails -> Action -> Api.ScriptContext -> Bool
mintValidate c action ctx = case action of
  MintNFT merkleProofs -> checkedMintNFT merkleProofs
  BurnNFT -> checkedBurnNFT
  MintExtra -> checkedMintExtra

  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txMint :: V.Value
    txMint = Api.txInfoMint txInfo

    ownSymbol :: Api.CurrencySymbol
    ownSymbol = Api.ownCurrencySymbol ctx

    checkedMintExtra :: Bool
    checkedMintExtra = -- | Mint royalty NFT (500) and Intellectual Property NFT (600)
                            Api.spendsOutput txInfo (Api.txOutRefId (extraOref c)) (Api.txOutRefIdx (extraOref c)) &&
                            txMint == V.singleton ownSymbol (royaltyName c) 1 <> V.singleton ownSymbol (ipName c) 1

    checkedBurnNFT :: Bool
    checkedBurnNFT = all (\(cs,_,am) -> if cs == ownSymbol then am == -1 else True) (V.flattenValue txMint)

    -- | We allow to migrate multiple SpaceBudz at a time. 
    -- To make that efficient we make use of the ordered outputs and value maps within the script context.
    checkedMintNFT :: [MT.Proof] -> Bool
    checkedMintNFT merkleProofs = checkHelper merkleProofs refOut lockOutFlatValue mint &&
                                  -- | Correct lock output datum
                                  (PlutusTx.unsafeFromBuiltinData lockOutDatum :: Api.CurrencySymbol) == ownSymbol
                                  where
                                    checkHelper [] [] [] [] = True
                                    checkHelper (merkleProof : merkleProofT) ((Api.OutputDatumHash (Api.DatumHash refOutDatumHash),refOutValue) : refOutT) 
                                                ((lockOutCs,Api.TokenName lockOutName, lockOutAm) : lockOutFlatValueT) 
                                                ((userCs, Api.TokenName userName, userAm) : (refCs, Api.TokenName refName, refAm) : mintT) = 
                                                let 
                                                  -- | Output with reference NFT
                                                  [(refOutCs,Api.TokenName refOutName,refOutAm)] = V.flattenValue (V.noAdaValue refOutValue)
                                                  -- | Create data for merkle tree (combination of asset names and datum hash from ref output)
                                                  merkleEntry = userName <> refName <> refOutName <> lockOutName <> refOutDatumHash
                                                in 
                                                  -- | Forcing to append datum with metadata to witness set 
                                                  -- (this will expose the metadata and not only the hash).
                                                  isJust (Api.findDatum (Api.DatumHash refOutDatumHash) txInfo) &&
                                                  -- | Matching policy id, quantities
                                                  1 == refOutAm && 1 == userAm && 1 == refAm && 1 == lockOutAm &&
                                                  ownSymbol == refOutCs && ownSymbol == userCs && ownSymbol == refCs && oldCs c == lockOutCs &&
                                                  -- | Check if metadata and asset names belong together and are part of the merkle tree
                                                  MT.member merkleEntry (merkleRoot c) merkleProof &&
                                                  -- | Check next 
                                                  checkHelper merkleProofT refOutT lockOutFlatValueT mintT
                                    -- | Sort mint in DESC order by asset name without CIP-0067 prefix
                                    mint = sortBy (\(_, Api.TokenName a, _) (_,Api.TokenName b, _) -> dropByteString labelLength b `compare` dropByteString labelLength a) (V.flattenValue txMint)
                                    refOut = scriptOutputsAt (refAddress c) txInfo
                                    -- | Output with locked old SpaceBudz
                                    [(Api.OutputDatum (Api.Datum lockOutDatum), lockOutValue)] = scriptOutputsAt (lockAddress c) txInfo
                                    -- | This is also in DESC order
                                    lockOutFlatValue = V.flattenValue (V.noAdaValue lockOutValue)


-- | The validator that holds the reference NFTs including the metadata.
{-# INLINEABLE referenceValidate #-}
referenceValidate :: BuiltinByteString -> DatumMetadata -> RefAction -> Api.ScriptContext -> Bool
referenceValidate label222 datumMetadata action ctx = case action of
  Burn -> checkedBurn
  Move -> checkedMove
  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txMint :: V.Value
    txMint = Api.txInfoMint txInfo

    ownValue :: V.Value
    ownValue =  let i = findOwnUniqueInput ctx
                    out = txInInfoResolved i
                in txOutValue out

    ownOutputDatumMetadata :: DatumMetadata
    ownOutputValue :: V.Value
    (ownOutputDatumMetadata, ownOutputValue) = case getContinuingOutputs ctx of
      [o] -> let (Api.OutputDatumHash h) = txOutDatum o in case Api.findDatum h txInfo of
        Just (Api.Datum d) -> case PlutusTx.fromBuiltinData d of
          Just m -> (m, txOutValue o)

    providesUserToken :: Api.CurrencySymbol -> Api.TokenName -> Integer -> Bool
    providesUserToken cs tn am = any (\(Api.TxInInfo _ out) -> valueOf (txOutValue out) cs tn >= am) (txInfoInputs txInfo)

    checkedBurn :: Bool
    checkedBurn = let
                    -- Allow burning only one pair (reference NFT and user token) at once
                    [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                    [(ownCs, Api.TokenName ownName, _)] = V.flattenValue (V.noAdaValue ownValue)
                  in
                    -- Matching policy id, quantities
                    -1 == userAm && -1 == refAm       &&
                    ownCs == userCs && ownCs == refCs && 
                    -- Matching asset names
                    dropByteString labelLength userName == dropByteString labelLength refName &&
                    ownName == refName

    checkedMove :: Bool
    checkedMove = let
                    noAdaOwnValue = V.noAdaValue ownValue
                    [(ownCs, Api.TokenName ownName, _)] = V.flattenValue noAdaOwnValue
                  in
                    -- Value matches
                    noAdaOwnValue == V.noAdaValue ownOutputValue && 
                    -- Metadata stays immutable
                    datumMetadata == ownOutputDatumMetadata &&
                    -- Moved by owner only
                    providesUserToken ownCs (Api.TokenName (label222 <> dropByteString labelLength ownName)) 1


-- | The validator that locks up the old SpaceBudz.
{-# INLINEABLE lockValidate #-}
lockValidate :: Api.CurrencySymbol -> Api.CurrencySymbol -> () -> Api.ScriptContext -> Bool
lockValidate oldCs newCs () ctx = checkedUnlock
  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txMint :: V.Value
    txMint = Api.txInfoMint txInfo

    ownValue :: V.Value
    ownDatum :: Api.OutputDatum
    ownValidatorHash :: Api.ValidatorHash
    (ownValue, ownDatum, ownValidatorHash) =  
                let i = findOwnUniqueInput ctx
                    out = txInInfoResolved i
                    Api.Address (Api.ScriptCredential validatorHash) _ = txOutAddress out 
                in (txOutValue out, txOutDatum out, validatorHash)

    checkedUnlock :: Bool
    checkedUnlock = let
                      noAdaOwnValue = V.noAdaValue ownValue
                      -- | Allow burning only one pair (reference NFT and user token) at once.
                      [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                      noLabelUserName = dropByteString labelLength userName -- "e.g. Bud123"
                      -- | Remove the asset from the total value that is allowed to be unlocked.
                      remainingLockedValue = noAdaOwnValue - V.singleton oldCs (Api.TokenName ("Space" <> noLabelUserName)) 1
                      -- | If there is remaining value locked then we need to check if there exists a new script output with this value.
                      [(lockOutDatum, lockOutValue)] = scriptOutputsAt ownValidatorHash txInfo
                    in
                      -- | Matching policy id, quantities
                      -1 == userAm && -1 == refAm &&
                      newCs == userCs && newCs == refCs &&
                      -- | Matching asset names
                      noLabelUserName == dropByteString labelLength refName &&
                      -- | If there is only lovelace left then you the script utxo can be destroyed.
                      -- However if there are still assets left then they need to be locked again with the correct datum!
                      if V.isZero remainingLockedValue then True 
                      else ownDatum == lockOutDatum && remainingLockedValue == V.noAdaValue lockOutValue

-- | Instantiate validators ------------------------------------------------------------------

mintInstance :: Scripts.MintingPolicy
mintInstance = Api.MintingPolicy $ Api.fromCompiledCode ($$(PlutusTx.compile [|| wrap ||]))
  where
    wrap c = Scripts.mkUntypedMintingPolicy $ mintValidate (PlutusTx.unsafeFromBuiltinData c)

referenceInstance :: Scripts.Validator
referenceInstance = Api.Validator $ Api.fromCompiledCode ($$(PlutusTx.compile [|| wrap ||]))
  where
    wrap l = Scripts.mkUntypedValidator $ referenceValidate (PlutusTx.unsafeFromBuiltinData l)

lockInstance :: Scripts.Validator
lockInstance = Api.Validator $ Api.fromCompiledCode ($$(PlutusTx.compile [|| wrap ||]))
  where
    wrap c = Scripts.mkUntypedValidator $ lockValidate (PlutusTx.unsafeFromBuiltinData c)

-- | Utils ------------------------------------------------------------------

{-# INLINABLE findOwnUniqueInput #-}
-- | Find the input currently being validated and make sure there is just one script in the transaction!
findOwnUniqueInput :: ScriptContext -> TxInInfo
findOwnUniqueInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Spending txOutRef} =
    case filter (\TxInInfo{txInInfoResolved} -> case txOutAddress txInInfoResolved of
                                          (Api.Address (Api.ScriptCredential _) _) -> True
                                          _ -> False) txInfoInputs
    of
      -- | The easy case: Either a lock utxo or a ref utxo
      [i] -> i
      -- | We need to make sure i1 and i2 are no the same validator to solve the double satisfaction problem. 
      -- In most cases it's not a problem, but for the two twin SpaceBudz it is (which have a quantity of 2)!
      -- There should never be a combination of (lock utxo, lock utxo) or (ref utxo, ref utxo). Only (lock utxo, ref utxo) is valid.
      [i1, i2] -> let
                    Api.Address (Api.ScriptCredential v1) _ = txOutAddress (txInInfoResolved i1)
                    Api.Address (Api.ScriptCredential v2) _ = txOutAddress (txInInfoResolved i2)
                    i = if txInInfoOutRef i1 == txOutRef then i1 else i2
                  in 
                    case v1 /= v2 of
                      True -> i

-- | Serialization ------------------------------------------------------------------

mintSerialized :: String
mintSerialized = C.unpack $ B16.encode $ serialiseToCBOR 
                      ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unMintingPolicyScript mintInstance) :: PlutusScript PlutusScriptV2)

lockSerialized :: String
lockSerialized = C.unpack $ B16.encode $ serialiseToCBOR 
                        ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript lockInstance) :: PlutusScript PlutusScriptV2)

referenceSerialized :: String
referenceSerialized = C.unpack $ B16.encode $ serialiseToCBOR 
                            ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript referenceInstance) :: PlutusScript PlutusScriptV2)

-- | Lift ------------------------------------------------------------------

PlutusTx.makeLift ''RefAction
PlutusTx.makeIsDataIndexed ''RefAction [('Burn, 0), ('Move, 1)]
PlutusTx.makeLift ''DatumMetadata
PlutusTx.makeIsDataIndexed ''DatumMetadata [('DatumMetadata, 0)]
PlutusTx.makeLift ''ContractDetails
PlutusTx.makeIsDataIndexed ''ContractDetails [('ContractDetails, 0)]
PlutusTx.makeLift ''Action
PlutusTx.makeIsDataIndexed ''Action [('MintNFT, 0), ('BurnNFT, 1), ('MintExtra, 2)]
PlutusTx.makeLift ''MT.Hash 