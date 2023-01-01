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

data DatumMetadata = DatumMetadata {metadata :: BuiltinData, version :: Integer}

data ContractDetails = ContractDetails { 
                       extraOref          :: Api.TxOutRef
                     , royaltyName        :: Api.TokenName
                     , ipName             :: Api.TokenName
                     , oldCs              :: Api.CurrencySymbol
                     , merkleRoot         :: MT.Hash
                     , refAddress         :: Api.ValidatorHash
                     , lockAddress        :: Api.ValidatorHash
                     }

data Action = MintNFT [MT.Proof] | BurnNFT | MintExtra

data RefAction = Burn | Move

instance Eq DatumMetadata where
    {-# INLINABLE (==) #-}
    DatumMetadata a b == DatumMetadata c d = a == c && b == d

labelLength = 4

-- | The primary minting policy for the NFT collection ------------------------------------------------------------------
-- MintNFT: Mints a pair of user token and reference NFT according to CIP-0068.
-- BurnNFT: Destroys this pair again. Only the holder of the NFT is allowed to proceed with that action.
{-# INLINEABLE mintValidate #-}
mintValidate :: ContractDetails -> Action -> Api.ScriptContext -> Bool
mintValidate c action ctx = case action of
  MintNFT merkleProof -> checkedMintNFT merkleProof
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
    checkedBurnNFT =  let
                      -- | Allow burning only one pair (reference NFT and user token) at once
                      [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                    in
                       -- | Matching policy id, quantities
                      -1 == userAm && -1 == refAm &&
                      ownSymbol == userCs && ownSymbol == refCs &&
                      -- | Matching asset names
                      dropByteString labelLength userName == dropByteString labelLength refName

    -- | We allow to migrate multiple SpaceBudz at a time. 
    -- To make that efficient we make use of the ordered outputs and value maps within the script context.
    checkedMintNFT :: [MT.Proof] -> Bool
    checkedMintNFT merkleProofs = checkHelper merkleProofs refOut lockOut mint
                                  where
                                    checkHelper [] [] [] [] = True
                                    checkHelper (merkleProof : merkleProofT) ((Api.OutputDatumHash (Api.DatumHash refOutDatumHash),refOutValue) : refOutT) ((Api.OutputDatum (Api.Datum lockOutDatum),lockOutValue) : lockOutT) ((userCs, Api.TokenName userName, userAm) : (refCs, Api.TokenName refName, refAm) : mintT) = 
                                                let 
                                                  -- | Output with reference NFT
                                                  [(refOutCs,Api.TokenName refOutName,refOutAm)] = V.flattenValue (V.noAdaValue refOutValue)
                                                  -- | Output with locked old SpaceBud
                                                  [(lockOutCs,Api.TokenName lockOutName,lockOutAm)] = V.flattenValue (V.noAdaValue lockOutValue)
                                                  -- | Create data for merkle tree (combination of asset names and datum hash from ref output)
                                                  merkleEntry = userName <> refName <> refOutName <> lockOutName <> refOutDatumHash
                                                in 
                                                  -- | Forcing to append datum with metadata to witness set (this will expose the metadata and not only the hash).
                                                  isJust (Api.findDatum (Api.DatumHash refOutDatumHash) txInfo) &&
                                                  -- | Matching policy id, quantities
                                                  1 == refOutAm && 1 == userAm && 1 == refAm && 1 == lockOutAm &&
                                                  ownSymbol == refOutCs && ownSymbol == userCs && ownSymbol == refCs && oldCs c == lockOutCs &&
                                                  -- | Correct lock output datum
                                                  (PlutusTx.unsafeFromBuiltinData lockOutDatum :: Api.CurrencySymbol) == ownSymbol &&
                                                  -- | Check if metadata and asset names belong together and are part of the merkle tree
                                                  MT.member merkleEntry (merkleRoot c) merkleProof &&
                                                  -- | Check next 
                                                  checkHelper merkleProofT refOutT lockOutT mintT
                                    mint = sortBy (\(_, Api.TokenName a, _) (_,Api.TokenName b, _) -> dropByteString labelLength a `compare` dropByteString labelLength b) (V.flattenValue txMint)
                                    refOut = scriptOutputsAt (refAddress c) txInfo
                                    lockOut = scriptOutputsAt (lockAddress c) txInfo


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
    ownValue =  let Just i = Api.findOwnInput ctx
                    out = txInInfoResolved i
                in txOutValue out

    ownOutputDatumMetadata :: DatumMetadata
    ownOutputValue :: V.Value
    (ownOutputDatumMetadata, ownOutputValue) = case getContinuingOutputs ctx of
      [o] -> let (Api.OutputDatumHash h) = txOutDatum o in case Api.findDatum h txInfo of
        Just (Api.Datum d) -> case PlutusTx.fromBuiltinData d of
          Just m -> (m, txOutValue o)

    providesUserToken :: Api.CurrencySymbol -> Api.TokenName -> Integer -> Bool
    providesUserToken cs tn am = any (\(Api.TxInInfo _ out) -> valueOf (txOutValue out) cs tn == am) (txInfoInputs txInfo)

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
                    [(ownCs, Api.TokenName ownName, _)] = V.flattenValue (V.noAdaValue ownValue)
                  in
                    -- Value matches
                    V.noAdaValue ownValue == V.noAdaValue ownOutputValue && 
                    -- Metadata stays immutable
                    datumMetadata == ownOutputDatumMetadata &&
                    -- Moved by owner
                    providesUserToken ownCs (Api.TokenName (label222 <> dropByteString labelLength ownName)) 1


-- | The validator that locks up the old SpaceBudz.
{-# INLINEABLE lockValidate #-}
lockValidate :: Api.CurrencySymbol -> () -> Api.ScriptContext -> Bool
lockValidate newCs () ctx = checkedUnlock
  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txMint :: V.Value
    txMint = Api.txInfoMint txInfo

    ownValue :: V.Value
    ownValue =  let Just i = Api.findOwnInput ctx
                    out = txInInfoResolved i
                 in txOutValue out

    checkedUnlock :: Bool
    checkedUnlock = let
                      -- | Allow burning only one pair (reference NFT and user token) at once
                      [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                      -- | Own input with correct asset
                      [(_, Api.TokenName oldName, oldAm)] = V.flattenValue (V.noAdaValue ownValue)
                    in
                      -- | Matching policy id, quantities
                      -1 == userAm && -1 == refAm && 1 == oldAm &&
                      newCs == userCs && newCs == refCs &&
                      -- | Matching asset names
                      dropByteString labelLength userName == dropByteString labelLength refName &&
                      -- | Check if user name matches with the old SpaceBud name (e.g. Bud123 == Bud123)
                      dropByteString labelLength userName == dropByteString 5 oldName

-- | Utils ------------------------------------------------------------------

{-# INLINEABLE scriptOutputsAtAddress #-}
scriptOutputsAtAddress :: Api.Address -> Api.TxInfo -> [(Api.OutputDatum, V.Value)]
scriptOutputsAtAddress address p =
    let flt Api.TxOut{Api.txOutDatum=d, txOutAddress=address', txOutValue} | address == address' = Just (d, txOutValue)
        flt _ = Nothing
    in mapMaybe flt (Api.txInfoOutputs p)

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
    wrap = Scripts.mkUntypedValidator $ lockValidate

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