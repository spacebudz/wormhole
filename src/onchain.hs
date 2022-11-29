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
                     , refAddress         :: Api.Address
                     , lockAddress        :: Api.Address
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
    -- e.g. flattend mint of two potential minted SpaceBudz (1,2,3) looks like: v = [(222)Bud3, (222)Bud2, (222)Bud1, (100)Bud3, (100)Bud2, (100)Bud1].
    -- Bud1 pair: (v!!0, v!!3), Bud2 pair: (v!!1, v!!4), Bud3 pair: (v!!2, V!!5)
    -- That's why we need to: '(length mint - 1) (length mint `divide` 2 - 1)' and decrement the number on both sides after each recursive call
    -- Also the other outputs need to be in a certain order (the locking outputs with the old SpaceBud), as well as the merkle trees
    -- Then we don't need to sort extra and save a lot of mem/cpu.
    -- Can this be done even more efficiently?
    checkedMintNFT :: [MT.Proof] -> Bool
    checkedMintNFT merkleProofs = checkHelper merkleProofs refOut lockOut (length mint - 1) (length mint `divide` 2 - 1)
                                  where
                                    -- | We need to make sure that mint is also empty by checking if length of mint is -1, otherwise you could mint anything you want.
                                    checkHelper [] [] [] _ userL = userL == -1
                                    checkHelper (merkleProof : merkleProofT) ((Api.OutputDatumHash (Api.DatumHash refOutDatumHash),refOutValue) : refOutT) ((Api.OutputDatum (Api.Datum lockOutDatum),lockOutValue) : lockOutT) refL userL = 
                                                let 
                                                  -- | Output with reference NFT
                                                  [(refOutCs,Api.TokenName refOutName,refOutAm)] = V.flattenValue (V.noAdaValue refOutValue)
                                                  -- | Output with locked old SpaceBud
                                                  [(lockOutCs,Api.TokenName lockOutName,lockOutAm)] = V.flattenValue (V.noAdaValue lockOutValue)
                                                  -- | Mint value (reference NFT and user token) 
                                                  [(refCs, Api.TokenName refName, refAm), (userCs, Api.TokenName userName, userAm)] = [mint!!refL, mint!!userL]
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
                                                  -- | Loop again 
                                                  checkHelper merkleProofT refOutT lockOutT (refL - 1) (userL - 1)
                                    mint = V.flattenValue txMint
                                    refOut = scriptOutputsAtAddress (refAddress c) txInfo
                                    lockOut = scriptOutputsAtAddress (lockAddress c) txInfo


-- | The validator that holds the reference NFTs including the metadata.
{-# INLINEABLE referenceValidate #-}
referenceValidate :: DatumMetadata -> RefAction -> Api.ScriptContext -> Bool
referenceValidate datumMetadata action ctx = case action of
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
    checkedMove = -- Value matches
                  V.noAdaValue ownValue == V.noAdaValue ownOutputValue && 
                  -- Metadata stays immutable
                  datumMetadata == ownOutputDatumMetadata


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
    wrap = Scripts.mkUntypedValidator $ referenceValidate

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