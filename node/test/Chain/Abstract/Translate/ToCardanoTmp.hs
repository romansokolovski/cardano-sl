{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Temporary module needed to hook-up the property based tests at
-- 'RulesValidity'. By using this module I (dnadales) can continue working on the property based
--
module Chain.Abstract.Translate.ToCardanoTmp
  (
    -- * Needed so interpretation intstances can be used with type annotations
    Abstract2Cardano
  ) where

import           Universum

import           Control.Lens (ix, (%=), (.=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Data.Default (def)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Wallet.Kernel.Types (RawResolvedBlock,
                     RawResolvedTx (..), mkRawResolvedBlock, mkRawResolvedTx,
                     rawResolvedBlock)
import           Chain.Abstract (Block (..), Chain, Output (..),
                     Transaction (..), hash, outAddr)
import           Pos.Chain.Block (BlockHeader, GenesisBlock, MainBlock)
import qualified Pos.Chain.Block as PosChain (Block)
import           Pos.Chain.Delegation (DlgPayload (UnsafeDlgPayload))
import           Pos.Chain.Ssc (defaultSscPayload)
import           Pos.Chain.Txp (TxAux (..), TxId, TxIn (..), TxOut (..),
                     TxOutAux (..))
import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Client.Txp.Util (makeMPubKeyTx, makeRedemptionTx)
import qualified Pos.Core as Core (ProtocolConstants, SlotId (..))
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Common (Coin (..), SlotLeaders, mkCoin)
import           Pos.Core.Slotting (localSlotIndexMaxBound)
import qualified Pos.Crypto (hash)
import           Pos.Crypto.Signing.Safe (SafeSigner (FakeSigner))
import           Pos.DB.Block (RawPayload (..), createMainBlockPure)
import           UTxO.Context (Addr, AddrInfo (..), BlockSignInfo (..),
                     blockSignInfoForSlot, resolveAddr)
import           UTxO.Crypto (ClassifiedInputs (InputsRedeem, InputsRegular),
                     RedeemKeyPair (..), RegularKeyPair (..), SomeKeyPair,
                     TxOwnedInput, classifyInputs)
import qualified UTxO.DSL as DSL (Hash, Input (..), Transaction, Value)
import           UTxO.IntTrans (ConIntT (..), IntCheckpoint (..),
                     IntException (..), IntRollback (..), Interpret (..),
                     Interpretation (..), constants, createEpochBoundary,
                     magic, mkCheckpoint)
import           UTxO.Translate (TranslateT (..), mapTranslateErrors,
                     translateNextSlot, withConfig)

data Abstract2Cardano

instance Interpretation Abstract2Cardano where
  type IntCtx Abstract2Cardano = IntT

instance DSL.Hash h Addr => Interpret Abstract2Cardano h (Chain h Addr) where
  type Interpreted Abstract2Cardano (Chain h Addr) = OldestFirst [] PosChain.Block

  int :: forall e m. Monad m
      => Chain h Addr -> IntT h e m (OldestFirst [] PosChain.Block)
  int = undefined
