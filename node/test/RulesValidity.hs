-- | Tests for the rule-based specification of the blockchain logic
--
-- The tests in this module aim at validating the <../../docs/rules/
-- rule-based-specification of the blockchain logic> against the current
-- implementation.
--
module RulesValidity (spec) where

import           Universum

import           Data.Functor.Identity (Identity, runIdentity)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Test.Hspec (Spec, describe, it, pending, shouldBe)
import           Test.QuickCheck (Arbitrary, Gen, Property, forAll, property,
                     (===))

import           Pos.Chain.Block (Block, mkGenesisBlock, verifyBlocks)
import           Pos.Chain.Lrc (genesisLeaders)
import           Pos.Core.Chrono (OldestFirst (OldestFirst))
import           Pos.Core.Slotting (EpochIndex (EpochIndex))
import           Serokell.Util (VerificationRes (VerSuccess))
import           Test.Pos.Chain.Genesis.Dummy (dummyBlockVersionData,
                     dummyConfig, dummyGenesisHash)
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)
import           UTxO.DSL (GivenHash)

import           Chain.Abstract (Addr (Addr), Chain)
import           Chain.Abstract.Translate.FromUTxO
                     (ChainValidity (InvalidChain, ValidChain), IntException)
import           Chain.Abstract.Translate.ToCardanoTmp (Abstract2Cardano)
import           Infrastructure.Generator (simpleGen)

spec :: Spec
spec = do
    -- TODO: Leaving the manuals tests for now till we can generate concrete
    -- chains from the abstract ones.
  describe "Manual tests" $ do
    it "Validates a manually generated chain " $
      traverse_ (`shouldBeA` ValidChain) [ emptyChain
                                         , singletonChain
                                         ]

  describe "Rule-based specification" $ do
    it "Produces valid chain according to actual validation rules" $ property $
      forAll simpleGen chainsAreValid

    it "Produces invalid chains that get rejected according to actual validation rules" $
      pending

  where
    shouldBeA :: OldestFirst [] Block -> ChainValidity -> IO ()
    shouldBeA ch validity =
      validateChain ch `shouldBe` asVerificationRes validity

    validateChain :: OldestFirst [] Block -> VerificationRes
    validateChain ch = verifyBlocks cfg Nothing dataMustbeKnown blockVersionData leaders ch

    -- TODO: we should define an Interpretation instance between these two
    -- types, for the sake of uniformity.
    asVerificationRes :: ChainValidity -> VerificationRes
    asVerificationRes ValidChain = VerSuccess
    asVerificationRes _ = error "Bad weather test cases not yet implemented"

    cfg = dummyConfig

    -- | Do we blocks to contain data we don't know about? Since we generate
    -- the blocks, the answer to this should be no.
    dataMustbeKnown = True

    blockVersionData = dummyBlockVersionData

    leaders = genesisLeaders dummyConfig

    emptyChain = OldestFirst []

    -- A single genesis block.
    genesisBlock =
      Left $ mkGenesisBlock
               dummyProtocolMagic
               (Left dummyGenesisHash)
               (EpochIndex 22) -- This doesn't seem to matter :thinking_face:
               leaders

    -- A singleton chain with the genesis block.
    singletonChain = OldestFirst [genesisBlock]

    chainsAreValid
      :: Either IntException (Chain GivenHash Addr, ChainValidity)
      -> Property
    chainsAreValid (Right (ch, ValidChain)) =
      validateChain (asConcreteChain ch) === VerSuccess
    chainsAreValid (Right (_, InvalidChain _)) =
      unexpectedError "when generating an abstract chain."
                      "`simpleGen` shouldn't generate invalid chains."
    chainsAreValid (Left e) =
      unexpectedError "when interpreting the abstract chain. "
                      "abstract chains should be interpreted without errors."

    unexpectedError :: Text -> Text -> a
    unexpectedError when why = error $
      when <> why
      <> "This shouldn't happen, and indicates an error in the tests."

    -- | Translate an abstract chain to its concrete counterpart.
    asConcreteChain :: Chain GivenHash Addr -> OldestFirst [] Block
    asConcreteChain = undefined
