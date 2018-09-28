module Test.Pos.Types.Golden.SafeCopy where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.SafeCopy ()

import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4)
import           Test.Pos.Util.Golden (discoverGolden, goldenTestSafeCopy)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

golden_Address0 :: Property
golden_Address0 =
    goldenTestSafeCopy
        exampleAddress
        "test/golden/safecopy/Address0"

golden_Address1 :: Property
golden_Address1 =
    goldenTestSafeCopy
        exampleAddress1
        "test/golden/safecopy/Address1"

golden_Address2 :: Property
golden_Address2 =
    goldenTestSafeCopy
        exampleAddress2
        "test/golden/safecopy/Address2"

golden_Address3 :: Property
golden_Address3 =
    goldenTestSafeCopy
        exampleAddress3
        "test/golden/safecopy/Address3"

golden_Address4 :: Property
golden_Address4 =
    goldenTestSafeCopy
        exampleAddress4
        "test/golden/safecopy/Address4"

--------------------------------------------------------------------------------
-- Address'
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
