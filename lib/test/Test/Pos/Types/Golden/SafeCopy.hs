module Test.Pos.Types.Golden.SafeCopy where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.SafeCopy ()

import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress',
                     exampleAddress'1, exampleAddress'2, exampleAddress'3,
                     exampleAddress'4, exampleAddress1, exampleAddress2,
                     exampleAddress3, exampleAddress4)
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

golden_Address'0 :: Property
golden_Address'0 =
    goldenTestSafeCopy
        exampleAddress'
        "test/golden/safecopy/Address'0"

golden_Address'1 :: Property
golden_Address'1 =
    goldenTestSafeCopy
        exampleAddress'1
        "test/golden/safecopy/Address'1"

golden_Address'2 :: Property
golden_Address'2 =
    goldenTestSafeCopy
        exampleAddress'2
        "test/golden/safecopy/Address'2"

golden_Address'3 :: Property
golden_Address'3 =
    goldenTestSafeCopy
        exampleAddress'3
        "test/golden/safecopy/Address'3"

golden_Address'4 :: Property
golden_Address'4 =
    goldenTestSafeCopy
        exampleAddress'4
        "test/golden/safecopy/Address'4"

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
