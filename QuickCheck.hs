import Test.QuickCheck
import Text.Printf
import System.IO.Unsafe
import System.Process

jason :: String -> String
jason str = unsafePerformIO $ readProcess "./jason" [] str

j :: String -> String
j str = unsafePerformIO $ readProcess "jconsole" [] str

newtype JInput = JInput String deriving (Show)

instance Arbitrary JInput where
    arbitrary = JInput . printf "i. %d" . abs <$> (arbitrary :: Gen Int)


main :: IO ()
main = quickCheck $ \(JInput x) -> jason x === j x
