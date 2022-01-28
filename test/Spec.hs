

import Lib
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

main :: IO ()
main = quickCheck $ monadicIO $ run f
