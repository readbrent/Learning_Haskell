module Main where

import Test.HUnit

import LogAnalysis
import Log
main :: IO ()
main = runTestTT tests >>= print

test1 = TestCase $ assertEqual "" (LogMessage (Error 2) 562 "help help") (parseMessage "E 2 562 help help")

test2 = TestCase $ assertEqual "" (LogMessage Info 29 "La La La") (parseMessage "I 29 La La La")

test3 = TestCase $ assertEqual "" (Unknown "blah blah blah") (parseMessage "blah blah blah")

tests = TestList [TestLabel "test1" test1,
                 TestLabel "test2" test2,
                 TestLabel "test3" test3]