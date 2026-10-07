module Main where

import Regex
import System.Exit (exitFailure, exitSuccess)

-- (a|b)*c
abc :: Regex
abc = Seq (Star (Alt (Chr 'a') (Chr 'b'))) (Chr 'c')

-- (a|b)*c*
abcc :: Regex
abcc = Seq (Star (Alt (Chr 'a') (Chr 'b'))) (Star (Chr 'c'))


-- Letterlijke string als regex
str :: String -> Regex
str = foldr (Seq . Chr) Eps

a, b :: Regex
a = Chr 'a'
b = Chr 'b'

tests :: [(String, Bool)]
tests =
  -- nullable
  [ ("nullable Empty",         not (nullable Empty))
  , ("nullable Eps",           nullable Eps)
  , ("nullable Chr",           not (nullable a))
  , ("nullable Star",          nullable (Star a))
  , ("nullable Seq Eps a",     not (nullable (Seq Eps a)))
  , ("nullable Seq Eps Eps",   nullable (Seq Eps Eps))
  , ("nullable Alt Empty Eps", nullable (Alt Empty Eps))

  -- basisgevallen
  , ("Empty / \"\"",     not (matches Empty ""))
  , ("Empty / \"a\"",    not (matches Empty "a"))
  , ("Eps / \"\"",       matches Eps "")
  , ("Eps / \"a\"",      not (matches Eps "a"))
  , ("a / \"a\"",        matches a "a")
  , ("a / \"b\"",        not (matches a "b"))
  , ("a / \"aa\"",       not (matches a "aa"))
  , ("a / \"\"",         not (matches a ""))
  , ("a* / \"\"",        matches (Star a) "")
  , ("a* / \"aaaa\"",    matches (Star a) "aaaa")
  , ("a* / \"aab\"",     not (matches (Star a) "aab"))
  , ("ab / \"ab\"",      matches (str "ab") "ab")
  , ("ab / \"a\"",       not (matches (str "ab") "a"))
  , ("ab / \"ba\"",      not (matches (str "ab") "ba"))
  , ("a|b / \"b\"",      matches (Alt a b) "b")
  , ("a|b / \"ab\"",     not (matches (Alt a b) "ab"))

  -- deriv-tak waarin het eerste deel nullable is
  , ("a*ab / \"ab\"",    matches (Seq (Star a) (str "ab")) "ab")
  , ("a*ab / \"aaab\"",  matches (Seq (Star a) (str "ab")) "aaab")
  , ("a*ab / \"b\"",     not (matches (Seq (Star a) (str "ab")) "b"))
  , ("a*a* / \"aaa\"",   matches (Seq (Star a) (Star a)) "aaa")
  , ("(a*)* / \"aaa\"",  matches (Star (Star a)) "aaa")
  ]
  -- (a|b)*c
  ++ [ ("(a|b)*c / " ++ show s, matches abc s)
     | s <- ["c", "ac", "bc", "abababc", "aaaac", "bbbbc"] ]
  ++ [ ("(a|b)*c / " ++ show s, not (matches abc s))
     | s <- ["", "a", "ab", "ca", "cc", "abcc", "abcx", "dc", "abdc"] ]
   -- (a|b)*c*
   ++ [ ("(a|b)*c* / " ++ show s, matches abcc s)
     | s <- ["c", "ac", "bc", "abababc", "aaaac", "bbbbc", "", "a", "ab", "cc", "abcc"] ]
  ++ [ ("(a|b)*c* / " ++ show s, not (matches abcc s))
     | s <- ["ca", "abcx", "dc", "abdc"] ]

main :: IO ()
main = do
  let failed = [n | (n, ok) <- tests, not ok]
  mapM_ (putStrLn . ("FAIL: " ++)) failed
  putStrLn $ show (length tests - length failed) ++ "/" ++ show (length tests) ++ " geslaagd"
  if null failed then exitSuccess else exitFailure
