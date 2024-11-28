-- test/Spec.hs
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Core (Match(..), match)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "diwordle Tests"
  [ testCase "match \"posta\" \"seria\" produces the expected result" $
      match "posta" "seria" @?= [
          ('s', LugarIncorrecto),
          ('e', NoPertenece),
          ('r', NoPertenece),
          ('i', NoPertenece),
          ('a', Correcto)
        ]

  , testCase "match \"posta\" \"savia\" produces the expected result" $
      match "posta" "savia" @?= [
          ('s', LugarIncorrecto),
          ('a', NoPertenece),
          ('p', LugarIncorrecto),
          ('i', NoPertenece),
          ('a', Correcto)
        ]
  , testCase "match \"asado\" \"aosso\" produces the expected result" $
      match "asado" "aosso" @?= [
          ('a', Correcto),
          ('o', NoPertenece),
          ('s', LugarIncorrecto),
          ('s', NoPertenece),
          ('o', Correcto)
        ]


    -- You can add more tests as needed
  ]