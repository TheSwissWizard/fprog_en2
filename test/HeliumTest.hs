module Main (main) where
import He.Html ( e, ea )
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [htmlTests]

htmlTests :: TestTree
htmlTests = testGroup "HTML tests"
  [ testCase "e works" $
      "<p>hallo</p>" @?= e "p" "hallo",

    testCase "ea with single attribute works" $
      "<p style='color:blue;'>hallo</p>" @?= ea "p" [("style", "color:blue;")] "hallo",

    testCase "ea with muultiple attribute works" $
      "<p style='color:blue;' id='42'>hallo</p>" @?= ea "p" [("style", "color:blue;"), ("id", "42")] "hallo" 
  ]

