

import Test.DocTest


main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Db.hs"
  , "app/Main.hs"
  ]
