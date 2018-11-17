

import Test.DocTest


main = doctest
  [ "-isrc"
  , "src/Db.hs"
  , "app/Main.hs"
  ]
