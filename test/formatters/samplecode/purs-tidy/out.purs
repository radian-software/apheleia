module Foo where

baz
  :: Array (String)
  -> Int
  -> Boolean
  -> Int
baz _ n = case _ of
  true -> 20 + 1
  false -> if n > 0 then 3 - (-2) else 0
