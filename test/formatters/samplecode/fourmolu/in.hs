-- Foo performs foo and sometimes bar.

foo :: Thoroughness
  -> Int -> Int
foo t x = if x > 20
    then case t of
           Thorough -> x + 50
           Somewhat -> x + 20
           NotAtAll -> 0
    else 10 + 1
