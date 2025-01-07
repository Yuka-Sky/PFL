until :: (a -> Bool) -> (a -> a) -> a -> a
mdc :: Integral a => t -> t
mdc a b = if b == 0 then a else mdc b (a'mod'b) until (\x -> x==2)(\x -> x+1) 0 mdc a b