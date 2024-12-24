-- [f x | x ← xs, p x]
f1 :: (a -> b) -> (a -> Bool) -> p -> [b]
f1 f p x = map f(filter p xs)