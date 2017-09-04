{-Нахождение значения определенного интеграла методом трапеций-}
integration :: (Double -> Double) -> Double -> Double -> Double

integration f a b =  h * (k + (helper 0 (a + h))) where
        h = (b - a) / 1000
        f1 = f a
        f2 = f b
        k = ( f1 + f2 ) / 2
        helper acc x | x < b = helper (acc + f x ) (x + h)
                     | otherwise = acc