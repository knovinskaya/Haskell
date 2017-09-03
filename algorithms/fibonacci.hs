{-Простая реализация нахождения чисел последовательности Фибоначчи, работает медленно, за счет большого количества рекурсивных вызовов-}
fibonacci :: Integer -> Integer

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci (-2) = -1

fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

{-Реализация с использованием вспомогательной функции, работает в разы быстрее-}
fibonacci2 :: Integer -> Integer

fibonacci2 = helper 0 1

helper cur prev n | n > 0 = helper (cur + prev) cur (n - 1)
                  | n == 0 = cur
                  | n < 0 = helper (prev - cur) cur (n + 1)