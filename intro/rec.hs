
-- loop = (\x -> x x)(\x -> x x)

rec f = f(rec f)

fac f n = if (n < 3) then n else n * f(n - 1)

factorial n = rec(fac)(n)

main = putStrLn(show (factorial 10))
