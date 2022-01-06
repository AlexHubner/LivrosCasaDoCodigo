dobroLista :: [Int] -> [Int]
dobroLista xs = [2*x | x <- xs]

lista :: [Int]
lista = [2*x+1 | x <- [0 .. 10], x/=5]