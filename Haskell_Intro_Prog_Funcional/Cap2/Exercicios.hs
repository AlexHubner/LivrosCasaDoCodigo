--  2.1) Gere as listas
--     a) [1, 11, 121, 131, 14641, 161051, 1771561]
questaoA :: [Int]
questaoA = [11 ^ x | x <- [0 .. 6]]

--  b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]	
questaoB :: [Int]
questaoB = [x | x <- [0 .. 40], x `mod` 4 /= 0]

-- c) ["AaBB",	"AbBB",	"AcBB",	"AdBB",	"AeBB",	"AfBB","AgBB"]	
questaoC = ['A' : x : 'B' : 'B' :[] | x <- ['a' .. 'g']]

-- d) [5,8,11,17,20,26,29,32,38,41]
questaoD :: [Int]
questaoD = [x + 3 | x <- [2, 5 .. 39], x /= 11 && x /= 20 && x /= 32]

-- e) [1.0,0.5,0.25,0.125,0.0625,0.03125]
questaoE :: [Float]
questaoE = [ x / 2 | x <- [2.0, 1.0, 0.5, 0.25, 0.125]]

-- f) [1,10,19,28,37,46,55,64]
questaoF :: [Int]
questaoF = [x+1| x <- [0, 9 .. 64]]

--g) [2,4,8,10,12,16,18,22,24,28,30]
questaoG :: [Int]
questaoG = [2*x+2 | x <- [0 .. 14], x /= 9 && x /= 12]

--h) ['@','A','C','D','E','G','J','L']
questaoH = [[x] | x <- ['@','A' .. 'L'], x /= 'B' && x /= 'F' && x /= 'H' && x /= 'I' && x /= 'K']

--  2.2) Crie uma função que verifique se o tamanho de uma String é par ou não. Use Bool como retorno

par :: Int -> Bool
par x 
    |x `mod` 2 == 0 = True
    |otherwise = False

questao2_2 :: String -> Bool
questao2_2 [] = False
questao2_2 x = par (length x)

-- 2.3) Escreva uma função que receba um vetor de Strings e retorne uma lista com todos os elementos
-- em ordem reversa

questao2_3 :: String -> String
questao2_3 x = reverse x

-- 2.4) Escreva uma função que receba um vetor de Strings e retorne uma lista com o tamanho de cada String.
-- As palavras de tamanho par devem ser excluídas da resposta.

questao2_4 :: [String] -> [Int]
questao2_4 xs = [length x | x <- xs, not (questao2_2 x)]

-- 2.5) Escreva a função head como composição de duas outras.

questao2_5 xs = head (reverse (questao2_4 xs))

-- 2.6) Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False.

questao2_6 :: String -> Bool
questao2_6 x
        | x == reverse x = True
        | otherwise = False

-- 2.7) Faça uma função que receba um inteiro e retorne uma tupla, contendo: o dobro deste número na primeira
-- coordenada, o triplo na segunda, o quádruplo na terceira e o quíntuplo na quarta.

questao2_7 :: Int -> (Int,Int,Int,Int)
questao2_7 x = (2*x,3*x,4*x,5*x)