-- Páginas 20 e 21
data Dia = Segunda | Terca | Quarta | 
           Quinta | Sexta | Sabado | Domingo

agenda :: Dia -> String
agenda Domingo = "TV..."
agenda Sabado = "Festa"
agenda _ = "Trabalho"

-- Páginas 26 e 27
data Pessoa = Fisica String Int | Juridica String

teste :: Pessoa -> (String, String)
teste (Fisica x y) = ("Nome: " ++ x, "Idade: " ++ show y)
teste (Juridica x) = ("Nome: " ++x, "Nao ha idade.")

-- Páginas 28 e 29
data Ponto = Ponto {xval, yval :: Double}

-- Para fazer uma função que calcula a	distância do ponto à origem, podemos proceder de três formas.
--1. Primeira Forma:
distOrig :: Ponto -> Double
distOrig (Ponto x y) = sqrt(x**2 + y**2)

--2. Segunda Forma:
distOrig2 :: Ponto -> Double
distOrig2 (Ponto {xval=x, yval=y}) = sqrt(x**2 + y**2)

--3. Terceira Forma:
distOrig3 :: Ponto -> Double
distOrig3 p = sqrt(xval p**2 + yval p**2)