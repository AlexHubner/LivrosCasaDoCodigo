module Exercicios where

{--3.1)	Crie o tipo Pergunta com os	values	constructors Sim ou	Nao. Faça as funções seguintes,
determinando seus tipos explicitamente.
	pergNum	: recebe via parâmetro	uma	Pergunta. Retorna 0	para Nao e 1 para Sim.
	listPergs :	recebe	via	parâmetro uma lista de Perguntas, e	retorna	 0	s	e 1	s correspondentes
    aos constructores contidos na	lista.
	and': recebe duas Perguntas como parâmetro	e retorna a	tabela	verdade	do and lógico, usando Sim
    como verdadeiro	e Nao como falso.
	or': idem ao anterior,	porém deve ser usado o ou lógico.
	not': idem aos anteriores, porém usando	o not lógico. --}

data Pergunta = Sim | Nao

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs lista = [pergNum x | x <- lista]

and' :: Pergunta -> Pergunta -> Bool
and' Sim Sim = True
and' Sim Nao = False
and' Nao Sim = False
and' Nao Nao = False

or' :: Pergunta -> Pergunta -> Bool
or' Sim Sim = True
or' Sim Nao = True
or' Nao Sim = True
or' Nao Nao = False

not' :: Pergunta -> Pergunta -> Bool
not' Sim Sim = False
not' Nao Nao = True

{-- 3.2)	Faça o	tipo Temperatura que pode ter valores Celsius, Farenheit ou	Kelvin.	Implemente	as	funções:
	>> converterCelsius	:	 recebe	um	valor double e	uma temperatura, e faz a conversão	para Celsius.
	>> converterKelvin	:	 recebe	um	valor double e	uma temperatura, e	faz	a conversão	para Kelvin.
	>> converterFarenheit :	 recebe	um	valor double e	uma temperatura, e	faz	a conversão	para Farenheit.--}

data Temperatura = Celsius | Kelvin | Farenheit

converterCelsius :: Double -> Temperatura -> Double
converterCelsius 0 _ = 0
converterCelsius x Celsius = x
converterCelsius x Kelvin = x - 273.15
converterCelsius x Farenheit = (x - 32)/1.8

converterKelvin :: Double -> Temperatura -> Double
converterKelvin 0 _ = 0
converterKelvin x Kelvin = x
converterKelvin x Celsius = x + 273
converterKelvin x Farenheit = (x - 32)*5/9 + 273

converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit 0 _ = 0
converterFarenheit x Farenheit = x
converterFarenheit x Celsius = (x * 1.8) + 32
converterFarenheit x Kelvin = (x - 273) * 1.8 + 32

{--3.3)	Implemente uma função que simule o vencedor de uma partida de pedra, papel e tesoura usando
tipos criados. Casos de empate devem ser considerados em seu tipo. --}

data Partida = Pedra | Papel | Tesoura

jonKenPo :: Partida -> Partida -> String
jonKenPo Pedra Papel = "Vence Papel"
jonKenPo Pedra Tesoura = "Vence Pedra"
jonKenPo Pedra Pedra = "Empate"
jonKenPo Papel Pedra = "Vence Papel"
jonKenPo Papel Tesoura = "Vence Tesoura"
jonKenPo Papel Papel = "Empate"
jonKenPo Tesoura Pedra = "Vence Pedra"
jonKenPo Tesoura Papel = "Vence Tesoura"
jonKenPo Tesoura Tesoura = "Empate"

{--3.4)	 Faça uma função que retorne uma string, com todas as vogais maiúsculas	e minúsculas eliminadas	de uma
string	passada por	parâmetro usando list compreenshion.--}

questao3_4 :: String -> String
questao3_4 palavra = [x | x <- palavra, x /='a' && x /='e' && x /='i' && x /='o' && x /='u' && x /='A' && x /='E' && x /='I' && x /='O' && x /='U' ]

{--3.5) Sabe-se que as unidades imperiais de comprimento podem ser Inch, Yard ou Foot (há outras ignoradas
aqui). Sabe-se que 1in=0.0254m, 1yd=0.9144m, 1ft=0.3048. Faça a função converterMetros que recebe uma 
unidade imperial e o valor correspondente nesta unidade. Esta função deve retornar o valor em metros. 
		Implemente também a função converterImperial, que recebe um valor em metros e a unidade de conversão.
Esta função deve retornar o valor convertido para a unidade desejada. --}

data Imperial = Inch | Yard | Foot

converterMetros :: Imperial -> Double -> Double
converterMetros _ 0 = 0
converterMetros Inch x = x * 0.0254
converterMetros Yard x = x * 0.9144
converterMetros Foot x = x * 0.3048

converterImperial :: Double -> Imperial -> Double
converterImperial 0 _ = 0
converterImperial x Inch = x * 39.37
converterImperial x Yard = x * 1.0936
converterImperial x Foot = x * 3.2808

{--3.6 Faça um novo tipo chamado Mes, que possui como valores todos	os	meses do ano. Implemente:
	>> A função checaFim, que retorna o número de dias que cada mês possui (considere fevereiro tendo
28 dias).
	>> A função prox, que recebe um mês atual e retorna o próximo mês. 
	>> A função estacao, que retorna a estação do ano de acordo com o mês do hemisfério. --}

data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving Show


checaFim :: Mes -> Int
chacaFim Janeiro = 31
chacaFim Fevereiro = 28
checaFim Marco = 31
checaFim Abril = 30
checaFim Maio = 31
checaFim Junho = 30
checaFim Julho = 31
checaFim Agosto = 31
checaFim Setembro = 30
checaFim Outubro = 31
checaFim Novembro = 30
checaFim Dezembro = 31 

prox :: Mes -> Mes
prox Janeiro = Fevereiro
prox Fevereiro = Marco
prox Marco = Abril
prox Abril = Maio
prox Maio = Junho
prox Junho = Julho
prox Julho = Agosto
prox Agosto = Setembro
prox Setembro = Outubro
prox Outubro = Novembro
prox Novembro = Dezembro
prox Dezembro = Janeiro

estacao :: Mes -> String
estacao Janeiro = "Verao"
estacao Fevereiro = "Verao"
estacao Marco = "Outono"
estacao Abril = "Outono"
estacao Maio = "Outono"
estacao Junho = "Inverno"
estacao Julho = "Inverno"
estacao Agosto = "Inverno"
estacao Setembro = "Primavera"
estacao Outubro = "Primavera"
estacao Novembro = "Primavera"
estacao Dezembro = "Verao"

{-- 3.7) Faça uma função que receba uma	String e retorne True se esta for um palíndromo; caso contrário, False.--}

palin :: String -> Bool
palin x
	|x == reverse x = True
	|otherwise = False

{--3.8) Faça uma função que elimine todos os números pares. todos os ímpares múltiplos de 7 e negativos de uma 
lista de inteiros passada via parâmetro. Você deve retornar uma lista em ordem reversa em comparação a do 
parâmetro.--}

--Função que verifica se é par:
par :: Int -> Bool
par x 
    |x `mod` 2 == 0 = True
    |otherwise = False

--Função que verifica se é múltiplo de sete:
msete :: Int -> Bool
msete x
	|x `mod` 7 == 0 = True
	|otherwise = False

--Função para eliminar elementos:
elimina :: [Int] -> [Int]
elimina lista = [x | x <- lista, not (par x) && not (msete x) && x > 0]

--Função principal:
listareversa :: [Int] -> [Int]
listareversa lista = reverse (elimina lista)

{--3.9) Faça uma função	que	recebe três	Strings	x, y e z como parâmetro. A função retorna uma tupla com
três coordenadas contendo a	ordem reversa em cada. A primeira coordenada deve conter string	reversa	do	
primeiro parâmetro,	e assim	por	diante. --}

tuplaReversa :: String -> String -> String -> (String, String, String)
tuplaReversa x y z = (reverse x, reverse y, reverse z)

{--3.10) Faça uma função chamada revNum, que receba	uma String s e um Int n. Esta deverá retornar as n	
primeiras letras em	ordem reversa e	o restante em sua ordem	normal.	Exemplo:
revNum 4 "FATEC" = "ETAFC" --}

revNum :: String -> Int -> String
revNum palavra x = take x (reverse palavra)

{--3.11) Crie o	tipo de	dado Binario que pode ser Zero ou Um. Faça outro tipo de dado chamado Funcao que
pode ser Soma2, Maior, Menor ou	Mult2. Implemente a	função aplicar que	recebe	uma	Funcao	e dois Binarios.
Seu	retorno consiste em	executar a operação	desejada. Exemplo: 
aplicar Soma2 Um Um	= Zero--}

data Binario = Zero | Um deriving Show
data Funcao = Soma2 | Maior | Menor | Mult2 deriving Show

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Um Um = Zero
aplicar Soma2 Um Zero = Um
aplicar Soma2 Zero Um = Um
aplicar Soma2 Zero Zero = Um
aplicar Mult2 Um Um = Um
aplicar Mult2 Um Zero = Zero
aplicar Mult2 Zero Um = Zero
aplicar Mult2 Zero Zero = Zero
aplicar Maior Um Zero = Um
aplicar Maior Zero Um = Um
aplicar Menor Um Zero = Zero
aplicar Menor Zero Um = Zero

{--3.12) Faça uma função chamada binList, usando list compreeshion,	que recebe uma lista de	Binarios 
(ver exercício anterior) e retorna outra lista com elemento somado Um e convertido para	Int. Exemplo:
binList[Um, Zero, Zero,	Um,	Zero] =	[0,1,1,0,1]--}

converter :: Binario -> Int --Função para converter o tipo binário em inteiro
converter Zero = 0
converter Um = 1

binList :: [Binario] -> [Int]
binList lista = [converter (aplicar Soma2 Um x)|x <- lista]

{--3.13) Faça um novo tipo chamado Metros, que possui um value constructor de mesmo	nome, cujos	parâmetros
são: um	 Int que representa	a dimensão,	e um Double	que	representa o valor da medida e outro chamado 
MetragemInvalida. Implemente as funções:
>> areaQuadrado	:: Metros -> Metros: calcula a área	de um quadrado.
>> areaRet :: Metros -> Metros -> Metros: calcula a área de	um retângulo.
>> areaCubo	:: Metros -> Metros: calcula a área	de um cubo. 
Exemplo:
Prelude> areaQuadrado (Metros 1 2.0)	
Metros 2 4.0
Use o pattern matching para	ignorar as metragens erradas (calcular a área de um	quadrado com um	lado de
dimensão 4 não é válido) --}

data Metros = Metros{dim :: Int, med :: Double} | MetragemInvalida deriving Show

areaQuadrado :: Metros -> Metros
areaQuadrado (Metros 4 _) = MetragemInvalida
areaQuadrado (Metros x y) = Metros{dim = x*2, med = y**2}

areaRet :: Metros -> Metros -> Metros
areaRet (Metros x y) (Metros z w) = Metros{dim = x+z, med = (y*w)/2}

areaCubo :: Metros -> Metros
areaCubo (Metros x y) = Metros {dim = x, med = 6*(y**2)}



{-- 3.14)	Faça o novo	tipo Valido que possui dois value constructors Sim e Nao. O value constructor Sim
possui um parâmetro	(campo) String. Implemente uma função isNomeValido que recebe um nome e	retorna Nao caso
a String seja vazia; caso contrário, Sim	 --}

data Valido = Yes String | No deriving Show -- como as palavras Sim e Nao já estão em uso coloquei Yes e No

isNomeValido :: String -> Valido
isNomeValido [] = No
isNomeValido nome = Yes ("Sim")

{--3.15) Refaça	o exercício	3 do capítulo anterior	usando	record syntax e	tipos com parâmetro (siga o	 
exemplo	da conversão de medidas	SI para	imperial).--}

--QUESTÃO NÃO ENCONTRADA

{--3.16) Faça o	tipo Numero, que possui	um value constructor Ok com	um	campo double e	outro value	
constructor Erro com um	campo String. Faça a função	dividir	que	divida	dois números e,	caso o	segundo
número	seja 0,	emita um erro (use o pattern matching).	Exemplo: 
Prelude>	dividir	(Numero	6)	(Numero	5)	
Numero	1.2. --}

data Numero = Ok Double | Erro String deriving Show

dividir :: Double -> Double -> Numero
dividir _ 0 = Erro ("Divisao por 0")
dividir x y = Ok (x/y)

{--3.17) Faça o	tipo Cripto	que	possua dois	values	constructors Mensagem e	Cifrado, ambos	com	um
campo String e	um value constructor Erro. Faça	as	funções encriptar e decriptar, seguindo	cada
exemplo	a seguir.
Prelude> encriptar (Mensagem "FATEC")	
Cifrado	"GBUFD"	
Prelude> decriptar (Cifrado	"DBTB")	
Mensagem "CASA"
Veja que a encriptação deve empurrar cada letra a frente e a decriptação faz o inverso,	empurrando	uma
letra para trás. Use as funções succ e pred, e também list compreeshions. Não é possível encriptar mensagens
cifradas e decriptar mensagens.--}

data Cripto = Mensagem String | Cifrado String | Errou deriving Show

encriptar :: Cripto -> Cripto
encriptar (Mensagem []) = Errou
encriptar (Mensagem m) = Cifrado ([succ x|x <- m])

decriptar :: Cripto -> Cripto
decriptar (Cifrado []) = Errou
decriptar (Cifrado m) = Mensagem ([pred x|x <- m])

{--3.18) Faça uma função encriptarTodos	que	encripta (ou dá erro) todos	os elementos de	um vetor de	Cripto	--}

encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos palavra = [encriptar x|x <- palavra]

{--3.19) Tendo como	base o exercício de	conversão de medidas, crie uma função que faça conversão de	câmbio.
Você deve criar	o tipo Cambio contendo os value	constructors Euro, Real e Dollar. Crie também o	tipo Moeda
que	possui	os	campos val :: Double e cur :: Cambio. Use record syntax	e as taxas de conversão	do	dia	no
qual você fez o	exercício.--}

data Cambio = Euro | Real | Dollar deriving Show
data Moeda = Moeda {val :: Double, cur :: Cambio} deriving Show

converterEuro :: Moeda -> Moeda  
converterEuro (Moeda 0 _) = Moeda{val = 0, cur = Euro}
converterEuro (Moeda x Real) = Moeda{val = x * 6.42, cur = Real} 		--1 Euro = 6.42 Reais em 04/01/22
converterEuro (Moeda x Dollar) = Moeda{val = x * 1.1297, cur = Dollar}	--1 Euro = 1.1297 Dolares em 04/01/22

converterReal :: Moeda -> Moeda
converterReal (Moeda 0 _) = Moeda{val = 0, cur = Real}
converterReal (Moeda x Dollar) = Moeda{val = x * 0.1752, cur = Dollar} 	--1 Real = 0.1752 Dolares em 04/01/22
converterReal (Moeda x Euro) = Moeda{val = x * 0.1548, cur = Euro} 		--1 Real = 0.1548 Euros em 04/01/22

converterDollar :: Moeda -> Moeda
converterDollar (Moeda 0 _) = Moeda{val = 0, cur = Dollar}
converterDollar (Moeda x Real) = Moeda{val = x * 5.7081, cur = Real} 	--1 Dolar = 5.7081 Reais em 04/01/22
converterDollar (Moeda x Euro) = Moeda{val = x * 0.8839, cur = Euro} 	--1 Dolar = 0.8839 Euros em 04/01/22

{--3.20) Crie a	 função	 converterTodosReal	que recebe uma lista de	moedas e retorna outra lista de	
moedas com todos os seus elementos	convertidos	para Real. Use list	compreenshion. --}

converterTodosReal :: [Moeda] -> [Moeda]
converterTodosReal lista = [converterReal x|x <- lista]

{--3.21) Crie a	função maxMoeda	que	recebe uma lista de	moedas e retorna o	valor máximo absoluto (sem	
conversão alguma) dentre os	campos val desta lista.	Exemplo:
Prelude> maxMoeda [Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro]	
7
Use	a função maximum .--}

--Primeiro criei uma função para extrair o campo val dos ítens da lista;
obtemValor :: [Moeda] -> [Double]
obtemValor lista = [val x | x <- lista]

--Depois fiz a função peincipal.
maxMoeda :: [Moeda] -> Double
maxMoeda lista = maximum (obtemValor lista)