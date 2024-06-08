type Number = Int -- Para quienes no usan el pdepreludat
data Obra = UnaObra {
    contenido :: String,
    fecha :: Number
} deriving (Show, Eq)

data Autor = UnAutor{
   nombre :: String,
   obras :: [Obra]
} deriving (Show,Eq)

--Punto 1
-- A
obraA :: Obra
obraA = UnaObra "Había una vez un pato." 1997
--B
obraB :: Obra
obraB = UnaObra "¡Habia una vez un pato!" 1998
--C
obraC :: Obra
obraC = UnaObra "Mirtha, Susana y Moria." 2010
--D
obraD :: Obra
obraD = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020
--E
obraE :: Obra
obraE = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

autor1 :: Autor
autor1 = UnAutor "nn1" [obraA]
autor2 :: Autor
autor2 = UnAutor "nn2" [obraB, obraC]
autor3 :: Autor
autor3 = UnAutor "nn3" [obraB, obraD]
autor4 :: Autor
autor4 = UnAutor "nn4" [obraE, obraB]

--Punto 2
versionCruda :: String -> String
versionCruda = filter esLetraONumero.map sinAcento

esLetraONumero :: Char ->  Bool
esLetraONumero caracter = elem caracter todasLasLetrasYNumeros 

sinAcento :: Char ->  Char
sinAcento 'á' = 'a'
sinAcento 'é' = 'e'
sinAcento 'í' = 'i'
sinAcento 'ó' = 'o'
sinAcento 'ú' = 'u'
sinAcento 'Á' = 'A'
sinAcento 'É' = 'E'
sinAcento 'Í' = 'I'
sinAcento 'Ó' = 'O'
sinAcento 'Ú' = 'U'
sinAcento letra = letra


todasLasLetrasYNumeros :: [Char]
todasLasLetrasYNumeros = ['a'..'z']++['A'..'Z'] ++ "0123456789 "

--plagios
--Punto 3

type FormaDeteccion = String ->  String ->  Bool

--a
copiaLiteral :: FormaDeteccion
copiaLiteral texto textoOriginal = versionCruda texto == versionCruda  textoOriginal
--b

empiezaIgual :: Number ->  FormaDeteccion
empiezaIgual cantidadDeCaracteres texto textoOriginal = take cantidadDeCaracteres texto == take cantidadDeCaracteres textoOriginal &&  length texto < length textoOriginal
--c
leAgregaronIntro :: FormaDeteccion
leAgregaronIntro texto textoOriginal = ultimosElementos (length textoOriginal)  texto == textoOriginal

ultimosElementos :: Number -> String -> String
ultimosElementos cant texto  = drop (length texto - cant) texto
--d

-- 
--punto 4
data Bot = UnBot {
    formas :: [FormaDeteccion],
    fabricante :: String
} 

botA :: Bot
botA = UnBot [copiaLiteral, leAgregaronIntro, empiezaIgual 10] "botter"

botB :: Bot
botB = UnBot [empiezaIgual 10, leAgregaronIntro] "botter"

--5. Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.

deteccion :: Obra -> Obra -> FormaDeteccion -> Bool
deteccion obra obraOriginal forma = fecha obra > fecha obraOriginal && forma (contenido obra) (contenido obraOriginal)  

esPlagioDeEstaObra :: Bot -> Obra -> Obra -> Bool
esPlagioDeEstaObra bot obra obraOriginal = any (deteccion obra obraOriginal)  (formas bot)

--6. Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando alguna de sus obras es plagio de alguna de las del otro según el bot.

cadenaPlagiadores :: Bot ->  [Autor] -> Bool
cadenaPlagiadores bot [ _] = False
cadenaPlagiadores bot [x1,x2] = plagioA bot x1 x2
cadenaPlagiadores bot (x1:x2:xs) = plagioA bot x1 x2 && cadenaPlagiadores bot (x2:xs)

plagioA :: Bot ->  Autor ->  Autor -> Bool
plagioA bot autor autorOriginal = any (esPlagioDeEsteAutor bot autorOriginal) (obras autor)

esPlagioDeEsteAutor :: Bot -> Autor ->  Obra -> Bool
esPlagioDeEsteAutor bot autorOriginal obra = any (esPlagioDeEstaObra bot obra) (obras autorOriginal)

--plagioA bot autor autorOriginal = any (\obra -> any (esPlagioDeEstaObra bot obra) (obras autorOriginal)) (obras autor)

-- 7. Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.

aprendieron :: Bot -> [Autor] -> [Autor]
aprendieron bot autores = filter (\a -> aprendio bot a (quitar a autores)) autores 
  where quitar x = filter (/= x)

aprendio :: Bot -> Autor -> [Autor] -> Bool
aprendio bot autor autores =  length (obrasPlagiadasDelAutor bot autor autores) == 1

obrasPlagiadasDelAutor :: Bot -> Autor -> [Autor] -> [Obra]
obrasPlagiadasDelAutor bot autor autores =  filter (esPlagioDeAlgunoDeEstosAutores bot autores) (obras autor) 

esPlagioDeAlgunoDeEstosAutores :: Bot -> [Autor] -> Obra -> Bool
esPlagioDeAlgunoDeEstosAutores  bot autores obra = any (\autor -> esPlagioDeEsteAutor bot autor obra) autores

--8---------------------------------------------------
obraInfinita :: Obra
obraInfinita = UnaObra (repeat 'a') 2021

--9---------------------------------------------------
obraInfinita2 :: Obra
obraInfinita2 = UnaObra (repeat 'a') 2025

{-

Suponiendo una consulta como: deteccion obraA obraInfinita copiaLiteral
No importa cuál sea la forma de detección, como la fecha de la obra que se pregunta si es plagio es anterior, no se cumple y corta diciendo False, por Lazy evaluation no es necesario seguir evaluando el otro lado del &&.

Ahora veamos los casos donde se cumple que la fecha es posterior:

copiaLiteral:
- Suponiendo la consulta: deteccion obraInfinita obraA copiaLiteral
da False, por Lazy Evaluation. Al evaluar la igualdad de contenidos no necesita evaluar toda la lista infinita para saber que los strings son distintos, con los primeros caracteres alcanza.
- Pero si consulto deteccion obraInfinita2 obraInfinita copiaLiteral, se cuelga, porque para saber si dos strings iguales son iguales necesita recorrerlos todos, aún con lazy evaluation.

empiezaIgual:
- Suponiendo la consulta: deteccion obraInfinita obraA empiezaIgual
Entonces da False, pues verifica con take que sean iguales los contenidos y eso da false. Por Lazy evaluation no es necesario evaluar la lista infinita para el take.
- Suponiendo la consulta: deteccion obraInfinita2 obraInfinita empiezaIgual
Ahí se cuelga, porque nunca llega a comparar las longitudes de los contenidos, aún con lazy evaluation. Es decir, aún si una es infinita y la otra empieza igual, jamás cortará.

leAgregaronIntro:
- Suponiendo la consulta: deteccion obraInfinita obraA leAgregaronIntro
Aún teniendo lazy evaluation, para el calcular el length del contenido de la obra infinita se cuelga, antes de poder hacer el drop.
- Ahora, si hacemos al revés: deteccion obraA obraInfinita leAgregaronIntro
Se colgaría, pues se pide hacer un ultimosElementos, que a su vez necesita el length de la lista infinita, no hay Lazy evaluation que lo salve.
-}
