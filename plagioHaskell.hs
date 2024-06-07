

type Number = Int
type Obra=(String,Number)
contenido = fst
fecha = snd

data Autor = UnAutor{
   nombre :: String,
   obras :: [Obra]
} deriving (Show,Eq)

--Punto 1
-- A
obraA::Obra
obraA=("Había una vez un pato.",1997)
--B
obraB::Obra
obraB=("¡Habia una vez un pato!",1998)
--C
obraC::Obra
obraC=("Mirtha, Susana y Moria.",2010)
--D
obraD::Obra
obraD=("La semántica funcional del amoblamiento vertebral es riboficiente",2020)
--E
obraE::Obra
obraE=("La semántica funcional de Mirtha, Susana y Moria.",2022)

autor1 = UnAutor "nn1" [obraA]
autor2 = UnAutor "nn2" [obraB, obraC]
autor3 = UnAutor "nn3" [obraB, obraD]
autor4 = UnAutor "nn4" [obraE, obraB]

--Punto 2
versionCruda::String ->String
versionCruda =filter esLetraONumero.map sinAcento

esLetraONumero:: Char->Bool
esLetraONumero caracter = elem caracter todasLasLetrasYNumeros 

sinAcento::Char->Char
sinAcento 'á'='a'
sinAcento 'é'='e'
sinAcento 'í'='i'
sinAcento 'ó'='o'
sinAcento 'ú'='u'
sinAcento letra=letra


todasLasLetrasYNumeros = ['a'..'z']++['A'..'Z'] ++ "0123456789"

--plagios
--Punto 3

type FormaDeteccion= String->String->Bool

--a
copiaLiteral::FormaDeteccion
copiaLiteral texto textoOriginal = versionCruda texto == versionCruda  textoOriginal
--b

empiezaIgual :: Number->FormaDeteccion
empiezaIgual cantidadDeCaracteres texto textoOriginal = take cantidadDeCaracteres texto == take cantidadDeCaracteres textoOriginal &&  length texto < length textoOriginal
--c
leAgregaronIntro::FormaDeteccion
leAgregaronIntro texto textoOriginal = ultimosElementos (length textoOriginal)  texto == textoOriginal

ultimosElementos :: Number -> String ->String
ultimosElementos cant texto  = drop (length texto - cant) texto
--d

-- 
--punto 4
data Bot=UnBot{
    formas::[FormaDeteccion],
    fabricante::String
} 

botA :: Bot
botA = UnBot [copiaLiteral, leAgregaronIntro, empiezaIgual 10] "botter"

botB :: Bot
botB= UnBot [empiezaIgual 10, leAgregaronIntro] "botter"




--5. Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.

deteccion:: FormaDeteccion -> Obra -> Obra -> Bool
deteccion forma obra obraOriginal = fecha obra > fecha obraOriginal && forma (contenido obra) (contenido obraOriginal)  

esPlagioDeEstaObra bot obra obraOriginal = any   (\f -> deteccion f obra obraOriginal)  (formas bot)

--6. Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando alguna de sus obras es plagio de alguna de las del otro según el bot.

cadenaPlagiadores :: Bot ->  [Autor] -> Bool
cadenaPlagiadores bot [ _]  = False
cadenaPlagiadores bot [x1,x2]  = plagioA bot x1 x2
cadenaPlagiadores bot (x1:x2:xs)  = plagioA bot x1 x2 && cadenaPlagiadores bot (x2:xs)

plagioA :: Bot ->  Autor-> Autor -> Bool
plagioA bot autor autorOriginal = any   (esPlagioDeEsteAutor  bot autorOriginal) (obras autor)

esPlagioDeEsteAutor:: Bot -> Autor-> Obra -> Bool
esPlagioDeEsteAutor bot autorOriginal obra = any   (esPlagioDeEstaObra bot obra)   (obras autorOriginal)

--plagioA bot autor autorOriginal = any   (\obra -> any (esPlagioDeEstaObra bot obra) (obras autorOriginal)) (obras autor)

-- 7. Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.

aprendieron:: Bot -> [Autor] -> [Autor]
aprendieron bot autores = filter (\a ->  aprendio  bot  a (quitar a autores) ) autores 
 where quitar x = filter (/= x)

aprendio:: Bot -> Autor -> [Autor] -> Bool
aprendio bot autor autores =  length (obrasPlagiadasDelAutor bot autor autores) == 1

obrasPlagiadasDelAutor:: Bot -> Autor -> [Autor] -> [Obra]
obrasPlagiadasDelAutor bot autor autores =  filter  (esPlagioDeAlgunoDeEstosAutores  bot autores )    (obras autor) 

esPlagioDeAlgunoDeEstosAutores:: Bot -> [Autor] -> Obra -> Bool
esPlagioDeAlgunoDeEstosAutores  bot autores obra = any   (\autor -> esPlagioDeEsteAutor  bot autor obra)  autores