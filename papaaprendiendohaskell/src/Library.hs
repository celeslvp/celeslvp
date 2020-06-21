module Library where
import PdePreludat

--  PARTE 1

--Punto 1

data Ingrediente = UnIngrediente {
    nombreIngrediente :: String,
    calorias :: Number
} deriving (Eq, Show)

data Chocolate = UnChocolate {
    nombre :: String,
    ingredientes :: [Ingrediente],
    gramaje :: Number,
    gramosDeAzucar :: Number
} deriving (Show)

precioChocolate :: Chocolate -> Number
precioChocolate chocolate 
    |esAmargo chocolate = calculoPremium chocolate
    |otherwise = calculoGramaje chocolate

esAmargo :: Chocolate -> Bool
esAmargo chocolate = any (esCacaoAmargo) (ingredientes chocolate) 

esCacaoAmargo :: Ingrediente -> Bool
esCacaoAmargo ingrediente = ((>60) . porcentajeIngrediente) ingrediente && ((=="Cacao").nombreIngrediente) ingrediente

porcentajeIngrediente :: Ingrediente -> Number
porcentajeIngrediente ingrediente = calorias ingrediente * 0.5 --esto lo invente ya que no aclaraba nada sobre los porcentajes 

calculoPremium :: Chocolate -> Number
calculoPremium chocolate
    |aptoParaDiabetico chocolate = (*8).gramaje $ chocolate
    |otherwise = (*5).gramaje $ chocolate

aptoParaDiabetico :: Chocolate -> Bool
aptoParaDiabetico = (==0).gramosDeAzucar

calculoGramaje :: Chocolate -> Number
calculoGramaje chocolate
    |cantidadDeIngredientes chocolate > 4 = cantidadDeIngredientes chocolate * 8
    |otherwise = gramaje chocolate * 1.5

cantidadDeIngredientes :: Chocolate -> Number
cantidadDeIngredientes = length . ingredientes

--Punto 2 

esBombonAsesino :: Chocolate -> Bool
esBombonAsesino chocolate = any ((>200).calorias) $ ingredientes chocolate

totalCalorias :: Chocolate -> Number
totalCalorias chocolate = foldr ((+).calorias) 0 $ ingredientes chocolate

aptoParaNinios :: [Chocolate] -> [Chocolate]
aptoParaNinios = take 3 . filter (not . esBombonAsesino)

-- PARTE 2

--Punto 3 

type Proceso = Chocolate -> Chocolate

frutalizado :: String -> Number -> Proceso
frutalizado nombreFruta gramosDeFruta = agregarIngrediente (fruta nombreFruta gramosDeFruta) 

agregarIngrediente :: Ingrediente -> Chocolate -> Chocolate
agregarIngrediente ingrediente chocolate = chocolate {ingredientes = ingrediente : ingredientes chocolate}

fruta :: String -> Number -> Ingrediente
fruta nombre gramos = UnIngrediente {
    nombreIngrediente = nombre,
    calorias = (gramos*2)
}

ddleche = UnIngrediente {nombreIngrediente = "Dulce de leche", calorias = 220} --le puse ddLeche porque dulceDeLeche ya es nombre de una funcion

dulceDeLeche :: Proceso
dulceDeLeche = agregarIngrediente ddleche . agregarPalabraAlNombre "tentacion"

agregarPalabraAlNombre :: String -> Chocolate -> Chocolate
agregarPalabraAlNombre palabra chocolate = chocolate {nombre = nombre chocolate ++ " " ++ palabra}

agregarGramosDeAzucar :: Number -> Chocolate -> Chocolate
agregarGramosDeAzucar cantidad chocolate = chocolate {gramosDeAzucar = gramosDeAzucar chocolate + cantidad} 

celiaCrucera :: Number -> Proceso
celiaCrucera = agregarGramosDeAzucar 

embriagadora :: Number -> Chocolate -> Chocolate
embriagadora gradoDeAlcohol = agregarIngrediente (licor gradoDeAlcohol) . (agregarGramosDeAzucar 100) 

licor :: Number -> Ingrediente
licor grado = UnIngrediente "Licor" (caloriasLicorMaximas grado)

caloriasLicorMaximas :: Number -> Number
caloriasLicorMaximas grado 
    |grado < 30 = grado
    |otherwise = 30

--Punto 4 
--Dar un ejemplo de una receta que conste de los siguientes procesos: agregar 10 gramos de naranja, dulce de leche y un licor de 32 grados.

recetaPunto4 :: Proceso
recetaPunto4 = (frutalizado "Naranja" 10) . dulceDeLeche . embriagadora 32  

--Punto 5 

prepararChocolate :: Chocolate -> [Proceso] -> Chocolate
prepararChocolate = foldr ($)

--    PARTE 3

--Punto 6 

data Persona = UnaPersona {
    limiteDeSaturacion :: Number,
    noLeGusta :: Ingrediente -> Bool -- osea que si no le gusta, da True
} deriving (Show)

hastaAcaLlegue :: Persona -> [Chocolate] -> [Chocolate]
hastaAcaLlegue persona [] = []
hastaAcaLlegue persona (chocolate:chocolates) 
    |alcanzoSuLimite persona = []
    |(tieneAlgoQueNoLeGusta persona chocolate) = hastaAcaLlegue persona chocolates
    | otherwise = chocolate : hastaAcaLlegue (modificarLimite persona chocolate) chocolates

modificarLimite :: Persona -> Chocolate -> Persona --cada chocolate que come le va reduciendo su limite
modificarLimite persona chocolate = persona{
    limiteDeSaturacion = limiteDeSaturacion persona - totalCalorias chocolate
}

alcanzoSuLimite :: Persona -> Bool
alcanzoSuLimite  = (<= 0).limiteDeSaturacion

tieneAlgoQueNoLeGusta :: Persona -> Chocolate -> Bool 
tieneAlgoQueNoLeGusta persona chocolate = any (noLeGusta persona) $ ingredientes chocolate

--Punto 7 
--Dada una caja de chocolates infinitos ¿es posible determinar cuáles son los chocolates aptosParaNinios?

--Si es posible, ya que la funcion aptosParaNinios toma los primeros 3 de la lista que cumplen, y por la evaluacion diferida no sigue 
--evaluando el resto de los infinitos chocolates


-- ¿y totalCalorias? Justifique su respuesta, relacionándolo con un concepto visto en la materia.

--No, ese no es posible, ya que para saber el total de las calorias de la caja SI deberia saber cuantas calorias tiene cada chocolate,
--y como son infinitos chocolates nunca terminaría de evaluarlo




