
module Library where
import PdePreludat

type Poder = (Nave -> Nave)

type Flota = [Nave]
data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poderEspecial :: Poder
}deriving (Show, Eq)

--Flota de ejemplo

laFlotita :: Flota
laFlotita = [tieFighter, xWing]

--Defino los poderes como funciones

movTurbo :: Nave -> Nave
movTurbo laNave = laNave {ataque = ataque laNave + 25}

reparacion :: Nave -> Nave
reparacion laNave = laNave {ataque = ataque laNave - 30, durabilidad = durabilidad laNave + 50}

superTurbo :: Nave -> Nave
superTurbo = movTurbo.movTurbo.movTurbo

reparacionEmergencia :: Nave -> Nave
reparacionEmergencia laNave = laNave {escudo = escudo laNave + 100}

velocidadTurbo :: Nave -> Nave
velocidadTurbo laNave = laNave{escudo = escudo laNave - escudo laNave, ataque = ataque laNave + escudo laNave }

--Modelado de las naves

tieFighter :: Nave
tieFighter = UnaNave {nombre = "TIE Fighter", durabilidad = 200, escudo = 100, ataque = 50, poderEspecial = movTurbo}

xWing :: Nave
xWing = UnaNave {nombre = "X Wing", durabilidad = 300, escudo = 150, ataque = 100, poderEspecial = reparacion}

darthVader :: Nave
darthVader = UnaNave {nombre = "Nave de Darth Vader", durabilidad = 500, escudo = 300, ataque = 200, poderEspecial = superTurbo}

millenniumFalcon :: Nave
millenniumFalcon = UnaNave {nombre = "Millennium Falcon", durabilidad = 1000, escudo = 500, ataque = 50, poderEspecial = reparacionEmergencia}

speedyWings :: Nave
speedyWings = UnaNave {nombre = "Speedy Wings", durabilidad = 350, escudo = 100, ataque=800, poderEspecial = velocidadTurbo}

--Punto 2: Durabilidad total de una flota

calcularDurabilidad :: Flota -> Number
calcularDurabilidad laFlota = sum(map durabilidad  laFlota)

--Punto 3: Saber como queda una nave luego de ser atacada por otra

dañoRecibido :: Nave -> Nave -> Number
dañoRecibido naveAtacante naveAtacada = ataque (poderEspecial naveAtacante naveAtacante) - escudo (poderEspecial naveAtacada naveAtacada)

ataqueDeNave :: Nave -> Nave -> Nave
ataqueDeNave naveAtacante naveAtacada
    |escudo naveAtacada < ataque naveAtacante && dañoRecibido naveAtacante naveAtacada < durabilidad naveAtacada = naveAtacada{durabilidad = durabilidad naveAtacada - dañoRecibido naveAtacante naveAtacada}
    |escudo naveAtacada < ataque naveAtacante && dañoRecibido naveAtacante naveAtacada >= durabilidad naveAtacada = naveAtacada{durabilidad = 0}
    |otherwise = naveAtacada

--Punto 4: Averiguar si una nave esta fuera de combate

fueraDeCombate :: Nave -> Bool
fueraDeCombate laNave = durabilidad laNave == 0

--Punto 5: Averiguar como queda una flota enemiga

escudoMenor200 :: Nave -> Bool
escudoMenor200 laNave = escudo laNave < 200

navesDebiles :: Flota -> Flota
navesDebiles = filter escudoMenor200

ataqueDesignado :: Number -> Nave -> Bool
ataqueDesignado valorAtaque laNave = ataque laNave > valorAtaque

navesPeligrosas :: Number -> Flota -> Flota
navesPeligrosas valorDesignado = filter (ataqueDesignado valorDesignado)

escudoMayor200 :: Nave -> Bool
escudoMayor200 laNave = escudo laNave > 200

navesTanques :: Flota -> Flota
navesTanques = filter escudoMayor200

navesFueraDeCombate :: Flota -> Flota
navesFueraDeCombate = filter (fueraDeCombate (ataqueDeNave))








