
module Library where
import PdePreludat

type Poder = (Nave -> Nave)

type Flota = [Nave]
data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poderEspecial :: String
}deriving Show

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
tieFighter = UnaNave {nombre = "TIE Fighter", durabilidad = 200, escudo = 100, ataque = 50, poderEspecial = "Movimiento turbo"}

xWing :: Nave
xWing = UnaNave {nombre = "X Wing", durabilidad = 300, escudo = 150, ataque = 100, poderEspecial = "Reparacion"}

darthVader :: Nave
darthVader = UnaNave {nombre = "Nave de Darth Vader", durabilidad = 500, escudo = 300, ataque = 200, poderEspecial = "Super turbo"}

millenniumFalcon :: Nave
millenniumFalcon = UnaNave {nombre = "Millennium Falcon", durabilidad = 1000, escudo = 500, ataque = 50, poderEspecial = "Reparacion de emergencia"}

speedyWings :: Nave
speedyWings = UnaNave {nombre = "Speedy Wings", durabilidad = 350, escudo = 100, ataque=800, poderEspecial = "Velocidad turbo" }

--Punto 2: Durabilidad total de una flota

calcularDurabilidad :: Flota -> Number
calcularDurabilidad laFlota = sum(map durabilidad  laFlota)

--Punto 3: Saber como queda una nave luego de ser atacada por otra



dañoRecibido :: Nave -> Nave -> Poder -> Poder -> Number
dañoRecibido naveAtacante naveAtacada elPoderAtacante elPoderAtacada  = ataque (elPoderAtacante naveAtacante) - escudo (elPoderAtacada naveAtacada)

ataqueDeNave :: Nave -> Nave -> Poder -> Poder -> Nave
ataqueDeNave naveAtacante naveAtacada elPoderAtacante elPoderAtacada
    |escudo naveAtacada < ataque naveAtacante = naveAtacada{durabilidad = durabilidad naveAtacada - dañoRecibido naveAtacante naveAtacada elPoderAtacante elPoderAtacada}
    |otherwise = naveAtacada




--Punto 4: Averiguar si una nave esta fuera de combate

fueraDeCombate :: Nave -> Bool
fueraDeCombate laNave = durabilidad laNave == 0





