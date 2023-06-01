
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
}deriving Show

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
speedyWings = UnaNave {nombre = "Speedy Wings", durabilidad = 350, escudo = 100, ataque=800, poderEspecial = velocidadTurbo }

calcularDurabilidad :: Flota -> Number
calcularDurabilidad laFlota = sum(map durabilidad  laFlota)



