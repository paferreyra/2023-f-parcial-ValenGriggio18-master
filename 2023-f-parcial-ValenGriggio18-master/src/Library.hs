module Library where
import PdePreludat

type Poder = (Nave, Nave)

data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poderEspecial :: Poder
}deriving Show

movTurbo :: Nave -> Nave
movTurbo laNave = laNave {ataque = ataque laNave + 25}

reparacion :: Nave -> Nave
reparacion laNave = laNave {ataque = ataque laNave - 30, durabilidad = durabilidad laNave + 50}

superTurbo :: Nave -> Nave 