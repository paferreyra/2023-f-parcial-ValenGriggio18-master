module Library where
import PdePreludat

type Poder = (Nave, Nave)

data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    poderEspecial :: Poder
}deriving Show
