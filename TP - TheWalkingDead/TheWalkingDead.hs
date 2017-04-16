import Text.Show.Functions
import Data.List
data Protagonista = Protagonista {
   nombre :: Nombre,
   vida :: PuntosVida
  } deriving (Show)

type Nombre = String
type PuntosVida = Int

daryl = Protagonista {nombre="daryl", vida = 55}
maggie = Protagonista {nombre="maggie", vida = 100}
carol = Protagonista {nombre="carol", vida = 200}

zombieTranqui protagonista = dañoZombieTranqui protagonista
zombieConCasco protagonista = dañoZombieConCasco protagonista
zombieSinDientes = id

matar _ = id
caer unZombie = (unZombie . unZombie)
sacrificarse _ = matarProtagonista

dañoZombieTranqui protagonista = vida protagonista - 10
dañoZombieConCasco protagonista = div (vida protagonista) 2
matarProtagonista protagonista =  vida protagonista * 0
-- Mostrar en consola --

-- 3) (zombieTranqui . zombieConCasco) carol
-- 5) sacrificarse zombieTranqui carol
-- 6) caer zombieConCasco (matar zombieSinDientes maggie)
