import Text.Show.Functions
data Protagonista = Protagonista Nombre PuntosVida deriving (Show)

type Nombre = String
type PuntosVida = Int

daryl = Protagonista "daryl" 55
maggie = Protagonista "maggie" 100
carol = Protagonista "carol" 200

zombieTranqui protagonista = dañoZombieTranqui protagonista
zombieConCasco protagonista = dañoZombieConCasco protagonista
zombieSinDientes = id

matar _ = id
caer unZombie = (unZombie . unZombie)
sacrificarse _ = matarProtagonista

dañoZombieTranqui (Protagonista unNombre unosPuntosVida) = Protagonista unNombre (unosPuntosVida - 10)
dañoZombieConCasco (Protagonista unNombre unosPuntosVida) = Protagonista unNombre (div unosPuntosVida 2)
matarProtagonista (Protagonista unNombre unosPuntosVida) =  Protagonista unNombre 0
-- Mostrar en consola --

-- 3) (zombieTranqui . zombieConCasco) carol
-- 5) sacrificarse zombieTranqui carol
-- 6) caer zombieConCasco (matar zombieSinDientes maggie)
