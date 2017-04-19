import Text.Show.Functions
import Data.List

type Nombre = String
type PuntosVida = Int
type ReaccionAnteZombie = Zombie -> Protagonista -> Protagonista -- Este es el TIPO de las acciones (definidas abajo) --
type SeresQueridos = [Nombre]

data Protagonista = Protagonista {
   nombre :: Nombre,
   vida :: PuntosVida
   reaccion :: ReaccionAnteZombie
   seresQueridos :: SeresQueridos
  } deriving (Show)



daryl = Protagonista {nombre = "daryl", vida = 55, comerSemillasDeHermitaño 30 , ["carol"] }
maggie = Protagonista {nombre = "maggie", vida = 100, caer, ["carol", "daryl", "krilin"]}
carol = Protagonista {nombre = "carol", vida = 200, matar, ["victor"], }
krilin = Protagonista {nombre = "krilin", vida = 1, sacrificarse, []}
victor sueiro = Protagonista {nombre = "victor sueiro", vida = 1, sacrificarse, []}



zombieTranqui protagonista = dañoZombieTranqui protagonista
zombieConCasco protagonista = dañoZombieConCasco protagonista
zombieSinDientes = id
zombieBuenazoAstuto hambre | hambre < seresQueridos = zombieSinDientes
                           | otherwise = zombieConCasco
zombieReSacado = zombieTranqui && map zombieTranqui (map . zombieConCasco) seresQueridos --Fijate esto --



matar _ = id
caer unZombie = (unZombie . unZombie)
sacrificarse _ = protagonistaMuerto



dañoZombieTranqui protagonista = vida protagonista - 10 -- Podriamos reemplazarlo a "dañoZombieTranqui" y "dañoZombieConCasco directamente en zombieTranqui y zombieConCasco, respectivamente--
dañoZombieConCasco protagonista = div (vida protagonista) 2
protagonistaMuerto protagonista =  vida protagonista * 0
comerSemillasDeHermitaño cantidadDeSemillas | vida <= 0 = id
                                            | cantidadDeSemillas <= 10 = vida * 0 + 100 --Funciona si le paso a protagonista por Patter Matcihng????
                                            | cantidadDeSemillas > 10 = protagonistaMuerto


-- Mostrar en consola --

-- 3) (zombieTranqui . zombieConCasco) carol
-- 5) sacrificarse zombieTranqui carol
-- 6) caer zombieConCasco (matar zombieSinDientes maggie)

----------------------------------------------------------------------

--2.1) TIPOS
zombieTranqui :: Protagonista -> Protagonista
zombieConCasco :: Protagonista -> Protagonista
zombieSinDientes :: Protagonista -> Protagonista
matar :: Zombie -> Protagonista -> Protagonista
caer :: Zombie -> Protagonista -> Protagonista
sacrificarse :: Zombie -> Protagonista -> Protagonista

--6) HORDAS 

ataqueDeHordas :: -> -> -> -> Protagonista -- Fijate como pensarlo aca

