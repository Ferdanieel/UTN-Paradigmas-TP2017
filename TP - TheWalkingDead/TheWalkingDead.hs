{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List

type Nombre = String
type Vida = Int
type Amigos = [Protagonista]
type Tipo = String
type Zombie = Protagonista -> Protagonista
type Reaccion =  Zombie -> Protagonista -> Protagonista
type Hambre = Int

data Protagonista = Protagonista {
   nombre :: Nombre,
   vida :: Vida,
   reaccion :: Reaccion,
   amigos :: Amigos
 } deriving Show

nuevoNombre unNOmbre unProtagonista = unProtagonista {nombre = unNOmbre}
nuevaVida unaVida unProtagonista = unProtagonista {vida = unaVida}
nuevaReaccion unaReaccion unProtagonista = unProtagonista {reaccion = unaReaccion}
nuevosAmigos unosAmigos unProtagonista = unProtagonista {amigos = unosAmigos}

instance Eq Protagonista where
  protagonista1 == protagonista2 =
    nombre protagonista1 == nombre protagonista2 &&
    vida protagonista1 == vida protagonista2

daryl = Protagonista {nombre = "daryl", vida = 55, reaccion = comerSemillasDeHermitanio 11, amigos = [carol] }
maggie = Protagonista {nombre = "maggie", vida = 100, reaccion = caer, amigos = [carol,daryl,krilin]}
carol = Protagonista {nombre = "carol", vida = 200, reaccion = matar , amigos = [victorSueiro]}
krilin = Protagonista {nombre = "krilin", vida = 1, reaccion = sacrificarse, amigos = []}
victorSueiro = Protagonista {nombre = "victorSueiro", vida = 1, reaccion = sacrificarse, amigos = []}

zombieTranqui :: Zombie
zombieTranqui unProtagonista | unProtagonista == carol = (reaccion unProtagonista) zombieTranqui (danioZombieTranqui unProtagonista)
                             | unProtagonista == daryl = (reaccion unProtagonista) zombieTranqui (danioZombieTranqui unProtagonista)
                             | unProtagonista == krilin = (reaccion unProtagonista) zombieTranqui (danioZombieTranqui unProtagonista)
                             | unProtagonista == maggie = (reaccion unProtagonista) zombieTranqui (danioZombieTranqui unProtagonista)
                             | unProtagonista == victorSueiro = (reaccion unProtagonista) zombieTranqui (danioZombieTranqui unProtagonista)
                             | otherwise = danioZombieTranqui unProtagonista

danioZombieTranqui :: Protagonista -> Protagonista
danioZombieTranqui unProtagonista = nuevaVida (vida unProtagonista - 10) unProtagonista

zombieConCasco :: Zombie
zombieConCasco unProtagonista | unProtagonista == carol = (reaccion unProtagonista) zombieConCasco (danioZombieConCasco unProtagonista)
                              | unProtagonista == daryl = (reaccion unProtagonista) zombieConCasco (danioZombieConCasco unProtagonista)
                              | unProtagonista == maggie = (reaccion unProtagonista) zombieConCasco (danioZombieConCasco unProtagonista)
                              | unProtagonista == krilin = (reaccion unProtagonista) zombieConCasco (danioZombieConCasco unProtagonista)
                              | unProtagonista == victorSueiro = (reaccion unProtagonista) zombieConCasco (danioZombieConCasco unProtagonista)
                              | otherwise = danioZombieConCasco unProtagonista

danioZombieConCasco :: Protagonista -> Protagonista
danioZombieConCasco unProtagonista = (nuevaVida (div (vida unProtagonista) 2) unProtagonista)

zombieSinDientes :: Zombie
zombieSinDientes = id

zombieBuenazoAstuto :: Hambre -> Protagonista -> Protagonista
zombieBuenazoAstuto unHambre unProtagonista | unHambre < length (amigos unProtagonista) = zombieTranqui unProtagonista
                                            | unHambre >= length (amigos unProtagonista) = zombieConCasco unProtagonista
zombieReSacado :: Zombie
zombieReSacado unProtagonista  = nuevosAmigos (map (zombieTranqui . zombieConCasco) (amigos unProtagonista)) (zombieTranqui unProtagonista)

pelearAmigos :: Protagonista -> [Protagonista] -> Protagonista
pelearAmigos unProtagonista unosProtagonistas = nuevosAmigos (diferenciaDeConjuntos (amigos unProtagonista) unosProtagonistas) unProtagonista

diferenciaDeConjuntos :: [Protagonista] -> [Protagonista] -> [Protagonista]
diferenciaDeConjuntos xs ys = filter (\x -> not (x `elem` ys)) (xs)

comerSemillasDeHermitanio :: Int -> Zombie -> Protagonista -> Protagonista
comerSemillasDeHermitanio semillas unZombie unProtagonista | vida unProtagonista <= 0 = unProtagonista
                                                           | vida unProtagonista > 0 && semillas <= 10 = alterarVidadebidoEfectoSemilla unProtagonista
                                                           | vida unProtagonista > 0 && semillas > 10 = muerte unProtagonista

alterarVidadebidoEfectoSemilla :: Protagonista -> Protagonista
alterarVidadebidoEfectoSemilla unProtagonista = nuevaVida (vida unProtagonista * 0 + 100) unProtagonista

horda :: Protagonista -> [Zombie] -> Protagonista
horda unProtagonista unosZombies = foldl (flip ($)) unProtagonista unosZombies



matar :: Reaccion
matar _ = id
caer :: Reaccion
caer unZombie = (.) unZombie unZombie
sacrificarse :: Reaccion
sacrificarse _ = muerte
muerte :: Protagonista -> Protagonista
muerte unProtagonista = nuevaVida (vida unProtagonista * 0) (nuevosAmigos (diferenciaDeConjuntos (amigos unProtagonista) [krilin,victorSueiro]) unProtagonista)

{- Consultas:
1) pelearAmigos maggie [daryl,carol,victorSueiro]
Protagonista {nombre = "maggie", vida = 100, reaccion = <function>, amigos =
[Protagonista {nombre = "krilin", vida = 1, reaccion = <function>, amigos = []}]}

2) zombieBuenazoAstuto maggie 5
Protagonista {nombre = "maggie", vida = 25, reaccion = <function>,
amigos = [Protagonista {nombre = "carol", vida = 200, reaccion = <function>,
amigos = [Protagonista {nombre = "victorSueiro", vida = 1, reaccion = <function>,
amigos = []}]},Protagonista {nombre = "daryl", vida = 55, reaccion = <function>,
amigos = [Protagonista {nombre = "carol", vida = 200, reaccion = <function>,
amigos = [Protagonista {nombre = "victorSueiro", vida = 1, reaccion = <function>,
amigos = []}]}]},Protagonista {nombre = "krilin", vida = 1, reaccion = <function>, amigos = []}]}

3) zombieBuenazoAstuto maggie 2
Protagonista {nombre = "maggie", vida = 80, reaccion = <function>,
amigos = [Protagonista {nombre = "carol", vida = 200, reaccion = <function>,
amigos = [Protagonista {nombre = "victorSueiro", vida = 1, reaccion = <function>,
amigos = []}]},Protagonista {nombre = "daryl", vida = 55, reaccion = <function>,
amigos = [Protagonista {nombre = "carol", vida = 200, reaccion = <function>,
amigos = [Protagonista {nombre = "victorSueiro", vida = 1, reaccion = <function>,
amigos = []}]}]},Protagonista {nombre = "krilin", vida = 1, reaccion = <function>, amigos = []}]}

4) horda maggie [zombieTranqui,zombieBuenazoAstuto 20,zombieReSacado,zombieBuenazoAstuto 1, zombieTranqui]
Protagonista {nombre = "maggie", vida = -40, reaccion = <function>,
amigos = [Protagonista {nombre = "carol", vida = 200, reaccion = <function>,
amigos = [Protagonista {nombre = "victorSueiro", vida = 1, reaccion = <function>,
amigos = []}]},Protagonista {nombre = "daryl", vida = 0, reaccion = <function>,
amigos = [Protagonista {nombre = "carol", vida = 200, reaccion = <function>,
amigos = [Protagonista {nombre = "victorSueiro", vida = 1, reaccion = <function>,
amigos = []}]}]},Protagonista {nombre = "krilin", vida = 0, reaccion = <function>, amigos = []}]}

5) filter (\x -> vida x == 0) (amigos (horda maggie [zombieTranqui,zombieBuenazoAstuto 20,zombieReSacado,zombieBuenazoAstuto 1, zombieTranqui]))
[Protagonista {nombre = "daryl", vida = 0, reaccion = <function>,
amigos = [Protagonista {nombre = "carol", vida = 200, reaccion = <function>,
amigos = [Protagonista {nombre = "victorSueiro", vida = 1, reaccion = <function>,
amigos = []}]}]},Protagonista {nombre = "krilin", vida = 0, reaccion = <function>, amigos = []}]

-}
