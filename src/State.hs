{-|
Module      : State
Description : Le module 'State' définit l'état du jeu Gobblet et fournit des fonctions pour manipuler cet état.
Copyright   : (c) NAYERI POOR Ali , 2023
                  
Ce module 'State' contient toutes les définitions nécessaires pour représenter l'état du jeu Gobblet,
ainsi que les fonctions permettant de manipuler l'état du jeu, telles que l'ajout ou le retrait des gobelets
sur le plateau, la vérification des conditions de victoire, et l'initialisation de l'état du jeu.
-}
module State (
  -- * Types
  Position(..),              -- ^ Représente une position sur le plateau de jeu.
  State(..),                 -- ^ Représente l'état actuel du jeu.
  Piece(..),                 -- ^ Représente une pièce de jeu (X ou O).
  Taille(..),                -- ^ Représente la taille d'un gobelet.
  Gobelet(..),               -- ^ Représente un gobelet du jeu.
  Case,                      -- ^ Représente une case du plateau de jeu.
  
  -- * Fonctions d'affichage
  afficherPlateau,           -- ^ Affiche l'état actuel du plateau de jeu.
  afficherPileJoueurCourant, -- ^ Affiche les gobelets disponibles pour le joueur courant.

  -- * Logique du jeu
  verifieGagnant,            -- ^ Vérifie l'existence d'un gagnant dans l'état actuel du jeu.

  -- * Manipulation de l'état
  ajouterGobelet,            -- ^ Ajoute un gobelet à une position donnée sur le plateau.
  retirerGobelet,            -- ^ Retire un gobelet de la position spécifiée sur le plateau.
  retirerGobeletPileJoueur,  -- ^ Retire le gobelet spécifié de la pile du joueur courant.
  inverserJoueurCourant,     -- ^ Passe le tour au prochain joueur.
  initialiserEtatJeu,        -- ^ Initialise le jeu avec un état de départ.

  -- * Fonctions utilitaires
  piecesDisponibles,         -- ^ Renvoie les gobelets jouables par le joueur courant.
  estPlusGrosQue,            -- ^ Détermine si un gobelet est plus grand qu'un autre.
  gobletEnSurface            -- ^ Renvoie le gobelet visible à la position donnée, s'il y en a un.
) where

import Data.Foldable (asum)

---------------- Definition des types et des structures ---------------------

data Piece = X | O deriving (Show, Eq) 
data Taille = T | S | M | B deriving (Show, Eq, Ord)
data Position = Position { x :: Int, y :: Int } deriving (Show, Eq, Ord)
data Gobelet = Gobelet { piece :: Piece, taille :: Taille } deriving (Show, Eq)
data State = State {
  board :: [[Case]],
  pilesJoueurX :: ([Gobelet], [Gobelet], [Gobelet]),
  pilesJoueurO :: ([Gobelet], [Gobelet], [Gobelet]),
  joueurCourant :: Piece
} deriving (Show, Eq)

type Case = [Gobelet]

---------------- Fonction d'affichage des éléments d'un état  -----------------------

-- | Affiche un gobelet.
afficherGobelet :: Gobelet -> String
afficherGobelet (Gobelet pce tl) = show pce ++ show tl  -- Renamed to avoid shadowing

-- | Affiche le contenu d'une case.
afficherCase :: Case -> String
afficherCase [] = " . "
afficherCase (gob:_) = afficherGobelet gob  -- Renamed to avoid shadowing

-- | Affiche une ligne du plateau.
afficherLigne :: [Case] -> String
afficherLigne ligne = unwords $ map afficherCase ligne

-- | Affiche le plateau entier.
afficherPlateau :: [[Case]] -> IO ()
afficherPlateau plateau = do
    putStrLn "Plateau de jeu :"
    mapM_ (putStrLn . afficherLigne) plateau

-- | Affiche les gobelets en tête de pile pour le joueur courant.
afficherPileJoueurCourant :: State -> IO ()
afficherPileJoueurCourant st = do
    let currentPlayer = joueurCourant st
    let (p1, p2, p3) = if currentPlayer == X then pilesJoueurX st else pilesJoueurO st
    let pileHeads = map (taille . head) $ filter (not . null) [p1, p2, p3]
    putStrLn $ "Pile de gobelets du joueur " ++ show currentPlayer ++ ": " ++ unwords (map show pileHeads)

---------------- Fonction de modification d'un état    -----------------------

-- | Ajoute un gobelet à une position donnée.
ajouterGobelet :: State -> Gobelet -> Position -> State 
ajouterGobelet etat gobelet (Position posX posY) = etat { board = nouveauPlateau }
  where
    plateauActuel = board etat
    ligneActuelle = plateauActuel !! posX
    nouvelleLigne = take posY ligneActuelle ++ [gobelet : ligneActuelle !! posY] ++ drop (posY + 1) ligneActuelle
    nouveauPlateau = take posX plateauActuel ++ [nouvelleLigne] ++ drop (posX + 1) plateauActuel

-- | Retire un gobelet d'une position donnée.
retirerGobelet :: State -> Position -> State
retirerGobelet etat (Position posX posY) = etat { board = nouveauPlateau }
  where
    plateauActuel = board etat
    ligneActuelle = plateauActuel !! posX
    nouvelleLigne = take posY ligneActuelle ++ tail (ligneActuelle !! posY) : drop (posY + 1) ligneActuelle
    nouveauPlateau = take posX plateauActuel ++ [nouvelleLigne] ++ drop (posX + 1) plateauActuel

-- | Retire un gobelet de la pile du joueur courant.
retirerGobeletPileJoueur :: State -> Gobelet -> State
retirerGobeletPileJoueur etat gobelet =
    let (pile1, pile2, pile3) = if joueurCourant etat == X 
                                 then pilesJoueurX etat 
                                 else pilesJoueurO etat
        (nouvellePile1, gobeletTrouve1) = retirerGobeletDePile gobelet pile1
        (nouvellePile2, gobeletTrouve2) = if gobeletTrouve1 then (pile2, False) else retirerGobeletDePile gobelet pile2
        (nouvellePile3, _) = if gobeletTrouve1 || gobeletTrouve2 then (pile3, False) else retirerGobeletDePile gobelet pile3
        nouvellesPiles = (nouvellePile1, nouvellePile2, nouvellePile3)
    in if joueurCourant etat == X
       then etat { pilesJoueurX = nouvellesPiles }
       else etat { pilesJoueurO = nouvellesPiles }
  where
    retirerGobeletDePile :: Gobelet -> [Gobelet] -> ([Gobelet], Bool)
    retirerGobeletDePile gobeletAEnlever pile =
      let (avant, apres) = break (== gobeletAEnlever) pile
      in if null apres
         then (pile, False) -- Le gobelet n'est pas dans la pile, retourne la pile inchangée et False
         else (avant ++ tail apres, True) -- Retourne la nouvelle pile et True car le gobelet a été trouvé

-- | Inverse le joueur courant.
inverserJoueurCourant :: State -> State
inverserJoueurCourant st = st { joueurCourant = if joueurCourant st == X then O else X }

---------------- Fonctions d'itialisation d'un etat -----------------------

caseVide :: Case
caseVide = []
pileDebutJoueurX :: [Gobelet]
pileDebutJoueurX = [ Gobelet X B, Gobelet X M, Gobelet X S, Gobelet X T ]
pileDebutJoueurO :: [Gobelet]
pileDebutJoueurO = [ Gobelet O B, Gobelet O M, Gobelet O S, Gobelet O T ]

-- Fonction Initialisant le state 
initialiserEtatJeu :: State
initialiserEtatJeu = State {
  board = replicate 4 (replicate 4 caseVide),  
  pilesJoueurX = (pileDebutJoueurX, pileDebutJoueurX, pileDebutJoueurX),
  pilesJoueurO = (pileDebutJoueurO, pileDebutJoueurO, pileDebutJoueurO),
  joueurCourant = X  -- Le jeu commence avec le joueur X
}

---------------- Fonctions servant à évaluer un état  -----------------------

-- | Retourne les pièces disponibles du joueur courant.
piecesDisponibles :: State -> [Gobelet]
piecesDisponibles state =
  let (pile1, pile2, pile3) = if joueurCourant state == X then pilesJoueurX state else pilesJoueurO state
      disponibles = map head . filter (not . null) $ [pile1, pile2, pile3]
  in disponibles

-- | Détermine si un gobelet est plus gros qu'un autre.
estPlusGrosQue :: Gobelet -> Gobelet -> Bool
estPlusGrosQue (Gobelet _ taille1) (Gobelet _ taille2) = taille1 > taille2

-- | Renvoie le gobelet en surface à une position donnée.
gobletEnSurface :: State -> Position -> Maybe Gobelet 
gobletEnSurface st (Position i t) = 
      if i < 0 || i >= 4 || t < 0 || t >= 4 || null (caseAtPosition) 
      then Nothing 
      else Just (head caseAtPosition)
      where caseAtPosition = (board st !! i) !! t

-- | Vérifie si un joueur a gagné la partie en évaluant l'alignement de quatre gobelets identiques
-- en ligne, colonne ou diagonale. Renvoie 'Just Piece' si un joueur a gagné, 'Nothing' autrement.
--
-- >>> let etat = initialiserEtatJeu
-- >>> verifieGagnant etatNonGagnant
-- Nothing
-- >>> let etatGagnant = etat { board = replicate 4 (replicate 4 [Gobelet X T]) }
-- >>> verifieGagnant etatGagnant
-- Just X
--
verifieGagnant :: State -> Maybe Piece
verifieGagnant st = asum $ map (verifieAlignement st) (lignes ++ colonnes ++ diagonales)
    where
      tailleBoard = length (board st) - 1
      lignes = [[Position posX posY | posY <- [0..tailleBoard]] | posX <- [0..tailleBoard]]
      colonnes = [[Position posX posY | posX <- [0..tailleBoard]] | posY <- [0..tailleBoard]]
      diagonales = [[Position i i | i <- [0..tailleBoard]], [Position i (tailleBoard-i) | i <- [0..tailleBoard]]]

      verifieAlignement :: State -> [Position] -> Maybe Piece
      verifieAlignement st' positions =
          let pieces = map (gobletEnSurface st') positions
              hasFourX = all (\mbGoblet -> fmap piece mbGoblet == Just X) pieces
              hasFourO = all (\mbGoblet -> fmap piece mbGoblet == Just O) pieces
          in if hasFourX then Just X
             else if hasFourO then Just O
             else Nothing