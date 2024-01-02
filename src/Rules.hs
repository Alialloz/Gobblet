{-|
Module      : Rules
Description : Le module 'Rules' fournit les règles du jeu Gobblet, y compris la validation des mouvements et la gestion de l'état du jeu.
Maintainer  : exemple@email.com
Stability   : experimental
Portability : portable

Ce module implémente les règles du jeu Gobblet, permettant d'appliquer des mouvements valides à l'état du jeu,
de vérifier la validité des mouvements, et d'obtenir les mouvements possibles pour un état donné.
-}
module Rules (
  applyMove,                -- ^ Applique un mouvement à l'état actuel du jeu si celui-ci est valide.
  availableOnBoard,         -- ^ Liste les mouvements possibles 'OnBoard' pour l'état actuel.
  availableDrop,            -- ^ Liste les mouvements possibles 'Drop' pour l'état actuel.
  listePosGobletJoueurIG,   -- ^ Renvoie les positions des gobelets jouables pour le joueur courant.
  availableMoves,           -- ^ Renvoie tous les mouvements possibles pour l'état actuel.
  applyMoves,               -- ^ Applique une série de mouvements à l'état initial du jeu.
  listeCasesVides,          -- ^ Liste toutes les cases vides sur le plateau.
  listePiecesJouable,       -- ^ Liste toutes les pièces jouables pour le joueur courant.
  listePositionPieceAdv3    -- ^ Liste les positions où l'adversaire a trois gobelets consécutifs.
) where

import qualified Data.Set as S
import Data.Maybe (isJust)
import Move 
import State 
import qualified Data.List as List
import Data.List (groupBy, maximumBy)
import Data.Ord (comparing)


-- | Essaye d'appliquer plusieurs mouvements consécutifs à partir de l'état initial du jeu.
applyMoves :: [Move] -> Maybe State
applyMoves moves = applyMovesHelper moves initialiserEtatJeu
  where
    applyMovesHelper :: [Move] -> State -> Maybe State
    applyMovesHelper [] state = Just state
    applyMovesHelper (m:ms) state =
      case applyMove state m of
        Just newState -> applyMovesHelper ms newState
        Nothing -> Nothing


-- | Fonction appliquant un mouvement à un état et 
applyMove :: State -> Move -> Maybe State
applyMove state move =
  if S.member move (availableMoves state) then
    case move of
      Drop tailleG position ->
        -- Find a goblet of the specified size belonging to the current player
        case List.find (\(Gobelet _ t) -> t == tailleG) (piecesDisponibles state) of
          Just gobeletTrouve -> 
            -- Add the goblet to the board, remove it from the player's supply, and switch players
            let nvState = ajouterGobelet state gobeletTrouve position
                nvState1 = retirerGobeletPileJoueur nvState gobeletTrouve
                nvState2 = inverserJoueurCourant nvState1 
            in Just nvState2
          Nothing -> Nothing -- No goblet of the specified size found
      OnBoard pos1 pos2 ->
        case gobletEnSurface state pos1 of
          Just gobelet -> 
            -- Move the goblet to the new position, remove it from the original position, and switch players
            let nvState = ajouterGobelet state gobelet pos2
                nvState1 = retirerGobelet nvState pos1
                nvState2 = inverserJoueurCourant nvState1
            in Just nvState2
          Nothing -> Nothing -- No goblet at the original position
  else Nothing -- The move is not available

-- | Renvoie l'ensemble des mouvements possibles à partir d'un état de jeu.
availableMoves :: State -> S.Set Move
availableMoves st =
    if isJust (verifieGagnant st)
    then S.empty
    else S.fromList (availableDrop st ++ availableOnBoard st)

-- Fonctions utilisées par availableMoves 

-- | Renvoie tous les mouvements 'Drop' valides à partir d'un état donné.
availableDrop :: State -> [Move]
availableDrop st =
    let gobeletsJouables = listePiecesJouable st
        casesVides = listeCasesVides st
        toutesLesPositionsAdverses = listePositionPieceAdv3 st
        -- Affiche la liste des positions adverses avant de retourner les mouvements
    in [Drop (taille gobelet) pos | gobelet <- gobeletsJouables, pos <- casesVides ++ filterPositionsPlusPetites st gobelet toutesLesPositionsAdverses]
  where
    filterPositionsPlusPetites st' gobeletRef positions = filter isSmaller positions
      where
        isSmaller pos = case gobletEnSurface st' pos of
                          Just gobletSurf -> estPlusGrosQue gobeletRef gobletSurf
                          Nothing -> False

-- | Renvoie tous les mouvements 'Drop' valides à partir d'un état donné.
availableOnBoard :: State -> [Move]
availableOnBoard st =
    let positionsGobeletsJoueur = listePosGobletJoueurIG st
        toutesLesPositions = [(Position posX posY) | posX <- [0..3], posY <- [0..3]]
        movesPourPosition pos = [OnBoard pos dest | dest <- toutesLesPositions, peutSeDeplacer pos dest]
        peutSeDeplacer startPos destPos =
            case (gobletEnSurface st startPos, gobletEnSurface st destPos) of
                (_, Nothing) -> True
                (Just gobeletStart, Just gobeletDest) -> estPlusGrosQue gobeletStart gobeletDest
                _ -> False
    in concatMap movesPourPosition positionsGobeletsJoueur


-- | Vérifie si une Case est vide.
isCaseVide :: Case -> Bool
isCaseVide c = c == []

-- | Prend un état State de jeu et renvoie une liste de Positions correspondant aux cases vides.
listeCasesVides :: State -> [Position]
listeCasesVides st = [Position posX posY | posX <- [0..3], posY <- [0..3], isCaseVide (board st !! posX !! posY)]

-- | Prend un état et renvoie une liste de gobellet jouable par le joueur courant 
listePiecesJouable :: State -> [Gobelet]
listePiecesJouable st = List.nub  (piecesDisponibles st)

-- | Prend un état et renvoie la liste des positions contenant un gobelet en surface du joueur actuel (qu'il peut onboard)
listePosGobletJoueurIG :: State -> [Position]
listePosGobletJoueurIG st = 
    [Position posX posY | posX <- [0..tailleBoard], posY <- [0..tailleBoard], estGobeletDuJoueurCourant (gobletEnSurface st (Position posX posY))]
  where
    tailleBoard = length (board st)
    estGobeletDuJoueurCourant :: Maybe Gobelet -> Bool
    estGobeletDuJoueurCourant (Just (Gobelet p _)) = p == joueurCourant st
    estGobeletDuJoueurCourant Nothing = False
    


-- | Vérifie si une rangée contient exactement trois gobelets consécutifs de l'adversaire.
verifieTroisAdversesConsecutifs :: State -> [Position] -> Bool
verifieTroisAdversesConsecutifs st positions =
  let adversaire = if joueurCourant st == X then O else X
      gobeletsAdversaires = map (\pos -> case gobletEnSurface st pos of
                                           Just (Gobelet p _) -> p == adversaire
                                           Nothing -> False) positions
  in countConsécutifs gobeletsAdversaires == 3


-- | Compte le nombre de gobelet consécutifs.
countConsécutifs :: [Bool] -> Int
countConsécutifs = length . filter id . maximumBy (comparing length) . groupBy (==)


-- | Récupère toutes les lignes, colonnes et diagonales de la taille du plateau.
obtenirLignesColonnesDiagonales :: State -> [[Position]]
obtenirLignesColonnesDiagonales st =
  let tailleS = length $ board st
      lignes = [[Position i j | j <- [0..tailleS-1]] | i <- [0..tailleS-1]]
      colonnes = [[Position j i | i <- [0..tailleS-1]] | j <- [0..tailleS-1]]
      diag1 = [Position i i | i <- [0..tailleS-1]]
      diag2 = [Position i (tailleS-1-i) | i <- [0..tailleS-1]]
  in lignes ++ colonnes ++ [diag1, diag2]

-- | Applique la vérification d'alignement consécutif de trois gobelets adverses
-- sur toutes les lignes, colonnes et diagonales et renvoie les positions.
listePositionsTroisAdversesConsecutifs :: State -> [[Position]]
listePositionsTroisAdversesConsecutifs st =
  filter (verifieTroisAdversesConsecutifs st) (obtenirLignesColonnesDiagonales st)

-- | Retourne une liste plate des positions où l'adversaire a trois gobelets consécutifs.
listePositionPieceAdv3 :: State -> [Position]
listePositionPieceAdv3 st =
  concat $ listePositionsTroisAdversesConsecutifs st



