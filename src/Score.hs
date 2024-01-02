{-|
Module      : Score
Description : Le module 'Score' fournit des fonctionnalités pour le calcul et l'affichage des scores dans le jeu Gobblet.
Copyright   : (c) NAYERI POOR Ali , 2023

Ce module permet de calculer le score des joueurs à un état donné du jeu et d'afficher les scores actuels.
Les scores sont déterminés par la différence entre le score du joueur courant et celui de l'adversaire,
avec des points attribués pour les alignements de gobelets sur le plateau.
-}
module Score (
  score,            -- ^ Calcule le score actuel pour l'état donné.
  afficherScore     -- ^ Affiche le score actuel pour l'état donné.
) where

import State (State(..), Gobelet(..), Piece(..))
import Data.List (groupBy)

-- | Calcule le score pour l'état actuel du jeu.
-- Le score est calculé comme la différence entre les scores des deux joueurs, basé sur les alignements actuels de gobelets sur le plateau.
-- 
-- >>> let etat = initialiserEtatJeu
-- >>> score etat
-- 0  -- Au début du jeu, les scores sont égaux.
--
score :: State -> Int 
score etat = 
  let scoreX = scoreJoueur etat X
      scoreO = scoreJoueur etat O
      scoreCourant = if joueurCourant etat == X then scoreX else scoreO
      scoreAdverse = if joueurCourant etat == X then scoreO else scoreX
  in scoreCourant - scoreAdverse

-- | Affiche le score pour l'état donné sur le terminal.
-- 
-- >>> let etatInitial = initialiserEtatJeu
-- afficherScore etatInitial
-- "Score : 0"
--
afficherScore :: State -> IO ()
afficherScore etat = do
    let scoreX = score etat 
    putStrLn $ "Score : " ++ show scoreX

-- | Calcul du score pour un joueur spécifié dans un état donné.
-- Additionne les scores obtenus sur les lignes, colonnes et diagonales.
scoreJoueur :: State -> Piece -> Int
scoreJoueur etat p =
  let scoresLignes = sum [verifLigne etat p ligne | ligne <- [0..3]]
      scoresColonnes = sum [verifColonne etat p colonne | colonne <- [0..3]]
      scoresDiagonales = verifDiagonale etat p 1 + verifDiagonale etat p 2
  in scoresLignes + scoresColonnes + scoresDiagonales

-- | Vérification de la présence d'un gobelet d'un joueur dans une case donnée.
verifCase :: State -> Piece -> Int -> Int -> Bool
verifCase etat p ligne colonne =
  let caseAVerifier = board etat !! ligne !! colonne
  in any (\gobelet -> piece gobelet ==  p) caseAVerifier

-- | Calcul du nombre maximum de valeurs 'True' consécutives dans une liste.
-- Utilisé pour déterminer le score d'alignement.
maxConsécutifs :: [Bool] -> Int
maxConsécutifs = maximum . (0 :) . map length . filter (head) . groupBy (==) . (False :)

-- | Évaluation d'une ligne et calcul du score basé sur les gobelets alignés du joueur.
verifLigne :: State -> Piece -> Int -> Int 
verifLigne etat p ligne = 
  scoreAlignement $ map (\col -> verifCase etat p ligne col) [0..3]

-- | Évaluation d'une colonne et calcul du score basé sur les gobelets alignés du joueur.
verifColonne :: State -> Piece -> Int -> Int 
verifColonne etat p colonne = 
  scoreAlignement $ map (\row -> verifCase etat p row colonne) [0..3]

-- | Évaluation d'une diagonale et calcul du score basé sur les gobelets alignés du joueur.
verifDiagonale :: State -> Piece -> Int -> Int
verifDiagonale etat p numDiagonale = 
  scoreAlignement $ map (\(row, col) -> verifCase etat p row col) indices
  where
    indices = if numDiagonale == 1 then zip [0..3] [0..3] else zip [0..3] (reverse [0..3])

-- | Attribution d'un score en fonction du nombre de gobelets alignés consécutivement.
-- Un alignement de trois gobelets vaut 10 points, deux gobelets vaut 1 point.
scoreAlignement :: [Bool] -> Int
scoreAlignement cases = 
  let consécutifs = maxConsécutifs cases
  in case consécutifs of
       3 -> 10  -- Trois gobelets alignés consécutivement
       2 -> 1   -- Deux gobelets alignés consécutivement
       _ -> 0   -- Moins de deux gobelets alignés ne rapportent pas de points
