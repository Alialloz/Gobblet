{-|
Module      : AlgoMiniMax
Description : Implémentation de l'algorithme MiniMax pour le jeu Gobblet.
Maintainer  : exemple@email.com
Copyright   : (c) NAYERI POOR Ali , 2023

Ce module fournit une implémentation de l'algorithme MiniMax, utilisé pour déterminer le meilleur
mouvement possible pour l'IA dans le jeu Gobblet. L'algorithme prend en compte la profondeur de
recherche et l'état actuel du jeu pour évaluer les mouvements potentiels.
-}
module AlgoMiniMax (choisirMouvementIAMiniMax) where

import State
import Move
import Rules
import Score
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S

-- | Valeur représentant l'infini pour l'algorithme.
infinity :: Int
infinity = 99999999

-- | Évalue l'état actuel du jeu et renvoie le score.
-- Utilise la fonction 'score' du module 'Score' comme heuristique.
evaluer :: State -> Int
evaluer etat = score etat

-- | Algorithme MiniMax qui choisit le meilleur mouvement.
-- Il alterne entre maximiser et minimiser le score, en fonction du joueur courant.
-- Renvoie un mouvement potentiel et l'évaluation associée.
minimax :: State -> Int -> Bool -> Int -> Int -> (Maybe Move, Int)
minimax etat profondeur estMax joueurMin joueurMax
    | profondeur == 0 || estFinDeJeu etat = (Nothing, evaluer etat)
    | estMax =
        let moves = S.toList $ availableMoves etat
            evals = [ (move, if isJust newState then snd (minimax (fromJust newState) (profondeur-1) False joueurMin joueurMax) else -infinity)
                    | move <- moves
                    , let newState = applyMove etat move
                    , isJust newState ]
            (bestMove, bestVal) = maximumBy (comparing snd) evals
        in (Just bestMove, bestVal)
    | otherwise = 
        let moves = S.toList $ availableMoves etat
            evals = [ (move, if isJust newState then snd (minimax (fromJust newState) (profondeur-1) True joueurMin joueurMax) else infinity)
                    | move <- moves
                    , let newState = applyMove etat move
                    , isJust newState ]
            (bestMove, bestVal) = minimumBy (comparing snd) evals
        in (Just bestMove, bestVal)

-- | Détermine si le jeu est terminé, soit quand un gagnant est trouvé.
estFinDeJeu :: State -> Bool
estFinDeJeu etat = isJust (verifieGagnant etat)

-- | Sélectionne et retourne le meilleur mouvement pour l'IA en utilisant l'algorithme MiniMax.
-- Cette fonction lance l'algorithme MiniMax avec la profondeur spécifiée et extrait le meilleur mouvement.
-- Renvoie une erreur si aucun mouvement valide n'est trouvé.
choisirMouvementIAMiniMax :: State -> Int -> IO Move
choisirMouvementIAMiniMax etat profondeur = do
    let (bestMove, _) = minimax etat profondeur True (-infinity) infinity
    return $ fromJust bestMove
