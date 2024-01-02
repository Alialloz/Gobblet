{-|
Module      : Main
Description : Ce module est le point d'entrée du jeu Gobblet, il gère les interactions utilisateur et initialise le jeu.
Copyright   : (c) NAYERI POOR Ali , 2023

Ce module gère le flux principal du jeu, y compris la sélection du mode de jeu, la boucle de jeu pour les différents modes,
et l'interaction avec l'IA à différents niveaux de difficulté.
-}
module Main where

import AlgoMiniMax (choisirMouvementIAMiniMax)
import State
import Move
import Score
import Rules
import System.IO (hSetEncoding, stdout, utf8)
import System.Random (randomRIO)
import qualified Data.Set as S


-- | Point d'entrée principal du programme.
-- Permet à l'utilisateur de choisir le mode de jeu et initialise le jeu.
main :: IO ()
main = do
    hSetEncoding stdout utf8  -- Configure l'encodage pour supporter les caractères spéciaux.
    putStrLn "Choisissez le mode de jeu :"
    putStrLn "1. Joueur contre Joueur"
    putStrLn "2. Joueur contre IA"
    mode <- getLine  -- Lecture du mode de jeu choisi par l'utilisateur.
    let etatInitial = initialiserEtatJeu  -- Crée l'état initial du jeu.
    case mode of
        "1" -> boucleDeJeuJoueurContreJoueur etatInitial
        "2" -> selectionDifficulteIA etatInitial
        _   -> putStrLn "Option non valide" >> main  -- Répète si l'option n'est pas valide.

-- | Permet à l'utilisateur de choisir la difficulté de l'IA et lance le jeu.
selectionDifficulteIA :: State -> IO ()
selectionDifficulteIA etat = do
    putStrLn "Choisissez le niveau de difficulté de l'IA :"
    putStrLn "1. Facile (Aléatoire)"
    putStrLn "2. Modéré (Algorithme MiniMax, profondeur 3)"
    putStrLn "3. Difficile (Algorithme MiniMax, profondeur 4)"
    difficulte <- getLine
    case difficulte of
        "1" -> boucleDeJeuJoueurContreIA etat choisirMouvementIAAleatoire
        "2" -> boucleDeJeuJoueurContreIA etat (choisirMouvementIAMiniMaxProfondeur 3)
        "3" -> boucleDeJeuJoueurContreIA etat (choisirMouvementIAMiniMaxProfondeur 4)
        _   -> putStrLn "Option non valide" >> selectionDifficulteIA etat

-- | Fonction adaptée pour utiliser MiniMax à une profondeur spécifique.
choisirMouvementIAMiniMaxProfondeur :: Int -> State -> IO Move
choisirMouvementIAMiniMaxProfondeur profondeur etat =
    choisirMouvementIAMiniMax etat profondeur

-- | Boucle de jeu pour le mode Joueur contre Joueur.
boucleDeJeuJoueurContreJoueur :: State -> IO ()
boucleDeJeuJoueurContreJoueur etat = do
    afficherPlateau (board etat)
    afficherScore etat
    afficherPileJoueurCourant etat
    case verifieGagnant etat of
            Just winner -> putStrLn $ "Félicitations joueur " ++ show winner ++ ", tu as gagné !"
            Nothing ->  do
                let currentPlayer = joueurCourant etat
                putStrLn $ "Hey joueur " ++ show currentPlayer ++ ", c'est à toi de jouer !"
                moveStr <- demanderMouvement 
                case parseMove moveStr of
                    Just move -> do
                        maybeNewState <- effectuerMouvement etat move
                        case maybeNewState of
                            Just newState -> boucleDeJeuJoueurContreJoueur newState
                            Nothing -> boucleDeJeuJoueurContreJoueur etat
                    Nothing -> do
                        putStrLn "Mouvement non valide, réessayez."
                        boucleDeJeuJoueurContreJoueur etat

-- | Boucle de jeu pour le mode Joueur contre IA.
boucleDeJeuJoueurContreIA :: State -> (State -> IO Move) -> IO ()
boucleDeJeuJoueurContreIA etat choisirMouvementIA = do
    afficherPlateau (board etat)
    afficherScore etat
    afficherPileJoueurCourant etat
    case verifieGagnant etat of
            Just winner -> putStrLn $ "Félicitations joueur " ++ show winner ++ ", tu as gagné !"
            Nothing -> do
                let currentPlayer = joueurCourant etat
                putStrLn $ "Hey joueur " ++ show currentPlayer ++ ", c'est à toi de jouer !"
                if estHumain currentPlayer
                    then do
                        moveStr <- demanderMouvement 
                        case parseMove moveStr of
                            Just move -> do
                                maybeNewState <- effectuerMouvement etat move
                                case maybeNewState of
                                    Just newState -> boucleDeJeuJoueurContreIA newState choisirMouvementIA
                                    Nothing -> putStrLn "Mouvement non valide, réessayez." >> boucleDeJeuJoueurContreIA etat choisirMouvementIA
                            Nothing -> putStrLn "Mouvement non valide, réessayez." >> boucleDeJeuJoueurContreIA etat choisirMouvementIA
                    else do
                        mouvement <- choisirMouvementIA etat
                        maybeNewState <- effectuerMouvement etat mouvement
                        case maybeNewState of
                            Just newState -> boucleDeJeuJoueurContreIA newState choisirMouvementIA
                            Nothing -> putStrLn "L'IA a fait une erreur." >> boucleDeJeuJoueurContreIA etat choisirMouvementIA


-- | Demande à l'utilisateur de saisir son mouvement.
demanderMouvement :: IO String
demanderMouvement = do
    putStrLn "Entrez votre mouvement (par exemple 'drop(S, (0, 0))' ou 'onboard((0, 0), (1, 0))') :"
    getLine  -- Retourne la ligne saisie par l'utilisateur.

-- | Applique le mouvement choisi par le joueur ou par l'IA à l'état actuel du jeu.
effectuerMouvement :: State -> Move -> IO (Maybe State)
effectuerMouvement etat move = do
    let maybeNewState = applyMove etat move
    case maybeNewState of
        Just newState -> do
            putStrLn "Mouvement appliqué."
            putStrLn " "
            return (Just newState)
        Nothing -> do
            putStrLn "Mouvement non valide."
            putStrLn " "
            return Nothing

-- | Sélectionne un mouvement aléatoire parmi ceux disponibles.
-- Utilisé pour l'IA de niveau "Facile".
choisirMouvementIAAleatoire :: State -> IO Move
choisirMouvementIAAleatoire etat = do
    let moves = S.toList (availableMoves etat)
    idx <- randomRIO (0, length moves - 1)
    return (moves !! idx)

-- | Détermine si le joueur actuel est un humain.
-- Supposons que l'humain joue toujours avec les pièces 'X'.
estHumain :: Piece -> Bool
estHumain X = True
estHumain _ = False
