{-|
Module      : Move
Description : Définition des mouvements dans le jeu Gobblet et fonctions d'analyse syntaxique.
Copyright   : (c) NAYERI POOR Ali , 2023

Ce module décrit les différents types de mouvements possibles dans le jeu Gobblet.
Il fournit également des fonctions pour analyser des chaînes de caractères et les transformer
en mouvements utilisables dans le jeu.
-}
module Move (
  -- * Types de mouvement
  Move(..), 

  -- * Fonctions d'analyse
  parseMove,   -- ^ Analyse une chaîne de caractères et la convertit en mouvement.
  parseMoves   -- ^ Analyse une liste de chaînes de caractères et les convertit en liste de mouvements.
) where

import State 
import qualified Data.List as List
import Data.Char (isAlpha, isSpace, isDigit)
import Data.Maybe (mapMaybe)

-- | La structure de données `Move` représente un mouvement dans le jeu.
-- Il peut s'agir soit d'un dépôt d'une pièce sur le plateau (`Drop`),
-- soit d'un déplacement d'une pièce déjà sur le plateau (`OnBoard`).
data Move = Drop Taille Position  -- ^ Représente le dépôt d'une pièce de taille donnée sur une position du plateau.
          | OnBoard Position Position  -- ^ Représente le déplacement d'une pièce d'une position à une autre sur le plateau.
          deriving (Show, Eq, Ord)


-- | Analyse une liste de chaînes de caractères pour créer une liste de mouvements.
-- Ignore les chaînes qui ne représentent pas un mouvement valide.
parseMoves :: [String] -> [Move]
parseMoves = mapMaybe parseMove

-- Fonctions privées utilisées pour l'analyse des mouvements.

-- | Tente de parser une taille à partir d'une chaîne de caractères.
parseTaille :: String -> Maybe Taille
parseTaille "T" = Just T
parseTaille "S" = Just S
parseTaille "M" = Just M
parseTaille "B" = Just B
parseTaille _   = Nothing

-- | Tente de parser une position à partir d'une chaîne de caractères.
-- La position est exprimée sous forme d'un tuple `(Int, Int)`.
parsePosition :: String -> Maybe Position
parsePosition str =
  case reads str :: [(Int, String)] of
    [(posX, rest)] -> case reads (tail rest) :: [(Int, String)] of
                     [(posY, "")] -> Just (Position posX posY)
                     _         -> Nothing
    _           -> Nothing

-- | Analyse une chaîne de caractères pour tenter de créer un mouvement.
-- La chaîne doit commencer par 'drop(' ou 'onboard(' et se terminer par ')'.
--
-- >>> parseMove "drop(S,(1,2))"
-- Just (Drop S (Position 1 2))
-- >>> parseMove "onboard((0,0),(1,1))"
-- Just (OnBoard (Position 0 0) (Position 1 1))
-- >>> parseMove "invalid"
-- Nothing
--
parseMove :: String -> Maybe Move
parseMove str
  | "drop(" `List.isPrefixOf` str && last str == ')' = parseDrop $ init $ drop 5 str
  | "onboard(" `List.isPrefixOf` str && last str == ')' = parseOnBoard $ init $ drop 8 str
  | otherwise = Nothing
  where
    -- Parse un mouvement de type `Drop` à partir d'une sous-chaîne.
    parseDrop s =
      let (tailleStr, rest) = break (== ',') s
          tailleS = parseTaille $ filter isAlpha tailleStr
          positionStr = dropWhile (not . isDigit) rest
          position = parsePosition $ takeWhile (/= ')') positionStr
      in Drop <$> tailleS <*> position

    -- Parse un mouvement de type `OnBoard` à partir d'une sous-chaîne.
    parseOnBoard s =
      let (pos1Str, rest) = span (/= ')') s
          pos1 = if not (null pos1Str) then parsePosition (filter (not . isSpace) $ tail pos1Str) else Nothing
          pos2 = if not (null rest) then parsePosition (filter (not . isSpace) $ tail $ init $ dropWhile (/= '(') rest) else Nothing
      in OnBoard <$> pos1 <*> pos2
