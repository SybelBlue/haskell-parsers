{-# LANGUAGE TupleSections #-}

module MoleculeToAtoms where

import Control.Applicative ( some )

import Data.Bifunctor
import Data.List ( nub )
import Data.Map.Strict ( (!), fromListWith )

import Text.Parsec

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = do
    atomCounts <- first (const "Not a valid molecule") $ parse molecule "" formula
    let atoms = nub (fst <$> atomCounts)
    let atomMap = fromListWith (+) atomCounts
    return $ (\a -> (a, atomMap ! a)) <$> atoms

atom = (:) <$> oneOf ['A'..'Z'] <*> many (oneOf ['a'..'z'])

bunch = do
    base <- (pure . (,1) <$> atom) <|> group
    mult <- (read <$> some digit) <|> return 1
    return (second (* mult) <$> base)

group = concat <$> choice (betweenPair <$> ["()", "[]", "{}"])
    where betweenPair [a, b] = char a *> many bunch <* char b

molecule = concat <$> some bunch
