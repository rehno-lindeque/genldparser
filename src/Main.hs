-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Rehno Lindeque, 2011
-- License     :  BSD3
--
-- Maintainer  :  Rehno Lindeque
-- Stability   :  Unstable
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.Maybe
import Data.Graph.Inductive
import qualified Data.Map
import qualified Data.Bimap as Bimap
import qualified Data.GraphViz as GraphViz

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

type SymbolId    = Int
data Symbol      = Terminal SymbolId | Nonterminal SymbolId deriving (Ord, Eq)
type Token       = String

type RuleRHS     = [Symbol]
data Rule        = Rule
  { ruleId     :: Int,
    ruleNumber :: Int,
    ruleRHS    :: RuleRHS
  }
data Production  = Production SymbolId [Rule]
type Grammar     = [ Production ]

-- Get the symbol's id
symbolId :: Symbol -> SymbolId
symbolId (Terminal id) = id
symbolId (Nonterminal id) = id

-- Define the test language's alphabet
testAlphabet :: Bimap.Bimap Token Symbol
testAlphabet =
    Bimap.fromList $ nonterminals ["S", "A", "B", "C", "D"] ++ terminals ["x", "y", "a", "b"]
  where
    nonterminals t = zip t (map Nonterminal [0..])
    terminals t    = zip t (map Terminal [0..])

lookupSymbolId token = symbolId $ fromJust $ Bimap.lookup token testAlphabet

-- Parse a grammar specification

--parseGrammar :: String -> Grammar
--parseRule =

--parseProduction :: String -> Production
--parseProduction =

--parseRule :: String -> Rule
--parseRule =

-- Define the test grammar
testGrammar :: Grammar
testGrammar = [
    production "S" [
        (0,[Terminal 0]),
        (1,[Terminal 0])
      ]
  ]
  where
    generateRules prodId = map (\(number,rhs) -> Rule prodId number rhs)
    production token rules = Production productionId $ generateRules productionId rules
      where
        productionId = lookupSymbolId token


graph :: Gr String String
graph = ([],2,"b",[("shift", 1)]) & (([],1,"a",[]) & empty)

main = do
    GraphViz.runGraphviz dotGraph GraphViz.Png "ouput.png"
    putStrLn "Done: See output.png"
  where
    params   = GraphViz.defaultParams :: GraphViz.GraphvizParams String String String String
    dotGraph = GraphViz.graphToDot params graph
