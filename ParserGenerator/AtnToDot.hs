module ParserGenerator.AtnToDot where

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.PatriciaTree as PT
import Data.GraphViz as GV
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Complete
import ParserGenerator.AllStar
import Data.Text.Lazy
import System.FilePath


getStates :: (Ord nt) => ATNEnv nt t -> [ATNState nt]
getStates atnEnv = S.toList (S.foldr (\(p,lbl,q) acc -> S.insert p (S.insert q acc))
                                     S.empty
                                     atnEnv)

getEdges :: ATNEnv nt t -> [ATNEdge nt t]
getEdges atnEnv = S.toList atnEnv

mkNodeMap :: (Ord nt) => [ATNState nt] -> Map (ATNState nt) Int
mkNodeMap states = M.fromAscList (L.zip states [1..L.length states])

mkGrNodes :: (Ord nt) => Map (ATNState nt) Int -> [ATNState nt] -> [LNode (ATNState nt)]
mkGrNodes nodeMap states = L.map (\st -> (M.findWithDefault 0 st nodeMap, st)) states

mkGrEdges :: (Ord nt) => Map (ATNState nt) Int -> [ATNEdge nt t] -> [LEdge (ATNEdgeLabel nt t)]
mkGrEdges nm atnEdges = L.map (\(p,lbl,q) -> (M.findWithDefault 0 p nm,
                                              M.findWithDefault 0 q nm,
                                              lbl))
                        atnEdges

fromATN :: (Ord nt) => (ATNEnv nt t) -> PT.Gr (ATNState nt) (ATNEdgeLabel nt t)
fromATN atnEnv = let states  = getStates atnEnv
                     edges   = getEdges atnEnv
                     nodeMap = mkNodeMap states
                     grNodes = mkGrNodes nodeMap states
                     grEdges = mkGrEdges nodeMap edges
                     g       = mkGraph grNodes grEdges
                 in  g

instance (Show nt) => Labellable (ATNState nt) where
  toLabelValue state = StrLabel (pack (show state))

instance (Show nt, Show t) => Labellable (ATNEdgeLabel nt t) where
  toLabelValue el = StrLabel (pack (show el))

toDotGraph :: (Show nt, Show t) => PT.Gr (ATNState nt) (ATNEdgeLabel nt t) -> DotGraph Node
toDotGraph g = let params = Params { isDirected       = True
                                   , globalAttributes = []
                                   , clusterBy        = N
                                   , isDotCluster     = const True
                                   , clusterID        = const (Num $ Int 0)
                                   , fmtCluster       = const []
                                   , fmtNode          = (\(n,l) -> [Label (toLabelValue l)])
                                   , fmtEdge          = (\(_,_,el) -> [Label (toLabelValue el)])
                                   } :: (Labellable nl, Labellable el) => GraphvizParams n nl el () nl
               in  graphToDot params g


-- Took these from an online example
createImage :: PrintDotRepr dg n => FilePath -> dg n -> IO FilePath
createImage n g = createImageInDir "." n Png g

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = GV.addExtension (runGraphvizCommand Dot g) o (combine d n)
-- </end>

atnToImage :: (Ord nt, Show nt, Show t) => ATNEnv nt t -> FilePath -> IO FilePath
atnToImage atn fname = ((createImage fname) . toDotGraph . fromATN) atn


atn = S.fromList [ -- First path through the 'S' ATN
                   (Init 'S', GS EPS, Middle 'S' 0 0),
                   (Middle 'S' 0 0, GS (NT 'A'), Middle 'S' 0 1),
                   (Middle 'S' 0 1, GS (T 'c'), Middle 'S' 0 2),
                   (Middle 'S' 0 2, GS EPS, Final 'S'),
                   
                   -- Second path through the 'S' ATN
                   (Init 'S', GS EPS, Middle 'S' 1 0),
                   (Middle 'S' 1 0, GS (NT 'A'), Middle 'S' 1 1),
                   (Middle 'S' 1 1, GS (T 'd'), Middle 'S' 1 2),
                   (Middle 'S' 1 2, GS EPS, Final 'S'),
                   
                   -- First path through the 'A' ATN
                   (Init 'A', GS EPS, Middle 'A' 0 0),
                   (Middle 'A' 0 0, GS (T 'a'), Middle 'A' 0 1),
                   (Middle 'A' 0 1, GS (NT 'A'), Middle 'A' 0 2),
                   (Middle 'A' 0 2, GS EPS, Final 'A'),
                   
                   -- Second path through the 'A' ATN
                   (Init 'A', GS EPS, Middle 'A' 1 0),
                   (Middle 'A' 1 0, GS (T 'b'), Middle 'A' 1 1),
                   (Middle 'A' 1 1, GS EPS, Final 'A')]

g = fromATN atn
dg = toDotGraph g

