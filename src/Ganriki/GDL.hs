module Ganriki.GDL (toGDL) where

import Data.List (intercalate)
import Data.Graph.Inductive (Gr, labNodes, labEdges)

toGDL :: (Show a, Show b) => Gr a b -> String
toGDL g = "graph: {\n\n" ++ nodes ++ "\n\n" ++ edges ++ "\n\n}\n"
    where
        nodes = intercalate "\n" $ map node (labNodes g)
        edges = intercalate "\n" $ map edge (labEdges g)

        node (n, label)    = "node: { title: \"" ++ (show n) ++ "\" label:\"" ++ (escape $ show label) ++ "\" }"
        edge (s, t, label) = "edge: { source: \"" ++ (show s) ++  "\"  target: \"" ++ (show t) ++ "\" }"

        escape = concatMap $ \x -> case x of
                                       '"'  -> "\\\""
                                       '\n' -> "\\n"
                                       x    -> [x]

