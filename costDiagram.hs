import Model

import Diagrams.Prelude
import Diagrams.Backend.Cairo(B)
import Diagrams.Backend.Cairo.CmdLine

costs :: Int -> Int -> Int -> Int -> Int -> Diagram B
costs size a b k p = diagramTimes (instances size a b) k p # centerXY # pad 1.1

main = mainWith costs

