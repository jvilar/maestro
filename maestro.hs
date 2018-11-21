{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-
Copyright Juan Miguel Vilar Torres (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Juan Miguel Vilar Torres nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import Control.Applicative((<*>),(<$>))
import Control.Arrow(second)
import Control.Monad(forM_, forM)
import Control.Monad.Reader(liftIO)
import Data.Maybe(isNothing)
import Diagrams.Core.Compile(renderDia)
import Diagrams.Backend.Cairo(B, Cairo(Cairo), OutputType(RenderOnly))
import Diagrams.Backend.Cairo.Internal (Options(CairoOptions))
import Diagrams.Prelude hiding (after, set)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.SVGFonts
import Numeric(showGFloat)
import Text.Printf(printf)

import Paths_maestro(getDataFileName)

instances :: Int -> Int -> Int -> [(Int, Double)]
instances size a b = takeWhile ((>=1).snd) $ iterate (\(n, s) -> (n*a, s/fromIntegral b)) (1, fromIntegral size)

newtype Fix f = Fix { unfix :: f (Fix f) }

data ExpressionF b = ValInt Int
                   | ValDouble Double
                   | Var String
                   | Log b
                   | LogBase b b
                   | Exp b b
                   | ExpLog b b
                   | Prod b b deriving Functor

type Expression = Fix ExpressionF

fix2 :: (Expression -> Expression -> ExpressionF Expression)
     -> Expression -> Expression -> Expression
fix2 f a b = Fix $ f a b

cata :: (ExpressionF a -> a) -> Expression -> a
cata f = f . fmap (cata f) . unfix

mkValInt :: Int -> Expression
mkValInt = Fix . ValInt

mkValDouble :: Double -> Expression
mkValDouble = Fix . ValDouble

mkVar :: String -> Expression
mkVar = Fix . Var

mkLog :: Expression -> Expression
mkLog = Fix . Log

mkLogBase :: Expression -> Expression -> Expression
mkLogBase = fix2 LogBase

mkExp :: Expression -> Expression -> Expression
mkExp = fix2 Exp

mkExpLog :: Expression -> Expression -> Expression
mkExpLog = fix2 ExpLog

mkProd :: Expression -> Expression -> Expression
mkProd = fix2 Prod


myText :: String -> Diagram B
myText = lwO 0.1 . fc black . strokeP . flip textSVG 1

toDiagram :: Expression -> Diagram B
toDiagram = cata go
  where go :: ExpressionF (Diagram B) -> Diagram B
        go (ValInt n) = myText $ show n
        go (ValDouble d) = myText $ show d
        go (Var s) = myText s
        go (Log d) = myText "log" ||| paren d
        go (LogBase d1 d2) = myText "log" ||| subindex d1 ||| paren d2
        go (Exp d1 d2) = d1 ||| exponent d2
        go (ExpLog d1 d2) = myText "log" ||| exponent d2 ||| paren d1
        go (Prod d1 d2) = d1 ||| sep ||| d2

        exponent d = d # translateY 0.5 # reduce
        subindex d = d # translateY (-0.4) # reduce
        reduce = scale 0.6
        sep = strut 0.2
        paren e = myText "(" ||| e ||| myText ")"

myLog :: Int -> Int -> Maybe Int
myLog b v = case compare b v of
              GT -> Nothing
              EQ -> Just 1
              LT -> if v `mod` b == 0
                    then (1+) <$> myLog b (v `div` b)
                    else Nothing

simplify :: Expression -> Expression
simplify = cata go
  where go :: ExpressionF Expression -> Expression
        go (Exp e (Fix (ValInt 0))) = mkValInt 1
        go (Exp e (Fix (ValInt 1))) = e
        go (Exp (Fix (ValInt 1)) _) = mkValInt 1
        go (Exp (Fix (Log b)) e) = mkExpLog b e
        go (Prod (Fix (ValInt 1)) e) = e
        go (Prod e (Fix (ValInt 1))) = e
        go e@(LogBase (Fix (ValInt b)) (Fix (ValInt v))) = maybe (Fix e) mkValInt $ myLog b v
        go e = Fix e


cost :: Int -> Int -> Int -> Int -> Expression
cost a b k p | a < b ^ k = mkProd nk $ mkExp logn (mkValInt p)
             | a == b ^ k = mkProd nk $ mkExp logn (mkValInt (p+1))
             | otherwise = mkExp (mkVar "n") $ mkLogBase (mkValInt b) (mkValInt a)
             where nk = mkExp (mkVar "n") (mkValInt k)
                   logn = mkLog $ mkVar "n"

iniN = 32
iniA = 2
iniB = 2
iniK = 1
iniP = 0

data GUIElements = GUIElements { nEntry :: Entry
                               , aEntry :: Entry
                               , bEntry :: Entry
                               , kEntry :: Entry
                               , pEntry :: Entry
                               , costDrawingArea :: DrawingArea
                               , instanceDrawingArea :: DrawingArea
                               , mainWindow :: Window
                               , quitButton :: Button
                               }


makeGUI :: IO GUIElements
makeGUI = do
    initGUI

    builder <- builderNew
    gladefn <- getDataFileName "maestro.glade"
    builderAddFromFile builder gladefn

    elements <- recoverElements builder

    let entries = map ($ elements) [ nEntry, aEntry, bEntry, kEntry, pEntry ]
    mapM_ (setEntryKeyEvent elements) entries
    forM_ (zip entries [iniN, iniA, iniB, iniK, iniP]) $ \(entry, ini) ->
        entrySetText entry $ show ini

    mainWindow elements `on` deleteEvent $ liftIO mainQuit >> return False
    quitButton elements `on` buttonActivated $ mainQuit

    widgetShowAll $ mainWindow elements
    mainWindow elements `after` sizeAllocate $ const $ updateGUI elements

    return elements

class CanBeCast a where
    doCast :: GObject -> a

instance CanBeCast Button where
    doCast = castToButton

instance CanBeCast Entry where
    doCast = castToEntry

instance CanBeCast DrawingArea where
    doCast = castToDrawingArea

instance CanBeCast Image where
    doCast = castToImage

instance CanBeCast Window where
    doCast = castToWindow


recoverElements :: Builder -> IO GUIElements
recoverElements builder = do
    let getObject :: (CanBeCast obj, GObjectClass obj) => String -> IO obj
        getObject = builderGetObject builder doCast

    ne <- getObject "nEntry"
    ae <- getObject "aEntry"
    be <- getObject "bEntry"
    ke <- getObject "kEntry"
    pe <- getObject "pEntry"

    cIm <- getObject "costDrawingArea"
    iIm <- getObject "instanceDrawingArea"

    qb <- getObject "quitButton"

    mw <- getObject "mainWindow"

    return GUIElements { nEntry = ne
                       , aEntry = ae
                       , bEntry = be
                       , kEntry = ke
                       , pEntry = pe
                       , costDrawingArea = cIm
                       , instanceDrawingArea = iIm
                       , mainWindow = mw
                       , quitButton = qb
                       }

main = do
         elements <- makeGUI
         mainGUI

setEntryKeyEvent :: GUIElements -> Entry -> IO ()
setEntryKeyEvent elements entry = do
    widgetSetState entry StateNormal
    entry `on` keyReleaseEvent $ liftIO $ updateGUI elements >> return True
    return ()

drawingLimit :: Int
drawingLimit = 500

diagramInstances :: [(Int, Double)] -> Diagram B
diagramInstances inst | sum (map fst inst) <= drawingLimit  = cat (r2 (0, -1)) $ map (ruler 1) inst
                      | otherwise = valuesDiagram inst

valuesDiagram :: [(Int, Double)] -> Diagram B
valuesDiagram values = cat (r2 (0, -1)) $ map (frame 0.2 . scale 0.8) $ map pairDiagram values ++ [ totalDiagram values ]

showFloat :: Double -> String
showFloat = flip (showGFloat (Just 2)) " "

totalDiagram :: [(Int, Double)] -> Diagram B
totalDiagram l = withCenter ":" (myText "Total") (myText . showFloat . sum $ map (\(n, d) -> fromIntegral n * d) l)

pairDiagram :: (Int, Double) -> Diagram B
pairDiagram (n, d) = withCenter "=" left right
  where left = hsep 0.2 (map myText [ show n, "x", showFloat d])
        right = myText $ showFloat $ d * fromIntegral n

withCenter :: String -> Diagram B -> Diagram B -> Diagram B
withCenter center left right = beside (r2 (1, 0)) (beside (r2 (-1, 0)) (myText center) (left ||| strutX 0.2)) (strutX 0.2 ||| right)

ruler :: Double -> (Int, Double) -> Diagram B
ruler h (n, l) = cat (r2 (1, 0)) $ replicate n (rect l h # translate (r2 (l/2, -h/2))) # lc black # lwO 0.5

rulerw :: Double -> (Int, Double) -> Diagram B
rulerw w (n, l) = cat (r2 (1, 0)) $ replicate n (rect w 1 # translate (r2 (w/2, -1/2))) # lc black # lwO 0.5

diagramTimes :: [(Int, Double)] -> Int -> Int -> Diagram B
diagramTimes inst k p | sum (map fst inst) > drawingLimit = valuesDiagram $ map (second toCost) inst
                      | k == 0 = cat (r2 (0, -1)) $ map (\(n, l) -> rulerw (w l) (n, l)) inst
                      | otherwise = cat (r2 (0, -1)) $ map (\(n, l) -> ruler (h l) (n, l)) inst
    where w l = (max (logBase 2 l) 1) ^ p
          h l = l^(k-1) * (max (logBase 2 l) 1) ^ p
          toCost l = l ^ k * (max (logBase 2 l) 1) ^ p

drawDiagram :: DrawingArea -> Diagram B -> IO ()
drawDiagram area diagram = do
    drawWindow <- widgetGetParentWindow area
    w <- drawWindowGetWidth drawWindow
    h <- drawWindowGetHeight drawWindow
    let opts = CairoOptions "aaa.png" sizeSpec RenderOnly False
        sizeSpec = mkSizeSpec2D (Just $ fromIntegral w) (Just $ fromIntegral h)
        (_, renderDiagram) = renderDia Cairo opts $ frame 0.2 diagram
        rect = Rectangle 0 0 w h
        render = do
           setSourceColor $ Color 65535 65535 65535
           rectangle rect
           Cairo.fill
           renderDiagram
    area `on` draw $ render
    drawWindowInvalidateRect drawWindow rect True
    return ()

readFromEntry :: Int -> Entry -> IO (Maybe Int)
readFromEntry min entry = do
    t <- entryGetText entry
    let result = case reads t :: [(Int, String)] of
                      [(n, "")] -> if n >= min
                                   then Just n
                                   else Nothing
                      _ -> Nothing
    widgetModifyFg entry StateNormal $ if isNothing result
                                       then (Color 65535 0 0)
                                       else (Color 0 0 0)
    return result



updateGUI :: GUIElements -> IO ()
updateGUI elements = do
    l <- mapM (\(e, m) -> readFromEntry m $ e elements)
              [ (aEntry, 1), (bEntry, 2), (nEntry, 1), (kEntry, 0), (pEntry, 0) ]

    let (costDiagram, instanceDiagrams) =
             case sequence l of
                Nothing -> (myText "Error", strutY 2)
                Just [a, b, n, k, p] ->
                    let inst = instances n a b
                        costDiagram = hsep 0.2 [ myText "O(", toDiagram $ simplify $ cost a b k p, myText ")"]
                        instanceDiagrams = diagramInstances inst
                                           ===
                                           strutY 2
                                           ===
                                           diagramTimes inst k p
                    in (costDiagram, instanceDiagrams)
    drawDiagram (costDrawingArea elements) costDiagram
    drawDiagram (instanceDrawingArea elements) instanceDiagrams
