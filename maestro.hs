{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Maybe(isNothing, fromJust)
import Data.String(IsString(..))
import Diagrams.Core.Compile(renderDia)
import Diagrams.Backend.Cairo(B)
import Diagrams.Backend.GIGtk
import Diagrams.Prelude hiding (after, Box, set, Sum)
import Data.Text(Text)
import qualified Data.Text as T
import GI.Gdk(Rectangle (Rectangle), newZeroRGBA, rGBAParse, windowInvalidateRect, getRectangleX, clearDeviceSeat)
import GI.Gdk.Structs.Rectangle(getRectangleWidth, getRectangleHeight)
import qualified GI.Gtk as Gtk
import GI.Gtk hiding (main)
import GI.Gtk.Flags(StateFlags(..))
import Data.GI.Base.GObject(GObjectClass)
import Graphics.SVGFonts
import Numeric(showGFloat)

import Paths_maestro(getDataFileName)
import GI.Gtk.Objects.Builder (builderNewFromFile)
import GI.Gtk.Objects (onWidgetDestroy)

instances :: Int -> Int -> Int -> [(Int, Double)]
instances size a b = takeWhile ((>=1).snd) $ iterate (\(n, s) -> (n*a, s/fromIntegral b)) (1, fromIntegral size)

newtype Fix f = Fix { unfix :: f (Fix f) }

data ExpressionF b = ValInt Int
                   | ValDouble Double
                   | Var String
                   | Log b
                   | LogBase b b
                   | Pow b b
                   | PowLog b b
                   | Sum b b
                   | Diff b b
                   | Prod b b
                   | Div b b
                   | FuncApp b b
                     deriving Functor

type Expression = Fix ExpressionF

instance Num Expression where
    (+) = mkSum
    (-) = mkDiff
    (*) = mkProd
    abs = mkFuncApp (mkVar "abs")
    signum = mkFuncApp (mkVar "signum")
    fromInteger = mkValInt . fromInteger

instance Fractional Expression where
    fromRational = mkValDouble . fromRational
    (/) = mkDiv

instance Floating Expression where
    (**) = mkPow
    logBase = mkLogBase
    log = mkLog

instance IsString Expression where
    fromString = mkVar

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

mkPow :: Expression -> Expression -> Expression
mkPow = fix2 Pow

mkPowLog :: Expression -> Expression -> Expression
mkPowLog = fix2 PowLog

mkSum :: Expression -> Expression -> Expression
mkSum = fix2 Sum

mkDiff :: Expression -> Expression -> Expression
mkDiff = fix2 Diff

mkProd :: Expression -> Expression -> Expression
mkProd = fix2 Prod

mkDiv :: Expression -> Expression -> Expression
mkDiv = fix2 Div

mkFuncApp :: Expression -> Expression -> Expression
mkFuncApp = fix2 FuncApp


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
        go (Pow d1 d2) = d1 ||| exponent d2
        go (PowLog d1 d2) = myText "log" ||| exponent d2 ||| paren d1
        go (Sum d1 d2) = d1 ||| myText "+" ||| d2
        go (Diff d1 d2) = d1 ||| myText "-" ||| d2
        go (Prod d1 d2) = d1 ||| sep ||| d2
        go (Div d1 d2) = d1 ||| myText "/" ||| d2
        go (FuncApp d1 d2) = d1 ||| paren d2

        exponent d = d # translateY 0.5 # reduce
        subindex d = d # translateY (-0.4) # reduce
        reduce = scale 0.6
        sep = strut 0.1
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
        go (Pow e (Fix (ValInt 0))) = mkValInt 1
        go (Pow e (Fix (ValInt 1))) = e
        go (Pow (Fix (ValInt 1)) _) = mkValInt 1
        go (Pow (Fix (Log b)) e) = mkPowLog b e
        go (Prod (Fix (ValInt 1)) e) = e
        go (Prod e (Fix (ValInt 1))) = e
        go (Div e (Fix (ValInt 1))) = e
        go e@(LogBase (Fix (ValInt b)) (Fix (ValInt v))) = maybe (Fix e) mkValInt $ myLog b v
        go e = Fix e


cost :: Int -> Int -> Int -> Int -> Expression
cost a b k p | a < b ^ k = "n" ** k' * log "n" ** p'
             | a == b ^ k = "n" ** k' * log "n" ** fromIntegral(p + 1)
             | otherwise = "n" ** logBase b' a'
             where a' = fromIntegral a
                   b' = fromIntegral b
                   k' = fromIntegral k
                   p' = fromIntegral p

generalEquation :: Int -> Expression
generalEquation 0 = "a" * mkFuncApp "T" ("n"/"b") + mkFuncApp "O" ("n" ** "k")
generalEquation _ = "a" * mkFuncApp "T" ("n"/"b") + mkFuncApp "O" ("n" ** "k" * mkLog "n" ** "p")

filledEquation :: Int -> Int -> Int -> Int -> Expression
filledEquation a b k p = simplify $ fromIntegral a * mkFuncApp "T" ("n"/fromIntegral b) + mkFuncApp "O" ("n" ** fromIntegral k * mkLog "n" ** fromIntegral p)

iniN :: Int
iniN = 16

iniA :: Int
iniA = 2

iniB :: Int
iniB = 2

iniK :: Int
iniK = 1

iniP :: Int
iniP = 0

data GUIElements = GUIElements { nEntry :: Entry
                               , aEntry :: Entry
                               , bEntry :: Entry
                               , kEntry :: Entry
                               , pEntry :: Entry
                               , costDrawingArea :: DrawingArea
                               , instanceDrawingArea :: DrawingArea
                               , generalEquationDrawingArea :: DrawingArea
                               , filledEquationDrawingArea :: DrawingArea
                               , mainWindow :: Window
                               , quitButton :: Button
                               }


tshow :: Show a => a -> Text
tshow = T.pack . show


makeGUI :: IO GUIElements
makeGUI = do
    Gtk.init Nothing

    gladefn <- getDataFileName "maestro.glade"
    builder <- builderNewFromFile $ T.pack gladefn

    elements <- recoverElements builder

    let entries = map ($ elements) [ nEntry, aEntry, bEntry, kEntry, pEntry ]
    mapM_ (setEntryKeyEvent elements) entries
    forM_ (zip entries [iniN, iniA, iniB, iniK, iniP]) $ \(entry, ini) ->
        entrySetText entry $ tshow ini

    onWidgetDestroy (mainWindow elements) mainQuit
    onButtonClicked (quitButton elements) mainQuit

    forM_ [(costDrawingArea, costDiagram), (instanceDrawingArea, instanceDiagram),
          (generalEquationDrawingArea, generalDiagram), (filledEquationDrawingArea, filledDiagram)]
          $ \(drawingArea, diagram) ->
             on (drawingArea elements) #draw $ drawDiagram (drawingArea elements) (diagram elements)

    widgetShowAll $ mainWindow elements
    return elements

class GObject a => CanBeCast a where
    doCast :: GObject o => o -> IO a

instance CanBeCast Button where
    doCast = unsafeCastTo Button

instance CanBeCast Entry where
    doCast = unsafeCastTo Entry

instance CanBeCast DrawingArea where
    doCast = unsafeCastTo DrawingArea

instance CanBeCast Image where
    doCast = unsafeCastTo Image

instance CanBeCast Box where
    doCast = unsafeCastTo Box

instance CanBeCast Window where
    doCast = unsafeCastTo Window


recoverElements :: Builder -> IO GUIElements
recoverElements builder = do
    let getObject :: CanBeCast obj => Text -> IO obj
        getObject name = builderGetObject builder name >>= doCast . fromJust

    ne <- getObject "nEntry"
    ae <- getObject "aEntry"
    be <- getObject "bEntry"
    ke <- getObject "kEntry"
    pe <- getObject "pEntry"

    cDa <- getObject "costDrawingArea"
    iDa <- getObject "instanceDrawingArea"
    geDa <- getObject "generalEquationDrawingArea"
    feDa <- getObject "filledEquationDrawingArea"

    qb <- getObject "quitButton"

    mw <- getObject "mainWindow"

    return GUIElements { nEntry = ne
                       , aEntry = ae
                       , bEntry = be
                       , kEntry = ke
                       , pEntry = pe
                       , costDrawingArea = cDa
                       , instanceDrawingArea = iDa
                       , generalEquationDrawingArea = geDa
                       , filledEquationDrawingArea = feDa
                       , mainWindow = mw
                       , quitButton = qb
                       }

main = do
         elements <- makeGUI
         Gtk.main

setEntryKeyEvent :: GUIElements -> Entry -> IO ()
setEntryKeyEvent elements entry = do
    widgetSetStateFlags entry [StateFlagsNormal] True
    onWidgetKeyReleaseEvent entry $ const $ do
        mapM_ (widgetQueueDraw . ($ elements))
            [ costDrawingArea
            , instanceDrawingArea
            , generalEquationDrawingArea
            , filledEquationDrawingArea ]
        return True
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
    where w l = max (logBase 2 l) 1 ^ p
          h l = l^(k-1) * max (logBase 2 l) 1 ^ p
          toCost l = l ^ k * max (logBase 2 l) 1 ^ p

drawDiagram :: DrawingArea -> IO (Diagram B) -> WidgetDrawCallback
drawDiagram drawingArea diagram context = do
    wDr <- fromIntegral <$> widgetGetAllocatedWidth drawingArea
    hDr <- fromIntegral <$> widgetGetAllocatedHeight drawingArea
    dia <- diagram
    let
      wdia = width dia
      hdia = height dia
      ratio = fromIntegral wDr / fromIntegral hDr
      hEnvelope = max hdia (wdia / ratio)
      wEnvelope = max (hEnvelope * ratio) wdia
      enveloped = dia # center # withEnvelope (rect wEnvelope hEnvelope :: D V2 Double)
      spec = mkSizeSpec2D (Just $ fromIntegral wDr) (Just $ fromIntegral hDr)
      scaledDia = toGtkCoords $ transform (requiredScaling spec (V2 wEnvelope hEnvelope)) enveloped
    renderToGtk context True scaledDia
    return True


readFromEntry :: Int -> Entry -> IO (Maybe Int)
readFromEntry min entry = do
    t <- T.unpack <$> entryGetText entry
    let result = case reads t :: [(Int, String)] of
                      [(n, "")] -> if n >= min
                                   then Just n
                                   else Nothing
                      _ -> Nothing
    rgba <- newZeroRGBA
    rGBAParse rgba $ if isNothing result
                     then "red"
                     else "black"
    widgetOverrideColor entry [StateFlagsNormal] (Just rgba)
    return result


readEntries :: GUIElements -> [(GUIElements -> Entry, Int)] -> IO (Maybe [Int])
readEntries elements request = do
    l <- mapM (\(e, m) -> readFromEntry m $ e elements) request
    return $ sequence l

costDiagram :: GUIElements -> IO (Diagram B)
costDiagram elements = do
    readEntries elements [(aEntry, 1), (bEntry, 2), (kEntry, 0), (pEntry, 0)] <&> (\case
        Nothing -> myText "Error"
        Just [a, b, k, p] -> hsep 0.2 [ myText "O(", toDiagram $ simplify $ cost a b k p, myText ")"]
        _ -> error "Impossible")

instanceDiagram :: GUIElements -> IO (Diagram B)
instanceDiagram elements = do
    readEntries elements [ (aEntry, 1), (bEntry, 2), (nEntry, 1), (kEntry, 0), (pEntry, 0) ]
      <&> (\case
        Nothing -> myText "Error"
        Just [a, b, n, k, p] -> let inst = instances n a b
                                in diagramInstances inst
                                   ===
                                   strutY 2
                                   ===
                                   diagramTimes inst k p
        _ -> error "Impossible")

filledDiagram :: GUIElements -> IO (Diagram B)
filledDiagram elements = do
    readEntries elements [(aEntry, 1), (bEntry, 2), (kEntry, 0), (pEntry, 0)] <&> (\case
        Nothing -> myText "Error"
        Just [a, b, k, p] -> hsep 0.2 [ myText "T(n) =", toDiagram $ filledEquation a b k p]
        _ -> error "Impossible")

generalDiagram :: GUIElements -> IO (Diagram B)
generalDiagram elements = do
    readEntries elements [(pEntry, 0)] <&> (\case
        Nothing -> myText "Error"
        Just [p] -> hsep 0.2 [ myText "T(n) =", toDiagram $ generalEquation p]
        _ -> error "Impossible")

