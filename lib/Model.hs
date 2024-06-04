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

module Model (
    cost,
    diagramInstances,
    diagramTimes,
    valuesDiagram,
    filledEquation,
    generalEquation,
    instances,
    myText,
    simplify,
    toDiagram
) where

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

import Graphics.SVGFonts
import Numeric(showGFloat)

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


diagramInstances :: Double -> Int -> Int -> Diagram B
diagramInstances size a b
  | size <= 1 = node 1 1
  | otherwise = (root
                ===
                strutY 0.5
                ===
                cat (r2 (1, 0)) (zipWith (\n -> frame 0.2 . named n) [(i, size) | i <- [(1::Int)..]] $ replicate a child) # centerXY)
                    # connect
      where child = diagramInstances (size / fromIntegral b) a b
            root = node size 1 # centerXY # named (0 :: Int, size)
            node l h  = rect l h # lc black # lwG 0.9
            arrowStyle = with & headLength .~ local 0.17 & shaftStyle %~ lwO 0.4
            connect :: Diagram B -> Diagram B
            connect = withName (0 :: Int, size) (\r ->
                                  applyAll (zipWith (\b i ->
                                                      withName (i, size)
                                                      (\c -> atop (arrowBetween' arrowStyle b (topAnchor c))))
                                            (bottomAnchors a r) [1..a]))

topAnchor :: (InSpace V2 n a, Enveloped a) => a -> Point V2 n
topAnchor d = p2 (x, y)
    where x = case extentX d of
                 Just (x1, x2) -> (x1 + x2) / 2
                 Nothing -> 0
          y = case extentY d of
                 Just (_, y2) -> y2
                 Nothing -> 0

bottomAnchors :: (InSpace V2 n a, Enveloped a) => Int -> a -> [Point V2 n]
bottomAnchors n d = [ p2 (x, y) | i <- [1..n], let x = x0 + fromIntegral i * dx, let y = y0 ]
    where x0 = case extentX d of
                 Just (x1, _) -> x1
                 Nothing -> 0
          y0 = case extentY d of
                 Just (y1, _) -> y1
                 Nothing -> 0
          dx = case extentX d of
                 Just (x1, x2) -> (x2 - x1) / fromIntegral (n + 1)
                 Nothing -> 0

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
diagramTimes inst k p -- sum (map fst inst) > drawingLimit = valuesDiagram $ map (second toCost) inst
                      | k == 0 = cat (r2 (0, -1)) $ map (\(n, l) -> rulerw (w l) (n, l)) inst
                      | otherwise = cat (r2 (0, -1)) $ map (\(n, l) -> ruler (h l) (n, l)) inst
    where w l = max (logBase 2 l) 1 ^ p
          h l = l^(k-1) * max (logBase 2 l) 1 ^ p
          toCost l = l ^ k * max (logBase 2 l) 1 ^ p

