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

module GUI (makeGUI) where

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

import Model

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

