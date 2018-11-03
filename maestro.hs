{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative((<*>),(<$>))
import Control.Monad(forM_, forM)
import Control.Monad.Reader(liftIO)
import Diagrams.Backend.Cairo(B, renderCairo)
-- import Diagrams.Backend.Gtk(defaultRender)
import Diagrams.Prelude hiding (set)
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Gdk.GC
import Graphics.SVGFonts
import Text.Printf(printf)

import Paths_maestro(getDataFileName)

instances :: Double -> Int -> Int -> [(Int, Double)]
instances size a b = takeWhile ((>=1/fromIntegral b).snd) $ iterate (\(n, s) -> (n*a, s/fromIntegral b)) (1, size)

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
myText = fc black . strokeP . flip textSVG 1

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

iniN = 20
iniA = 2
iniB = 2
iniK = 1
iniP = 0

data GUIElements = GUIElements { nEntry :: Entry
                               , aEntry :: Entry
                               , bEntry :: Entry
                               , kEntry :: Entry
                               , pEntry :: Entry
                               , costImage :: Image
                               , instancesImage :: Image
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

    quitButton elements `on` buttonActivated $ mainQuit
    mainWindow elements `on` deleteEvent $ liftIO mainQuit >> return False
    mainWindow elements `on` configureEvent $ liftIO (updateGUI elements) >> return False

    widgetShowAll $ mainWindow elements
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

    cIm <- getObject "costImage"
    iIm <- getObject "instancesImage"

    qb <- getObject "quitButton"

    mw <- getObject "mainWindow"

    return GUIElements { nEntry = ne
                       , aEntry = ae
                       , bEntry = be
                       , kEntry = ke
                       , pEntry = pe
                       , costImage = cIm
                       , instancesImage = iIm
                       , mainWindow = mw
                       , quitButton = qb
                       }

main = do
         elements <- makeGUI
         mainGUI

setEntryKeyEvent :: GUIElements -> Entry -> IO ()
setEntryKeyEvent elements entry = do
    entry `on` keyReleaseEvent $ liftIO $ do
                    t <- entryGetText entry
                    case reads t :: [(Int, String)] of
                      [(n, "")] -> do
                                     widgetModifyBg entry StateNormal (Color 65535 65535 65535)
                                     updateGUI elements
                      _ -> widgetModifyBg entry StateNormal (Color 65535 0 0)
                    return True
    return ()

diagramInstances :: [(Int, Double)] -> Diagram B
diagramInstances = cat (r2 (0, -1)) . map (ruler 1)

ruler :: Double -> (Int, Double) -> Diagram B
ruler h (n, l) = cat (r2 (1, 0)) $ replicate n (rect l h # translate (r2 (l/2, -h/2))) # lc black # lwO 0.2

rulerw :: Double -> (Int, Double) -> Diagram B
rulerw w (n, l) = cat (r2 (1, 0)) $ replicate n (rect w 1 # translate (r2 (w/2, -1/2))) # lc black # lwO 0.2

diagramTimes :: [(Int, Double)] -> Int -> Int -> Diagram B
diagramTimes inst 0 p = cat (r2 (0, -1)) $ map (\(n, l) -> rulerw (w l) (n, l)) inst
    where w l = (max (logBase 2 l) 1) ^ p
diagramTimes inst k p = cat (r2 (0, -1)) $ map (\(n, l) -> ruler (h l) (n, l)) inst
    where h l = l^(k-1) * (max (logBase 2 l) 1) ^ p


drawDiagram :: Image -> Diagram B -> IO ()
drawDiagram image diagram = do
    w <- (Just . fromIntegral) <$> widgetGetAllocatedWidth image
    h <- (Just . fromIntegral) <$> widgetGetAllocatedHeight image
    renderCairo "drawing_.png" (mkSizeSpec2D w h) $ frame 0.2 diagram
    imageSetFromFile image "drawing_.png"


updateGUI :: GUIElements -> IO ()
updateGUI elements = do
    a <- read <$> entryGetText ( aEntry elements )
    b <- read <$> entryGetText ( bEntry elements )
    n <- read <$> entryGetText ( nEntry elements )
    k <- read <$> entryGetText ( kEntry elements )
    p <- read <$> entryGetText ( pEntry elements )

    let inst = instances n a b
        costDiagram = toDiagram $ simplify $ cost a b k p
        instanceDiagrams = diagramInstances inst
                           ===
                           strutY 2
                           ===
                           diagramTimes inst k p
    drawDiagram (costImage elements) costDiagram
    drawDiagram (instancesImage elements) instanceDiagrams
    return ()


