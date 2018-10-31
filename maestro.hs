{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative((<*>),(<$>))
import Control.Monad(forM_, forM)
import Control.Monad.Reader(liftIO)
import Diagrams.Backend.Cairo(B)
import Diagrams.Backend.Gtk(defaultRender)
import Diagrams.Prelude hiding (set)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.SVGFonts
import Text.Printf(printf)

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

up :: (ExpressionF a -> a) -> Expression -> a
up f = f . fmap (up f) . unfix

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

textOptions :: Double -> IO (TextOpts Double)
textOptions h = do
  f <- lin
  return $ TextOpts f INSIDE_H HADV False h 1

myText :: String -> Diagram B
myText = stroke . flip textSVG 1

toDiagram :: Expression -> Diagram B
toDiagram = up go
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
        reduce = scale 0.8
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
simplify = up go
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



pow :: String -> Int -> String
pow s 0 = ""
pow s 1 = s ++ " "
pow s n = printf "%s^%d " s n

iniN = 20
iniA = 2
iniB = 2
iniK = 1
iniP = 0

data GUIElements = GUIElements { ne :: Entry
                               , ae :: Entry
                               , be :: Entry
                               , ke :: Entry
                               , pe :: Entry
                               , costLabel :: Label
                               , canvas :: DrawingArea
                               }


main = do
         initGUI
         window <- windowNew
         set window [ windowTitle := "Master Theorem"
                    , windowDefaultWidth := 300
                    , windowDefaultHeight := 200
                    ]
         box <- hBoxNew False 2
         containerAdd window box

         let entryData = [("n:", iniN), ("a:", iniA), ("b:", iniB), ("k:", iniK), ("p:", iniP)]
             nrows = 2 + length entryData
         entryTable <- tableNew nrows 2 False
         boxPackStart box entryTable PackNatural 0
         entries <- forM (zip entryData [0..]) (addEntry entryTable)

         cLabel <- labelNew (Nothing :: Maybe String)
         tableAttach entryTable cLabel 0 2 (length entries) (length entries + 1) [] [] 2 2

         cvs <- drawingAreaNew
         boxPackStart box cvs PackGrow 1
         widgetModifyBg cvs StateNormal (Color 65535 65535 65535)

         let elements = GUIElements { ne = entries !! 0
                                    , ae = entries !! 1
                                    , be = entries !! 2
                                    , ke = entries !! 3
                                    , pe = entries !! 4
                                    , costLabel = cLabel
                                    , canvas = cvs
                                    }

         forM_ entries $ setEntryKeyEvent elements

         quitButton <- buttonNewWithLabel "Quit"
         tableAttach entryTable quitButton 0 2 (nrows-1) nrows [Expand, Fill] [] 2 2
         onClicked quitButton mainQuit
         onDestroy window mainQuit
         widgetShowAll window

         mainGUI

addEntry :: Table -> ((String, Int), Int) -> IO Entry
addEntry tbl ((title, ini), row) = do
    label <- labelNew $ Just title
    miscSetAlignment label 0 0.5
    tableAttach tbl label 0 1 row (row+1) [Fill] [] 2 2
    entry <- entryNew
    entrySetText entry $ show ini
    widgetModifyBg entry StateNormal (Color 65535 65535 65535)
    tableAttach tbl entry 1 2 row (row+1) [Expand, Fill] [] 2 2
    return entry

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
diagramInstances = cat (r2 (0, -1)) . map ruler
                   where ruler (n, l) = cat (r2 (1,0)) $ replicate n (rect l 1 # translate (r2 (l/2, -0.5))) # lc black # lwG 0.2

diagramTimes :: [(Int, Double)] -> Int -> Int -> Diagram B
diagramTimes inst k p = cat (r2 (0, -1)) $ map ruler inst
    where ruler (n, l) = if k > 0
                         then let
                                 h = l^(k-1) * (max (logBase 2 l) 1) ^ p
                              in cat (r2 (1,0)) $ replicate n (rect l h # translate (r2 (l/2, -h/2))) # lc black # lwG 0.2
                         else let
                                 w = (max (logBase 2 l) 1) ^ p
                              in cat (r2 (1,0)) $ replicate n (rect w 1 # translate (r2 (w/2, -1/2))) # lc black # lwG 0.2


drawInCanvas :: DrawingArea -> [Diagram B] -> IO ()
drawInCanvas canvas elements = do
  clearCanvas canvas
  defaultRender canvas $ vsep 2 elements

-- |Clears a 'DrawinArea' by drawing a white rectangle.
clearCanvas :: DrawingArea -> IO()
clearCanvas canvas = do
    dr <- widgetGetDrawWindow canvas
    (w, h) <- drawableGetSize dr
    gc <- gcNewWithValues dr $ newGCValues { foreground = Color 65535 65535 65535 }
    drawRectangle dr gc True 0 0 w h


updateGUI :: GUIElements -> IO ()
updateGUI elements = do
  a <- read <$> entryGetText ( ae elements )
  b <- read <$> entryGetText ( be elements )
  n <- read <$> entryGetText ( ne elements )
  k <- read <$> entryGetText ( ke elements )
  p <- read <$> entryGetText ( pe elements )

  let inst = instances n a b
  drawInCanvas (canvas elements)
    [ toDiagram $ simplify $ cost a b k p
    , diagramInstances inst
    , diagramTimes inst k p
    ]


