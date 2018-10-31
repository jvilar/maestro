{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative((<*>),(<$>))
import Control.Monad(forM_, forM)
import Control.Monad.Reader(liftIO)
import Diagrams.Backend.Cairo(B, Cairo)
import Diagrams.Backend.Gtk(defaultRender)
import Diagrams.Prelude hiding (set)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Text.Printf(printf)

instances :: Double -> Int -> Int -> [(Int, Double)]
instances size a b = takeWhile ((>=1/fromIntegral b).snd) $ iterate (\(n, s) -> (n*a, s/fromIntegral b)) (1, size)

pow :: String -> Int -> String
pow s 0 = ""
pow s 1 = s ++ " "
pow s n = printf "%s^%d " s n

cost :: Int -> Int -> Int -> Int -> String
cost a b k 0 | a < b ^ k  = if nk == ""
                            then "1"
                            else nk
             | a == b ^ k = nk ++ "log(n)"
             | otherwise  = printf "n^(logBase %d(%d))" b a
             where nk = pow "n" k
cost a b k p | a < b ^ k  = printf "%slog^%d(n)" nk p
             | a == b ^ k = printf "%slog^%d(n)" nk (p+1)
             | otherwise  = printf "n^(logBase %d(%d))" b a
             where nk = pow "n" k

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


drawInstances :: DrawingArea -> [(Int, Double)] -> Int -> Int -> IO ()
drawInstances canvas inst k p = do
    clearCanvas canvas
    defaultRender canvas (diagramInstances inst
                          ===
                          strutY 2
                          ===
                          diagramTimes inst k p)

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

  labelSetText (costLabel elements) $ cost a b k p
  drawInstances (canvas elements) (instances n a b) k p

