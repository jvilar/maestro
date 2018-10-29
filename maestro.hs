{-# LANGUAGE ExistentialQuantification #-}

import Control.Applicative((<*>),(<$>))
import Control.Monad(forM)
import Diagrams.Backend.Cairo(Cairo)
import Diagrams.Backend.Gtk(defaultRender)
import Diagrams.Prelude
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R
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

main = do
         initGUI
         window <- windowNew
         set window [ windowTitle := "Master Theorem"
                    , windowDefaultWidth := 300
                    , windowDefaultHeight := 200
                    ]
         box <- hBoxNew False 2
         containerAdd window box

         let entries = [("n:", iniN), ("a:", iniA), ("b:", iniB), ("k:", iniK), ("p:", iniP)]
             nrows = 2 + length entries
         entryTable <- tableNew nrows 2 False
         boxPackStart box entryTable PackNatural 0
         [ns, as, bs, ks, ps] <- forM (zip entries [0..]) (addEntry entryTable)

         costLabel <- labelNew Nothing
         tableAttach entryTable costLabel 0 2 (length entries) (length entries + 1) [] [] 2 2

         canvas <- drawingAreaNew
         boxPackStart box canvas PackGrow 1
         widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

         network <- R.compile (createNetwork Inputs { aSource = as
                                                    , bSource = bs
                                                    , nSource = ns
                                                    , kSource = ks
                                                    , pSource = ps
                                                    }
                                             Actions { treatInstances = drawInstances canvas
                                                     , writeCost = \a b k p -> labelSetText costLabel (cost a b k p)
                                                     }
                              )
         R.actuate network


         quitButton <- buttonNewWithLabel "Quit"
         tableAttach entryTable quitButton 0 2 (nrows-1) nrows [Expand, Fill] [] 2 2
         onClicked quitButton mainQuit
         onDestroy window mainQuit
         widgetShowAll window

         mainGUI

type EventSource a = (R.AddHandler a, a -> IO ())

addHandler :: EventSource a -> R.AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

data Inputs = Inputs { aSource :: EventSource Int
                     , bSource :: EventSource Int
                     , nSource :: EventSource Int
                     , kSource :: EventSource Int
                     , pSource :: EventSource Int
                     }

data Actions = Actions { treatInstances :: ([(Int, Double)], Int, Int) -> IO ()
                       , writeCost :: Int -> Int -> Int -> Int -> IO ()
                       }

addEntry :: Table -> ((String, Int), Int) -> IO (EventSource Int)
addEntry tbl ((title, ini), row) = do
    label <- labelNew $ Just title
    miscSetAlignment label 0 0.5
    tableAttach tbl label 0 1 row (row+1) [Fill] [] 2 2
    entry <- entryNew
    entrySetText entry $ show ini
    widgetModifyBg entry StateNormal (Color 65535 65535 65535)
    tableAttach tbl entry 1 2 row (row+1) [Expand, Fill] [] 2 2
    source <- R.newAddHandler
    entry `on` keyReleaseEvent $ do
        R.liftIO $ do
                    t <- entryGetText entry
                    case reads t of
                      [(n, "")] -> do
                                     widgetModifyBg entry StateNormal (Color 65535 65535 65535)
                                     fire source n
                      _ -> widgetModifyBg entry StateNormal (Color 65535 0 0)
        return True
    return source

diagramInstances :: [(Int, Double)] -> Diagram Cairo R2
diagramInstances = cat (r2 (0, -1)) . map ruler
                   where ruler (n, l) = cat (r2 (1,0)) $ replicate n (rect l 1 # translate (r2 (l/2, -0.5))) # lc black # lwG 0.2

diagramTimes :: [(Int, Double)] -> Int -> Int -> Diagram Cairo R2
diagramTimes inst k p = cat (r2 (0, -1)) $ map ruler inst
    where ruler (n, l) = if k > 0
                         then let
                                 h = l^(k-1) * (max (logBase 2 l) 1) ^ p
                              in cat (r2 (1,0)) $ replicate n (rect l h # translate (r2 (l/2, -h/2))) # lc black # lwG 0.2
                         else let
                                 w = (max (logBase 2 l) 1) ^ p
                              in cat (r2 (1,0)) $ replicate n (rect w 1 # translate (r2 (w/2, -1/2))) # lc black # lwG 0.2


drawInstances :: DrawingArea -> ([(Int, Double)], Int, Int) -> IO ()
drawInstances canvas (inst, k, p) = do
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


createNetwork :: forall t . R.Frameworks t => Inputs -> Actions -> R.Moment t ()
createNetwork inputs actions = do
    ea <- R.fromAddHandler (addHandler $ aSource inputs)
    eb <- R.fromAddHandler (addHandler $ bSource inputs)
    en <- R.fromAddHandler (addHandler $ nSource inputs)
    ek <- R.fromAddHandler (addHandler $ kSource inputs)
    ep <- R.fromAddHandler (addHandler $ pSource inputs)
    let
        ba = R.stepper iniA ea
        bb = R.stepper iniB eb
        bn = R.stepper iniN en
        bk = R.stepper iniK ek
        bp = R.stepper iniP ep
        bpar = (\n a b k p -> (n, a, b, k, p)) <$> bn <*> ba <*> bb <*> bk <*> bp
        binst = (\n a b k p -> (instances (fromIntegral n) a b, k, p)) <$> bn <*> ba <*> bb <*> bk <*> bp
    eParChanged <- R.changes bpar
--    R.reactimate $ (\(n, a, b, k, p) -> treatInstances actions (instances (fromIntegral n) a b, k, p)) <$> eParChanged
--    R.reactimate $ (\(_, a, b, k, p) -> writeCost actions a b k p) <$> eParChanged
    R.reactimate' $ ((\(_, a, b, k, p) -> writeCost actions a b k p) <$> ((,,,,) <$> en <*> ea <*> eb <*> ek <*> ep))
