-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import CmmProcPoint (callProcPoints)
import Data.Maybe

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     zoom    <- mkInput 5 "0.04"                -- Zoom 
     draw    <- mkButton "Draw graph"         -- The draw button
     diffButt <- mkButton "Differentiate" --The differentiate Button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     zoomInput <- row [pure zoom]
     getBody window #+ [column [pure canvas,pure formula,pure draw,pure zoomInput,pure diffButt]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]
     pure zoom # set style [("fontSize","12pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas zoom
     on valueChange' zoom  $ \ _ -> readAndDraw input canvas zoom
     on valueChange' input $ \ _ -> readAndDraw input canvas zoom
     on UI.click     diffButt $ \ _ -> onDiff input zoom canvas

onDiff :: Element -> Element -> Canvas -> UI ()
onDiff input zoom canvas = do
                        formula <- get value input 
                        let f = fromJust (readExpr formula)
                        let t = showExpr (differentiate f)                      
                        input # set' value t
                        readAndDraw input canvas zoom


readAndDraw :: Element -> Canvas -> Element -> UI ()
readAndDraw input canvas zoom =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     z <- get value zoom
     let f = fromJust (readExpr formula)
     let scale = read z
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points f scale (canWidth, canHeight)) canvas






-- points :: Expr -> Double -> (Int,Int) -> [Point]
-- points exp scale (width, height)  
--                          | widthNeg > fromIntegral width = [(widthNeg, expEval)]
--                          | otherwise = (widthNeg, expEval) : points exp scale (width, height)
--                              where
--                              widthNeg = (-1) * fromIntegral width
--                              expEval = eval exp (widthNeg + scale)

-- converts a pixel x-coordinate to a real x-coordinate 
pixToReal :: Double -> Double -> Double 
pixToReal scale x = x * scale
-- converts a real y-coordinate to a pixel y-coordinate 
realToPix :: Double -> Double -> Double 
realToPix scale x = -x / scale

inrange :: Double -> Point -> Bool
inrange height x = snd x <= height && snd x > (-height)

points :: Expr -> Double -> (Int, Int) -> [Point]
points exp scale (width, height) = filter (inrange (fromIntegral height)) [(fromIntegral (x + width `div` 2), realToPix scale (eval exp (pixToReal scale (fromIntegral x))) + fromIntegral width / 2) | x <- [-width `div` 2..width `div` 2]]
 