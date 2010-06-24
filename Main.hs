-- file: Haskell/Graphic/Main.hs

import Graphics.SOE
import Shape
import Draw

sh1,sh2,sh3,sh4 :: Shape

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5,2.5),(-1.5,2.0),(-1.1,0.2),(-1.7,-1.0),(-3.0,0)]

main0 = runGraphics (
                     do w <- openWindow "Drawing Shapes" (xWin,yWin)
                        drawInWindow w (withColor Red $ shapeToGraphic sh1)
                        drawInWindow w (withColor Blue $ shapeToGraphic sh2)
                        spaceClose w
                    )
        
        
        
---- TEST ----        