-- file:Graphic/window.hs

import Graphics.SOE

myPutStr :: String -> IO ()
myPutStr (x:xs) = do putChar x 
                     myPutStr xs
myPutStr _ = return ()

main0 = runGraphics (
                     do w <- openWindow
                             "My First Graphics Program" (300, 300)
                        drawInWindow w (text (100, 200) "Hello GraphicsWorld")
                        -- k <- getKey w
                        spaceClose w
                        closeWindow w
                    )

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w else spaceClose w
                  
                  
pic1 = withColor Red                  
       (ellipse (150,150) (300,300))
pic2 = withColor Blue 
       (polyline [(100,50),(200,50),(200,250),(100,250),(100,50)])
pic3 = withColor White       
       (ellipse (0,0) (150,150))
pic4 = withColor Green
       (polyline [(0,150),(150,150),(150,300),(0,300),(0,150)])

main2 = runGraphics (       
                     do w <- openWindow
                             "Some Graphics Figures" (300,300)
                        drawInWindow w pic1
                        drawInWindow w pic2
                        drawInWindow w pic3
                        drawInWindow w pic4
                        spaceClose w
                    )
        
        
fillTri :: Window -> Int -> Int -> Int -> IO ()         
fillTri w x y size = 
    drawInWindow w (withColor Blue
                    (polygon [(x,y),(x+size,y),(x,y-size)]))
    
minSize :: Int    
minSize = 8

sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size 
              = if size <= minSize
                then fillTri w x y size
                else let size2 = size `div` 2
                     in do sierpinskiTri w x y size2
                           sierpinskiTri w x (y-size2) size2
                           sierpinskiTri w (x+size2) y size2
                           
main3 = runGraphics (                           
                     do w <- openWindow "Sierpinski's Triangle" (400,400)
                        sierpinskiTri w 50 300 256
                        spaceClose w
                    )
        
        

type XY = (Int, Int)

starPoint :: XY -> Int -> ([XY], [XY])
starPoint (x,y) size = let a = (0,size)
                           b = (size,-size `div` 2)
                           c = (-size,-size `div` 2)
                           f (x', y') = (x'+x, y'+y)
                           g (x', y') = (-1*x', -1*y')
                           q = f . g
                       in ([f a,f b,f c], [q a
                                          ,q b,q c]) 


fillStar :: Window -> Color -> XY -> Int -> IO ()
fillStar w color origin size = let (xs, ys) = starPoint origin size
                               in do drawInWindow w (withColor color
                                                     (polygon xs))
                                     drawInWindow w (withColor color
                                                     (polygon ys))
    

nextOrigins :: XY -> Int -> [XY]
nextOrigins origin offset = let (xs, ys) = starPoint origin offset
                            in xs ++ ys

snowFractral :: Window -> XY -> Int -> IO ()
snowFractral w origin size 
    | size <= 3 = return ()
    | otherwise =  
        do fillStar w color origin size
           mapM_ (\x -> snowFractral w x size') origins
    where origins = nextOrigins origin size
          size' = size `div` 3

          color | size `mod` 3 == 0 = Red
                | size `mod` 5 == 0 = Green
                | otherwise        = Blue

main4 = runGraphics (                           
                     do w <- openWindow "Sierpinski's Triangle" (400,400)
                        snowFractral w (200, 200) 150
                        spaceClose w
                    )
       