-- file Haskell/Graphic/Region.hs

module Region ( Region( Shape
                      , Translate
                      , Scale
                      , Complement
                      , Union
                      , Intersect
                      , Empty )
              , Coordinate
              , containsS
              , containsR
              , module Shape 
              ) where

import Shape

data Region = Shape Shape 
            | Translate Vector Region 
            | Scale Vector Region
            | Complement Region
            | Region `Union` Region
            | Region `Intersect` Region
            | Empty
              deriving (Show)

type Vector = (Float,Float)

infixr 5 `Union`
infixr 6 `Intersect` 


              