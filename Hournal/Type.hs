module Hournal.Type where

type Title = String 
type Stroke = [(Double,Double)]
data Dimension = Dim { dim_width :: Double, dim_height :: Double }
               deriving Show

data Background = Background { bkg_string :: String }
                deriving Show 

data Xournal = Xournal { xoj_title :: Title, xoj_pages :: [Page] }
             deriving Show 
data Page = Page { page_dim :: Dimension
                 , page_bkg :: Background 
                 , page_layers :: [Layer] }
          deriving Show 
data Layer = Layer { layer_strokes :: [Stroke] } 
           deriving Show 
