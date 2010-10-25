module Type where

-- type PenDrawingMode a = State Int a 

--data Status = StandBy | Drawing 
--data PenStatus = Pen | Erase

data EventType = ButtonPress | ButtonRelease | MouseMove 
               deriving (Eq,Show) 
type Coord = (Double,Double)                

type EventResult = (EventType,Coord) 
