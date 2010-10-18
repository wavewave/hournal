matchMarker :: String -> String -> (Bool,String)  
matchMarker [] xs = (True, xs) 
matchMarker (c:cs) (x:xs) = if c == x 
                            then matchMarker cs xs 
                            else (False, (x:xs) ) 

dropUntilMarker :: String -> String -> (String,String) 
dropUntilMarker [] xs = ([],xs)
dropUntilMarker cs [] = ([],[])
dropUntilMarker mkr@(c:cs) str = let str' = dropWhile (/= c) str 
                                     (ismatched,str'') = matchMarker mkr str'  
                                 in  if ismatched 
                                     then (mkr,str'') 
                                     else dropUntilMarker mkr (tail str'')


takeUntilMarker :: String -> String -> (String,String)
takeUntilMarker [] xs = ([],xs)
takeUntilMarker cs [] = ([],[])
takeUntilMarker mkr@(c:cs) str = let (strpass,strremain) = break (==c) str 
                                     (ismatched,strremain2) = matchMarker mkr strremain 
                                 in  if ismatched
                                     then (strpass,strremain2)
                                     else takeUntilMarker mkr (tail strremain2)
                        




--readheader :: String -> (String, String) 
--readheader str = let str2 = dropWhile (/= '<') str
--                 in if (str2 !! 1) == '?' 
--                    then takeWhile (/= '?') str2 
                            
readheadertest str = let (_,str') = dropUntilMarker "<?" str
                     in  takeUntilMarker "?>" str' 
                     
readtag str = let (_,str') = dropUntilMarker "<" str  
              in  takeUntilMarker ">" str' 

readxojtest :: String -> IO (String,String) 
readxojtest filename = do 
  handle <- openFile filename ReadMode
  str <- hGetContents handle 
  let (str1,str2) = readheadertest str
      (str3,str4) = readtag str2
      
  return $ (str1,str3)

readxoj :: String -> IO ((Double,Double),[Stroke])
readxoj filename = do 
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  let strs = lines str
      pagesizestr = words (strs !! 0) 
  
      width = read $ pagesizestr !! 0  
      height = read $ pagesizestr !! 1
  
      strokesstr = tail strs
      
      strokes = map (parseOneStroke.words) strokesstr 
  
  
  return ((width,height),strokes)

parseOneStroke :: [String] -> Stroke
parseOneStroke [] = [] 
parseOneStroke (x:y:xs) = (read x, read y) : parseOneStroke xs 

