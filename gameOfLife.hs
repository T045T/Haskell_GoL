import System.IO

data Field = Field { width :: Int
                   , height :: Int
                   , matrix :: [[Bool]]
                   }

instance Show Field where
    show (Field _ _ matrix) =
        printMatrix matrix
            where
                printMatrix :: [[Bool]] -> String
                printMatrix [] = ""
                printMatrix (line:remainder) = printLine line ++ (printMatrix remainder)
                printLine :: [Bool] -> String
                printLine [] = "\n"
                printLine (field:remainder) = (if field == True
                                          then "@"
                                          else ".") ++ (printLine remainder)        

step :: Field -> Field
step field = Field (width field) (height field) (updateMatrix (matrix field) (matrix field) (0, 0))
    where
        updateMatrix :: [[Bool]] -> [[Bool]] -> (Int, Int) -> [[Bool]]
        updateMatrix _ [] _ = []
        updateMatrix oldMatrix (line:currentMatrix) coords = (updateLine oldMatrix line coords) : (updateMatrix oldMatrix currentMatrix ((fst coords), (snd coords) + 1))
        
        updateLine :: [[Bool]] -> [Bool] -> (Int, Int) -> [Bool]
        updateLine _ [] _ = []
        updateLine oldMatrix (item:restLine) coords = (willLive coords oldMatrix) : updateLine oldMatrix restLine ((fst coords) + 1, (snd coords))

addDot :: Field -> (Int, Int) -> Field
addDot field coords = if (fst coords) < (height field) && (snd coords) < (width field)
                        then Field (height field) (width field) (setMatrixValue True coords (matrix field))
                        else field

getValue :: (Int, Int) -> [[Bool]] -> Bool
getValue coords matrix = (matrix !! (snd coords)) !! (fst coords)

getAbove :: (Int, Int) -> [[Bool]] -> Bool
getAbove coords matrix = if (snd coords) > 0
                           then getValue ((fst coords), (snd coords) - 1) matrix
                           else False

getBelow coords matrix = if (snd coords) < (length matrix) - 1
                           then getValue ((fst coords), (snd coords) + 1) matrix
                           else False
                           
getLeft coords matrix = if (fst coords) > 0
                           then getValue ((fst coords) - 1, (snd coords)) matrix
                           else False
                           
getRight coords matrix = if (fst coords) < (length (matrix !! 0)) - 1
                           then getValue ((fst coords) + 1, (snd coords)) matrix
                           else False
                           
getDiagUL coords matrix = if (snd coords) > 0 && (fst coords) > 0
                           then getValue ((fst coords) - 1, (snd coords) - 1) matrix
                           else False
                           
getDiagUR coords matrix = if (snd coords) > 0 && (fst coords) < (length (matrix !! 0)) - 1
                           then getValue ((fst coords) + 1, (snd coords) - 1) matrix
                           else False
                           
getDiagLL coords matrix = if (snd coords) < (length matrix) - 1 && (fst coords) > 0
                           then getValue ((fst coords) - 1, (snd coords) + 1) matrix
                           else False
                           
getDiagLR coords matrix = if (snd coords) < (length matrix) - 1 && (fst coords) < (length (matrix !! 0)) - 1
                           then getValue ((fst coords) + 1, (snd coords) + 1) matrix
                           else False

countSurrounding :: (Int, Int) -> [[Bool]] -> Int
countSurrounding coords matrix = length (filter (== True) boolList)
    where
        boolList = [getAbove coords matrix, 
                    getBelow coords matrix, 
                    getLeft coords matrix, 
                    getRight coords matrix, 
                    getDiagUL coords matrix, 
                    getDiagUR coords matrix, 
                    getDiagLL coords matrix, 
                    getDiagLR coords matrix]
                    
willLive :: (Int, Int) -> [[Bool]] -> Bool
willLive coords matrix = if (getValue coords matrix) == True
                           then (1 < s && s < 4)
                           else (s == 3)
    where
        s = countSurrounding coords matrix

setMatrixValue :: Bool -> (Int, Int) -> [[Bool]] -> [[Bool]]
setMatrixValue value coords matrix =
	take (snd coords) matrix
	++ [setRowValue value (fst coords) (matrix!!(snd coords))]
	++ drop ((snd coords) + 1) matrix
	where
		setRowValue val x row =
			take x row ++ (val : (drop (x+1) row))

constructField :: Int -> Int -> Field
constructField width height = Field width height $ take height $ repeat $ line
    where
        line = take width $ repeat False
        
populateField :: Field -> [String] -> Field
populateField f [] = f
populateField field (line:conf) = addDot (populateField field conf) coords
    where
        coords = ( read (stringCoords !! 0), read (stringCoords !! 1) )
        stringCoords = words line

loadConf :: FilePath -> IO [String]
loadConf path = do
    rawConf <- readFile path
    return [chopComment line | line <- lines rawConf, length line > 0, head line /= '#']
        where
            chopComment :: String -> String
            chopComment [] = []
            chopComment (c:line) = if c == '#'
                                     then ""
                                     else c : (chopComment line)

buildFromConf :: [String] -> Field
buildFromConf confList = populateField (constructField (fst dim) (snd dim)) (drop 2 confList)
    where
        dim = parseDim confList
        parseDim :: [String] -> (Int, Int)
        parseDim list = ( read (list !! 0), read (list !! 1) )

main = do
    conf <- loadConf "conf"
    let myField = buildFromConf conf
    input <- getContents
    let r = map (advance myField) ((lines input) `zip` [0..])
    putStr (unlines r)
    where
        advance field (_, 0) = show field
        advance field (_, num) = advance (step field) ("", num - 1)
        
        
        
        