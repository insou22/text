module Text where  

import ShapeGraphics
import Codec.Picture
import Data.Char

canvasSize :: Int
canvasSize = 800

maxRow :: Int
maxRow = 20

rowWidth :: Int
rowWidth = canvasSize `div` maxRow

maxCol :: Int
maxCol = 20

colWidth :: Int
colWidth = canvasSize `div` maxCol

-- Main

vectorToPath :: Colour -> Int -> Int -> ((Int, Int), (Int, Int)) -> PictureObject
vectorToPath colour xOff yOff ((xStart, yStart), (xEnd, yEnd)) = 
    Path [
        Point (fromIntegral (yOff * colWidth + xStart)) (fromIntegral (xOff * rowWidth + yStart)), 
        Point (fromIntegral (yOff * colWidth + xEnd))   (fromIntegral (xOff * rowWidth + yEnd))
    ] colour Solid

drawLetter :: Int -> Int -> Char -> Colour -> Picture
drawLetter row col chr colour = map (vectorToPath colour row col) (letterVectors chr)

iterateRowCol :: Int -> Int -> Maybe (Int, Int)
iterateRowCol row col
    | col + 1 < maxCol  = Just (row, col + 1)
    | row + 1 < maxRow  = Just (row + 1, 0)
    | otherwise         = Nothing

drawText1 :: String -> Colour -> Maybe (Int, Int) -> Picture
drawText1 "" _ _ = []
drawText1 _ _ Nothing = []
drawText1 (chr:chrs) colour (Just (row, col)) = drawLetter row col chr colour ++ drawText1 chrs colour (iterateRowCol row col)

drawText :: String -> Colour -> Picture
drawText str colour = drawText1 (map toLower str) colour $ Just (0, 0) 

text :: Picture
text = drawText ("heyo comp3141'ers!  i made a super epic font-face for our   shape-graphics      library! " ++
                 "now i can  finally write imper-atively in haskell! " ++
                 replicate 20 ' ' ++
                 "int main(void) {    " ++
                 " int i = 42;        " ++
                 " printf(\"%d\", i);   " ++
                 " return 0;          " ++
                 "}                   " ++
                 replicate 20 ' ' ++
                 "...and the fontface:" ++
                 "abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()[]{}_+-=`~\\|;:'\",./<>?" ++
                 replicate 12 ' ' ++
                 "git.insou.dev/text") green

writeToFile pic
  = writePng "text.png" (drawPicture 3 text)

-- 40 wide x 40 tall
letterVectors :: Char -> [((Int, Int), (Int, Int))]
letterVectors 'a' = [
        ((10, 10), (10, 30)),
        ((30, 10), (30, 30)),
        ((10, 20), (30, 20)),
        ((10, 10), (30, 10))
    ];
letterVectors 'b' = [
        ((10, 10), (30, 10)),
        ((10, 10), (10, 30)),
        ((10, 30), (30, 30)),
        ((10, 20), (25, 20)),
        ((25, 20), (30, 10)),
        ((25, 20), (30, 30))
    ];
letterVectors 'c' = [
        ((10, 10), (30, 10)),
        ((10, 10), (10, 30)),
        ((10, 30), (30, 30))
    ];
letterVectors 'd' = [
        ((10, 10), (10, 30)),
        ((10, 10), (25, 10)),
        ((10, 30), (25, 30)),

        ((25, 10), (30, 20)),
        ((25, 30), (30, 20))
    ];
letterVectors 'e' = [
        ((10, 10), (30, 10)),
        ((10, 20), (30, 20)),
        ((10, 30), (30, 30)),
        ((10, 10), (10, 30))
    ];
letterVectors 'f' = [
        ((10, 10), (30, 10)),
        ((10, 20), (30, 20)),
        ((10, 10), (10, 30))
    ];
letterVectors 'g' = [
        ((10, 10), (10, 30)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 20), (30, 30)),
        ((20, 20), (30, 20))
    ];
letterVectors 'h' = [
        ((10, 10), (10, 30)),
        ((30, 10), (30, 30)),
        ((10, 20), (30, 20))
    ];
letterVectors 'i' = [
        ((10, 10), (30, 10)),
        ((20, 10), (20, 30)),
        ((10, 30), (30, 30))
    ];
letterVectors 'j' = [
        ((10, 10), (30, 10)),
        ((20, 10), (20, 30)),
        ((10, 30), (20, 30))
    ];
letterVectors 'k' = [
        ((10, 10), (10, 30)),
        ((10, 20), (30, 10)),
        ((10, 20), (30, 30))
    ];
letterVectors 'l' = [
        ((10, 10), (10, 30)),
        ((10, 30), (30, 30))
    ];
letterVectors 'm' = [
        ((10, 10), (10, 30)),
        ((30, 10), (30, 30)),
        ((10, 10), (20, 20)),
        ((30, 10), (20, 20))
    ];
letterVectors 'n' = [
        ((10, 10), (10, 30)),
        ((30, 10), (30, 30)),
        ((10, 10), (30, 30))
    ];
letterVectors 'o'   = [
        ((10, 10), (10, 30)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30))
    ];
letterVectors 'p' = [
        ((10, 10), (10, 30)),
        ((30, 10), (30, 20)),
        ((10, 20), (30, 20)),
        ((10, 10), (30, 10))
    ];
letterVectors 'q'   = [
        ((10, 10), (10, 30)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30)),
        ((30, 30), (35, 35))
    ];
letterVectors 'r' = [
        ((10, 10), (10, 30)),
        ((30, 10), (30, 20)),
        ((10, 20), (30, 20)),
        ((10, 10), (30, 10)),
        ((20, 20), (30, 30))
    ];
letterVectors 's' = [
        ((10, 10), (30, 10)),
        ((10, 20), (30, 20)),
        ((10, 30), (30, 30)),
        ((10, 10), (10, 20)),
        ((30, 20), (30, 30))
    ];
letterVectors 't' = [
        ((10, 10), (30, 10)),
        ((20, 10), (20, 30))
    ];
letterVectors 'u'   = [
        ((10, 10), (10, 30)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30))
    ];
letterVectors 'v'   = [
        ((10, 10), (20, 30)),
        ((20, 30), (30, 10))
    ];
letterVectors 'w'   = [
        ((10, 10), (10, 30)),
        ((10, 30), (20, 20)),
        ((20, 20), (30, 30)),
        ((30, 30), (30, 10))
    ];
letterVectors 'x'   = [
        ((10, 10), (30, 30)),
        ((10, 30), (30, 10))
    ];
letterVectors 'y'   = [
        ((20, 30), (20, 20)),
        ((20, 20), (10, 10)),
        ((20, 20), (30, 10))
    ];
letterVectors 'z' = [
        ((10, 10), (30, 10)),
        ((30, 10), (10, 30)),
        ((10, 30), (30, 30))
    ];
letterVectors '1' = [
        ((10, 10), (20, 10)),
        ((20, 10), (20, 30)),
        ((10, 30), (30, 30))
    ];
letterVectors '2' = [
        ((10, 10), (30, 10)),
        ((10, 20), (30, 20)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 20)),
        ((10, 20), (10, 30))
    ];
letterVectors '3' = [
        ((10, 10), (30, 10)),
        ((10, 20), (30, 20)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30))
    ];
letterVectors '4' = [
        ((10, 10), (10, 20)),
        ((30, 10), (30, 30)),
        ((10, 20), (30, 20))
    ];
letterVectors '5' = letterVectors 's'
letterVectors '6' = [
        ((10, 10), (30, 10)),
        ((10, 20), (30, 20)),
        ((10, 30), (30, 30)),
        ((10, 10), (10, 30)),
        ((30, 20), (30, 30))
    ];
letterVectors '7' = [
        ((10, 10), (30, 10)),
        ((30, 10), (20, 30))
    ];
letterVectors '8'   = [
        ((10, 10), (10, 30)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30)),
        ((10, 20), (30, 20))
    ];
letterVectors '9'   = [
        ((10, 10), (10, 20)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30)),
        ((10, 20), (30, 20))
    ];
letterVectors '0'   = [
        ((10, 10), (10, 30)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30)),
        ((10, 30), (30, 10))
    ];
letterVectors '!'   = [
        ((20, 10), (20, 23)),
        ((20, 29), (20, 30))
    ];
letterVectors '@'   = [
        ((10, 10), (10, 30)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 20)),
        ((30, 20), (20, 20)),
        ((20, 20), (20, 10))
    ];
letterVectors '#'   = [
        ((11, 15), (31, 15)),
        ((10, 25), (30, 25)),
        ((13, 30), (18, 10)),
        ((22, 30), (27, 10))
    ];
letterVectors '$' = [
        ((12, 12), (28, 12)),
        ((12, 20), (28, 20)),
        ((12, 28), (28, 28)),
        ((12, 12), (12, 20)),
        ((28, 20), (28, 28)),
        ((20, 8), (20, 32))
    ];
letterVectors '%' = [
        ((10, 30), (30, 10)),
        
        ((10, 10), (10, 12)),
        ((10, 12), (12, 12)),
        ((12, 12), (12, 10)),
        ((12, 10), (10, 10)),

        ((30, 30), (30, 28)),
        ((30, 28), (28, 28)),
        ((28, 28), (28, 30)),
        ((28, 30), (30, 30))
    ];
letterVectors '^' = [
        ((15, 15), (20, 10)),
        ((25, 15), (20, 10))
    ];
letterVectors '&' = [
        ((20, 10), (30, 10)),
        ((20, 10), (20, 30)),
        ((30, 10), (30, 20)),
        ((10, 20), (30, 20)),
        ((10, 20), (10, 30)),
        ((10, 30), (30, 30))
    ];
letterVectors '*' = [
        ((20, 15), (20, 25)),
        ((16, 17), (24, 23)),
        ((24, 17), (16, 23))
    ];
letterVectors '(' = [
        ((20, 15), (20, 25)),
        ((20, 15), (25, 10)),
        ((20, 25), (25, 30))
    ];
letterVectors ')' = [
        ((20, 15), (20, 25)),
        ((20, 15), (15, 10)),
        ((20, 25), (15, 30))
    ];
letterVectors '[' = [
        ((20, 10), (20, 30)),
        ((20, 10), (25, 10)),
        ((20, 30), (25, 30))
    ];
letterVectors ']' = [
        ((20, 10), (20, 30)),
        ((20, 10), (15, 10)),
        ((20, 30), (15, 30))
    ];
letterVectors '{' = [
        ((20, 13), (20, 18)),
        ((20, 22), (20, 27)),
        ((20, 13), (23, 10)),
        ((20, 27), (23, 30)),
        ((20, 18), (17, 20)),
        ((20, 22), (17, 20))
    ];
letterVectors '}' = [
        ((20, 13), (20, 18)),
        ((20, 22), (20, 27)),
        ((20, 13), (17, 10)),
        ((20, 27), (17, 30)),
        ((20, 18), (23, 20)),
        ((20, 22), (23, 20))
    ];
letterVectors '-' = [
        ((15, 20), (25, 20))
    ];
letterVectors '_' = [
        ((10, 30), (30, 30))
    ];
letterVectors '+' = [
        ((15, 20), (25, 20)),
        ((20, 15), (20, 25))
    ];
letterVectors '=' = [
        ((15, 17), (25, 17)),
        ((15, 23), (25, 23))
    ];
letterVectors '`' = [
        ((15, 10), (20, 15))
    ];
letterVectors '~' = [
        ((15, 22), (18, 18)),
        ((18, 18), (22, 22)),
        ((22, 22), (25, 18))
    ];
letterVectors '\\' = [
        ((15, 10), (25, 30))
    ];
letterVectors '|' = [
        ((20, 10), (20, 30))
    ];
letterVectors ';' = [
        ((20, 14), (20, 16)),
        ((20, 23), (18, 26))
    ];
letterVectors ':' = [
        ((20, 14), (20, 16)),
        ((20, 24), (20, 26))
    ];
letterVectors '\'' = [
        ((20, 14), (18, 16))
    ];
letterVectors '"' = [
        ((18, 14), (18, 16)),
        ((22, 14), (22, 16))
    ];
letterVectors ',' = [
        ((20, 27), (18, 30))
    ];
letterVectors '.' = [
        ((20, 29), (20, 30))
    ];
letterVectors '/' = [
        ((15, 30), (25, 10))
    ];
letterVectors '<' = [
        ((15, 20), (25, 15)),
        ((15, 20), (25, 25))
    ];
letterVectors '>' = [
        ((25, 20), (15, 25)),
        ((25, 20), (15, 15))
    ];
letterVectors '?' = [
        ((10, 10), (10, 15)),
        ((10, 10), (30, 10)),
        ((30, 10), (30, 20)),
        ((30, 20), (20, 20)),
        ((20, 20), (20, 22)),
        ((20, 28), (20, 30))
    ];
letterVectors ' ' = []
letterVectors _   = [
        ((10, 10), (10, 30)),
        ((10, 10), (30, 10)),
        ((10, 30), (30, 30)),
        ((30, 10), (30, 30)),
        ((10, 10), (30, 30)),
        ((10, 30), (30, 10))
    ];