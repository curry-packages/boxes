module Boxes where

-- Adapted from the Haskell boxes library by Brent Yorgey

import List (intersperse, transpose)

data Box = Box
  { rows :: Int
  , cols :: Int
  , content :: Content }

data Alignment = AlignFirst
               | AlignCenter1
               | AlignCenter2
               | AlignLast

top :: Alignment
top = AlignFirst

bottom :: Alignment
bottom = AlignLast

left :: Alignment
left = AlignFirst

right :: Alignment
right = AlignLast

center1 :: Alignment
center1 = AlignCenter1

center2 :: Alignment
center2 = AlignCenter2

data Content = Blank
             | Text String
             | Row [Box]
             | Col [Box]
             | SubBox Alignment Alignment Box

nullBox :: Box
nullBox = emptyBox 0 0

emptyBox :: Int -> Int -> Box
emptyBox r c = Box r c Blank

char :: Char -> Box
char c = Box 1 1 (Text [c])

text :: String -> Box
text t = Box 1 (length t) (Text t)

(<>) :: Box -> Box -> Box
l <> r = hcat top [l, r]

(<+>) :: Box -> Box -> Box
l <+> r = hcat top [l, emptyBox 0 1, r]

(//) :: Box -> Box -> Box
t // b = vcat left [t, b]

(/+/) :: Box -> Box -> Box
t /+/ b = vcat left [t, emptyBox 1 0, b]

hcat :: Alignment -> [Box] -> Box
hcat a bs = Box h w (Row $ map (alignVert a h) bs)
 where
  (w, h) = sumMax cols 0 rows bs

hsep :: Int -> Alignment -> [Box] -> Box
hsep sep a bs = punctuateH a (emptyBox 0 sep) bs

vcat :: Alignment -> [Box] -> Box
vcat a bs = Box h w (Col $ map (alignHoriz a w) bs)
 where
  (h, w) = sumMax rows 0 cols bs

sumMax :: (a -> Int) -> b -> (a -> b) -> [a] -> (Int, b)
sumMax f defaultMax g as = foldr go (,) as 0 defaultMax
 where
  go a r n b = (r $! f a + n) $! g a `max` b

vsep :: Int -> Alignment -> [Box] -> Box
vsep sep a bs = punctuateV a (emptyBox sep 0) bs

punctuateH :: Alignment -> Box -> [Box] -> Box
punctuateH a p bs = hcat a (intersperse p bs)

punctuateV :: Alignment -> Box -> [Box] -> Box
punctuateV a p bs = vcat a (intersperse p bs)

paraFill :: Alignment -> Int -> String -> Box
paraFill a n t = (\ss -> mkParaBoxFill a (length ss) n ss) $ flow n t

mkParaBoxFill :: Alignment -> Int -> Int -> [String] -> Box
mkParaBoxFill a h w = align AlignFirst a h w . vcat a . map text

para :: Alignment -> Int -> String -> Box
para a n t = (\ss -> mkParaBox a (length ss) ss) $ flow n t

columns :: Alignment -> Int -> Int -> String -> [Box]
columns a w h t = map (mkParaBox a h) . chunksOf h $ flow w t

mkParaBox :: Alignment -> Int -> [String] -> Box
mkParaBox a n = alignVert top n . vcat a . map text

flow :: Int -> String -> [String]
flow n t = map (take n)
         . getLines
         $ foldl addWordP (emptyPara n) (map mkWord . words $ t)

data Para = Para
  { paraWidth :: Int
  , paraContent :: ParaContent }

data ParaContent = Block
  { fullLines :: [Line]
  , lastLine :: Line }

emptyPara :: Int -> Para
emptyPara pw = Para pw (Block [] (Line 0 []))

getLines :: Para -> [String]
getLines (Para _ (Block ls l))
  | lLen l == 0 = process ls
  | otherwise   = process (l:ls)
  where process = map (unwords . reverse . map getWord . getWords) . reverse

data Line = Line 
  { lLen :: Int
  , getWords :: [Word] }

mkLine :: [Word] -> Line
mkLine ws = Line (sum (map ((+1) . wLen) ws) - 1) ws

startLine :: Word -> Line
startLine = mkLine . (:[])

data Word = Word
  { wLen :: Int
  , getWord :: String }

mkWord :: String -> Word
mkWord w = Word (length w) w

addWordP :: Para -> Word -> Para
addWordP (Para pw (Block fl l)) w
  | wordFits pw w l = Para pw (Block fl (addWordL w l))
  | otherwise       = Para pw (Block (l:fl) (startLine w))

addWordL :: Word -> Line -> Line
addWordL w (Line len ws) = Line (len + wLen w + 1) (w:ws)

wordFits :: Int -> Word -> Line -> Bool
wordFits pw w l = lLen l == 0 || lLen l + wLen w + 1 <= pw

alignHoriz :: Alignment -> Int -> Box -> Box
alignHoriz a c b = align a AlignFirst (rows b) c b

alignVert :: Alignment -> Int -> Box -> Box
alignVert a r b = align AlignFirst a r (cols b) b

align :: Alignment -> Alignment -> Int -> Int -> Box -> Box
align ah av r c = Box r c . SubBox ah av

moveUp :: Int -> Box -> Box
moveUp n b = alignVert top (rows b + n) b

moveDown :: Int -> Box -> Box
moveDown n b = alignVert bottom (rows b + n) b

moveLeft :: Int -> Box -> Box
moveLeft n b = alignHoriz left (cols b + n) b

moveRight :: Int -> Box -> Box
moveRight n b = alignHoriz right (cols b + n) b

table :: [[String]] -> [Int] -> Box
table rows widths = vcat left $ map (hcat left . map (uncurry $ paraFill left)) withLengths
 where
  withLengths = map (zip widths) rows

render :: Box -> String
render = unlines . renderBox

takeP :: a -> Int -> [a] -> [a]
takeP b n xs 
  | n <= 0 = []
  | otherwise = case xs of
    []     -> replicate n b
    (y:ys) -> y : takeP b (n - 1) ys

fReverse :: ([a], b) -> ([a], b)
fReverse (xs, y) = (reverse xs, y)

(***) :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
f1 *** f2 = \(x, y) -> (f1 x, f2 y)

takePA :: Alignment -> a -> Int -> [a] -> [a]
takePA c b n = glue . (takeP b (numRev c n) *** takeP b (numFwd c n)) . split
 where
  split t = fReverse . splitAt (numRev c (length t)) $ t
  glue = uncurry (++) . fReverse
  numFwd AlignFirst n = n
  numFwd AlignLast  _ = 0
  numFwd AlignCenter1 n = n `div` 2
  numFwd AlignCenter2 n = (n + 1) `div` 2
  numRev AlignFirst _ = 0
  numRev AlignLast  n = n
  numRev AlignCenter1 n = (n + 1) `div` 2
  numRev AlignCenter2 n = n `div` 2

blanks :: Int -> String
blanks = flip replicate ' '

renderBox :: Box -> [String]
renderBox (Box r c Blank) = resizeBox r c [""]
renderBox (Box r c (Text t)) = resizeBox r c [t]
renderBox (Box r c (Row bs)) = resizeBox r c
                             . merge
                             . map (renderBoxWithRows r)
                             $ bs
  where merge = foldr (zipWith (++)) (repeat [])
renderBox (Box r c (Col bs)) = resizeBox r c
                             . concatMap (renderBoxWithCols c)
                             $ bs
renderBox (Box r c (SubBox ha va b)) = resizeBoxAligned r c ha va
                                     . renderBox
                                     $ b

renderBoxWithRows :: Int -> Box -> [String]
renderBoxWithRows r b = renderBox (b { rows = r })

renderBoxWithCols :: Int -> Box -> [String]
renderBoxWithCols c b = renderBox (b { cols = c })

resizeBox :: Int -> Int -> [String] -> [String]
resizeBox r c = takeP (blanks c) r . map (takeP ' ' c)

resizeBoxAligned :: Int -> Int -> Alignment -> Alignment -> [String] -> [String]
resizeBoxAligned r c ha va = takePA va (blanks c) r . map (takePA ha ' ' c)

printBox :: Box -> IO ()
printBox = putStr . render

-- From Haskell's Data.List.Split

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = map (take n) (xs:(partials xs))
 where
  partials []       = []
  partials ys@(_:_) = let ys' = drop n ys in case ys' of
    []    -> []
    (_:_) -> (ys':(partials ys'))

sum :: [Int] -> Int
sum = foldl (+) 0

