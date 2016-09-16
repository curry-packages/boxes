# boxes - A Pretty Printer for Two Dimensions

boxes is a pretty-printing library for laying out text in two dimensions. It is
a direct port of the Haskell library [boxes](1) by Brent Yorgey.

boxes' core data type is the `Box`, which has a width, a height and some 
contents. A box's contents can be text or other boxes. There are functions for
creating boxes from text and for combining boxes into bigger boxes.

## Creating Boxes

The `text` function can be used to create a box from a string, which will have
height 1 and length N, where N is the length of the string (Nx1). `char` creates
a 1x1 box containing a single character. `emptyBox` creates an empty box of 
arbitrary width and height.

`para :: Alignment -> Int -> String -> Box` creates a box from a string with a 
specific width. The box will be as high as necessary to fit the text, which is 
layed out according to the given alignment.

## Combining Boxes

The `<>` and `<+>` operators combine boxes horizontally with and without a 
column of space between both boxes, respectively. The `//` and `/+/` operators
are similar, but combine boxes vertically instead of horizontally. `hcat` and
`vcat` are versions of `<>` and `//` that combine whole lists of boxes instead
of two at a time. `hsep` and `vsep` are versions of `<+>` and `/+/` that operate
on lists, with a configurable amount of space between each two boxes. 
`punctuateH` and `punctuateV` also combine lists of boxes horizontally and
vertically, but allow us to specify another box which is copied in between each
two boxes.

The `align`, `alignVert` and `alignHoriz` functions can be used to create new
boxes which contain other boxes in some alignment. `moveUp`, `moveLeft`, 
`moveDown` and `moveRight` move boxes by some amount inside larger boxes.

`table` creates a table from a list of rows and a list of widths for each 
column. 

## Rendering Boxes

The `render` function renders a box to a string. The `printBox` function prints
a box to stdout.

[1]: https://hackage.haskell.org/package/boxes
