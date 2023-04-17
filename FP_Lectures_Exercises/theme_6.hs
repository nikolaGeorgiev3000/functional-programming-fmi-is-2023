
main :: IO()
main = do
    print $ flipV ["    ##    ",
                  "   #  #   ",
                  "  #    #  ",
                  " ######## ",
                  "    ##    "]
    print $ flipH ["      #   ",
                  "    #     ",
                  " ######## ",
                  "       $  ",
                  "  %       "]
    print $ above [" ###### ",
                   "   $$   "]
                  ["   $$   ",
                   " ###### "]
    print $ sideBySide [" ##### ", " $$$ "] [" $$$ ", " ##### "]
    putStr "Hello, Haskell!\n" -- '\n' is used for a line-break


type Picture = [String]

flipV :: Picture -> Picture
flipV pic = reverse pic

flipH :: Picture -> Picture
flipH pic = [reverse line | line <- pic] -- Picture === [[Char]]

above :: Picture -> Picture -> Picture
above p q = p ++ q -- Easy

sideBySide :: Picture -> Picture -> Picture
sideBySide p q = [pLine ++ qLine | (pLine, qLine) <- zip p q]

