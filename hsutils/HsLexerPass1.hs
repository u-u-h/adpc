module HsLexerPass1 where
import HsLex(haskellLex)
import HsLexUtils
import HsLayoutPre(layoutPre,Pos,PosToken)
import List(mapAccumL)

default(Int)

{-+
The function #lexerPass1# handles the part of lexical analysis that
can be done independently of the parser, i.e., the tokenization and the
addition of the extra layout tokens &lt;n&gt; and {n}, as specified in
section 9.3 of the revised Haskell 98 Report.
-}

type LexerOutput = [PosToken]
type Lexer = String -> LexerOutput

lexerPass1 :: Lexer
lexerPass1 = lexerPass1Only . lexerPass0

lexerPass1Only = layoutPre . rmSpace

rmSpace = filter (notWhite.fst)

notWhite t = t/=Whitespace &&
             t/=Commentstart && t/=Comment &&
             t/=NestedComment

-- Tokenize and add position information:

lexerPass0 :: Lexer
lexerPass0 = lexerPass0' startPos

lexerPass0' :: Pos -> Lexer
lexerPass0' pos0 = addPos . haskellLex . rmcr
  where
    addPos = snd . mapAccumL pos pos0

    pos p (t,r) = (nextPos p s,(t,(p,s)))
      where s = reverse r

startPos = (1,1) :: Pos -- The first column is designated column 1, not 0.


nextPos :: Pos -> String -> Pos
nextPos = foldl nextPos1

nextPos1 :: Pos -> Char -> Pos
nextPos1 (y,x) c =
    case c of
      -- The characters newline, return, linefeed, and formfeed, all start
      -- a new line.
      '\n'  -> (y+1, 1)
      '\CR' -> (y+1, 1)
      '\LF' -> (y+1, 1)
      '\FF' -> (y+1, 1)
      -- Tab stops are 8 characters apart.
      -- A tab character causes the insertion of enough spaces to align the
      -- current position with the next tab stop.
      -- + (not in the report) the first tab stop is column 1.
      '\t'  -> (y, x+8-(x-1) `mod` 8)
      _ -> (y, x+1)


{-+
Since #nextPos# examines one character at a time, it will increase the line
number by 2 if it sees \CR\LF, which can happen when reading DOS files on
a Unix like system.
Since the extra \CR characters can cause trouble later as well, we choose
to simply remove them here.
-}
rmcr ('\CR':'\LF':s) = '\LF':rmcr s
rmcr (c:s) = c:rmcr s
rmcr "" = ""
