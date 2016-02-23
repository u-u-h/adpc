module HsLexUtils(module HsLexUtils,Token(..)) where
import HsTokens

gotEOF [] = []
gotEOF as = [(GotEOF, as)]

gotError as is =
  (ErrorToken, as):
  if null is then [(GotEOF,[])] else [(TheRest,reverse (take 80 is))]

-- Inlining the call to output does not make a big difference.
--output token as cont = (token, reverse as):cont

-- Not reversing the token string seems to save about 10% of the time
output token as cont = (token,as):cont

-- This avoids constructing a closure for the call to reverse.
-- This saves about 10% too.
{-
output token as cont =
    rev as []
  where
    rev [] as' = (token,as'):cont
    rev (a:as) as' = rev as (a:as')
--}

-- #ifndef __HBC__
isSymbol _ = False
-- #endif

nestedComment as is next = nest 0 as is
  where
    nest n as is =
      case is of
        '-':'}':is -> if n==0
                      then next gotError ('}':'-':as) is
                      else nest (n-1) ('}':'-':as) is
        '{':'-':is -> nest (n+1) ('-':'{':as) is
        c:is -> nest n (c:as) is
        _ -> gotError as is -- EOF inside comment
