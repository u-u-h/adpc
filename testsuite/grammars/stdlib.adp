> #typesyn Input = Char
> #typesyn Subword = (Int,Int)
> #typesyn Filter = Subword -> Bool
 
Terminale Parser:
 
> #extern empty    :: Parser ()
> #extern achar    :: Parser Input
> #extern char     :: Input -> Parser Input
> #extern astring  :: Parser Subword

RNA:

> #extern base     :: Parser Input
> #extern region   :: Parser Subword
> #extern basepairing :: Filter
> #extern minloopsize :: Filter
> #extern maxsize     :: Int -> Filter
> #extern minsize     :: Int -> Filter
> #extern size        :: Int -> Filter


Standard-Auswahlfunktionen fuer Algebren:

> #extern maximum  :: [a] -> [a]
> #extern minimum  :: [a] -> [a]
> #extern sum      :: Num a => [a] -> [a]
> #extern id       :: [a] -> [a]

Kombinatoren:

> #extern (|||)    :: Parser b -> Parser b -> Parser b
> #extern (<<<)    :: (b -> c) -> Parser b -> Parser c
> #extern (~~~)    :: Parser (b -> c) -> Parser b -> Parser c
> #extern (...)    :: Parser b -> ([b] -> [b]) -> Parser b
> #extern with     :: Parser b -> (Subword -> Bool) -> Parser b
> 
> #extern (~~)     :: Subword -> Subword -> Parser (b -> c) -> Parser b -> Parser c
> #extern (~~*)    :: Subword -> Int  -> Parser (a -> b) -> Parser a -> Parser b 
> #extern (*~~)    :: Int -> Subword  -> Parser (a -> b) -> Parser a -> Parser b 
> #extern (*~*)    :: Int -> Int  -> Parser (a -> b) -> Parser a -> Parser b 

Axiom:

> #extern axiom ::  Parser b -> [b] 

Operatoren:
 
> #extern (+)  :: Num a => a -> a -> a 
> #extern (-)  :: Num a => a -> a -> a 
> #extern (*)  :: Num a => a -> a -> a 
> #extern (/)  :: Num a => a -> a -> a 
> 
> #extern (<)  :: a -> a -> Bool 
> #extern (<=) :: a -> a -> Bool 
> #extern (>)  :: a -> a -> Bool 
> #extern (>=) :: a -> a -> Bool 
> #extern (==) :: a -> a -> Bool 
> #extern (/=) :: a -> a -> Bool 
> 
> #extern (&&) :: Bool -> Bool -> Bool 
> #extern (||) :: Bool -> Bool -> Bool 
> 
> #extern (++) :: String -> String -> String 
> #extern (:)  :: Char -> String -> String 

Typumwandlung, Beispiele:

> #extern double2int   :: Double -> Int  
> #extern float2double :: Float -> Double 
> #extern decode :: Char -> Int

