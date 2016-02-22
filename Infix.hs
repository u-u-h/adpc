{-------------------------------------------------------------------------------

        Module:                 Infix

        Description:            The Haskell Parser does not consider the fixity
                                of operators when parsing infix applications.
                                All infix applications are thus parsed as
                                if the operators were all left associative with
                                the same precedence.

                                This module contains code for re-parsing infix
                                applications taking into account the fixity of 
                                operators.

                                The important algorithm is the well known
                                operator precedence parsing algorithm. See
                                comments in the code for more detailed 
                                information.

        Primary Authors:        Bernie Pope (bjpop@cs.mu.oz.au)

-------------------------------------------------------------------------------}


module Infix ( infixer 
             , insertTopInfixDecls 
             , InfixMap
             , plusInfixMap
             , Fixity (..) 
             , showInfixMap
             , infixMapToList
             , emptyInfixMap
             ) where

import Language.Haskell.Syntax 
import Data.FiniteMap

--------------------------------------------------------------------------------

-- the fixity information
data Fixity = Fixity { fixity_assoc :: HsAssoc,
                       fixity_prec  :: Int }
            deriving Show

-- a mapping from qualified operator names to fixity information
type InfixMap = FiniteMap HsQName Fixity 

plusInfixMap :: InfixMap -> InfixMap -> InfixMap
plusInfixMap = plusFM

emptyInfixMap :: InfixMap
emptyInfixMap = emptyFM

infixMapToList :: InfixMap -> [(HsQName, Fixity)]
infixMapToList = fmToList

-- pretty crude illustration of infix map 
showInfixMap :: InfixMap -> String
showInfixMap infixMap 
   = unlines $ map show mapList
   where
   mapList = infixMapToList infixMap 

-- fix up infix applications in a list of decls, to be called with
-- the topmost declarations in a module
infixer :: InfixMap -> [HsDecl] -> [HsDecl] 
infixer infixMap decls 
   = runInfix infixMap $ mapM infixDecl decls 

-- this this the default fixity of an operator, to be used if the
-- operator does not have a corresponding fixity declaration
defaultFixity :: Fixity 
defaultFixity = Fixity { fixity_assoc = HsAssocLeft, fixity_prec = 9 }

--------------------------------------------------------------------------------

-- a monad for passing the infix map around

data State = State { stateInfixMap :: InfixMap }

newtype Infix a = Infix (State -> (a, State))

instance Monad Infix where
    return a
        = Infix (\state -> (a, state))
    Infix comp >>= fun
        = Infix (\state ->
                        let (result, newState) = comp state
                            Infix comp' = fun result
                        in comp' newState)

runInfix :: InfixMap -> Infix a -> a
runInfix infixMap (Infix comp)
   = result 
   where
   (result, _newState) = comp (State {stateInfixMap = infixMap})

select :: (State -> a) -> Infix a
select selector = Infix (\state -> (selector state, state))

updateState :: (State -> State) -> Infix ()
updateState f = Infix (\state -> ((), f state))

getInfixMap :: Infix InfixMap 
getInfixMap = select stateInfixMap 

getFixityQOp :: HsQOp -> Infix Fixity 
getFixityQOp op
   = getFixityQName $ qNameFromQOp op 

getFixityQName :: HsQName -> Infix Fixity 
-- special consideration of (:) because it is not
-- explicitly given fixity info in the Prelude 
-- this is the only one that needs special treatment
getFixityQName (Special HsCons) 
   = return $ Fixity {fixity_assoc = HsAssocRight, fixity_prec  = 5}
getFixityQName name 
   =  do infixMap <- getInfixMap
         case lookupFM infixMap name of
            Nothing -> return defaultFixity
            Just fixity -> return fixity

updateInfixMap :: [HsDecl] -> Infix ()
updateInfixMap decls
   = do oldInfixMap <- getInfixMap
        let newInfixMap = insertLocalInfixDecls oldInfixMap decls 
        updateState (\state -> state {stateInfixMap = newInfixMap})

qNameFromQOp :: HsQOp -> HsQName
qNameFromQOp (HsQVarOp qName) = qName
qNameFromQOp (HsQConOp qName) = qName

nameFromOp :: HsOp -> HsName
nameFromOp (HsVarOp name) = name
nameFromOp (HsConOp name) = name
            
--------------------------------------------------------------------------------

-- crawl over expressions

infixExp :: HsExp -> Infix HsExp
infixExp exp@(HsVar qName) = return exp
infixExp exp@(HsCon qName) = return exp
infixExp exp@(HsLit literal) = return exp
infixExp exp@(HsInfixApp e1 qop e2)
   = do infixList <- hsExpToInfixList exp 
        let eFix = opPrecParse HsInfixApp infixList 
        return eFix
infixExp exp@(HsApp e1 e2)
   = do e1Fix <- infixExp e1
        e2Fix <- infixExp e2
        return $ HsApp e1Fix e2Fix
infixExp (HsNegApp e)
   = do eFix <- infixExp e
        return $ HsNegApp eFix
infixExp (HsLambda sloc pats e)
   = do patsFix <- mapM infixPat pats
        eFix <- infixExp e
        return $ HsLambda sloc patsFix eFix
infixExp (HsLet decls e)
   = do updateInfixMap decls 
        eFix <- infixExp e
        declsFix <- mapM infixDecl decls
        return $ HsLet decls eFix
infixExp (HsIf e1 e2 e3)
   = do e1Fix <- infixExp e1
        e2Fix <- infixExp e2
        e3Fix <- infixExp e3
        return $ HsIf e1Fix e2Fix e3Fix
infixExp (HsCase e alts)
   = do eFix <- infixExp e
        altsFix <- mapM infixAlt alts
        return $ HsCase eFix altsFix
infixExp (HsDo stmts)
   = do stmtsFix <- mapM infixStmt stmts
        return $ HsDo stmtsFix
infixExp (HsTuple exps)
   = do expsFix <- mapM infixExp exps
        return $ HsTuple expsFix
infixExp (HsList exps)
   = do expsFix <- mapM infixExp exps
        return $ HsList expsFix
infixExp (HsParen exp)
   = do expFix <- infixExp exp
        return $ HsParen expFix
infixExp (HsLeftSection e qop)
   = do eFix <- infixExp e
        return $ HsLeftSection eFix qop 
infixExp (HsRightSection qop e)
   = do eFix <- infixExp e
        return $ HsRightSection qop eFix  
infixExp (HsRecConstr qName fieldUpdates)
   = do fixFieldUpdates <- mapM infixFieldUpdate fieldUpdates
        return $ HsRecConstr qName fixFieldUpdates
infixExp (HsRecUpdate e fieldUpdates)
   = do eFix <- infixExp e 
        fixFieldUpdates <- mapM infixFieldUpdate fieldUpdates
        return $ HsRecUpdate eFix fixFieldUpdates
infixExp (HsEnumFrom e)
   = do eFix <- infixExp e
        return $ HsEnumFrom eFix
infixExp (HsEnumFromTo e1 e2)
   = do e1Fix <- infixExp e1
        e2Fix <- infixExp e2
        return $ HsEnumFromTo e1Fix e2Fix
infixExp (HsEnumFromThen e1 e2)
   = do e1Fix <- infixExp e1
        e2Fix <- infixExp e2
        return $ HsEnumFromThen e1Fix e2Fix
infixExp (HsEnumFromThenTo e1 e2 e3)
   = do e1Fix <- infixExp e1
        e2Fix <- infixExp e2
        e3Fix <- infixExp e3
        return $ HsEnumFromThenTo e1Fix e2Fix e3Fix
infixExp (HsListComp e stmts)
   = do eFix <- infixExp e
        stmtsFix <- mapM infixStmt stmts
        return $ HsListComp eFix stmtsFix 
infixExp (HsExpTypeSig sloc e qualType)
   = do eFix <- infixExp e
        return $ HsExpTypeSig sloc eFix qualType
infixExp (HsAsPat name e)
   = do eFix <- infixExp e
        return $ HsAsPat name eFix
infixExp HsWildCard = return HsWildCard
infixExp (HsIrrPat e)
   = do eFix <- infixExp e
        return $ HsIrrPat eFix


--------------------------------------------------------------------------------

-- crawl over patterns

infixPat :: HsPat -> Infix HsPat
infixPat pat@(HsPVar name) = return pat
infixPat pat@(HsPLit lit)  = return pat
infixPat (HsPNeg pat)  
   = do pFix <- infixPat pat
        return $ HsPNeg pFix
infixPat pat@(HsPInfixApp p1 op p2)
   = do infixList <- hsPatToInfixList pat 
        let pFix = opPrecParse HsPInfixApp infixList 
        return pFix
infixPat (HsPApp qName pats)  
   = do patsFix <- mapM infixPat pats 
        return $ HsPApp qName patsFix
infixPat (HsPTuple pats)  
   = do patsFix <- mapM infixPat pats 
        return $ HsPTuple patsFix
infixPat (HsPList pats)  
   = do patsFix <- mapM infixPat pats 
        return $ HsPList patsFix
infixPat (HsPParen pat)
   = do pFix <- infixPat pat 
        return $ HsPParen pFix
infixPat (HsPRec qName patFields)
   = do fieldsFix <- mapM infixPatField patFields
        return $ HsPRec qName fieldsFix
infixPat (HsPAsPat name pat)
   = do pFix <- infixPat pat
        return $ HsPAsPat name pFix
infixPat HsPWildCard = return HsPWildCard
infixPat (HsPIrrPat pat)
   = do pFix <- infixPat pat
        return $ HsPIrrPat pFix

-- pattern fields
infixPatField :: HsPatField -> Infix HsPatField
infixPatField (HsPFieldPat qName pat)
   = do pFix <- infixPat pat
        return $ HsPFieldPat qName pFix 

--------------------------------------------------------------------------------

-- crawl over alternatives

infixAlt :: HsAlt -> Infix HsAlt
infixAlt (HsAlt sloc pat guardedAlts decls)
   = do pFix <- infixPat pat 
        fixAlts <- infixGuardedAlts guardedAlts
        fixDecls <- mapM infixDecl decls
        return $ HsAlt sloc pFix fixAlts fixDecls

infixGuardedAlts :: HsGuardedAlts -> Infix HsGuardedAlts
infixGuardedAlts (HsUnGuardedAlt e)
   = do eFix <- infixExp e
        return $ HsUnGuardedAlt eFix
infixGuardedAlts (HsGuardedAlts gAlts)
   = do fixGAlts <- mapM infixGuardedAlt gAlts
        return $ HsGuardedAlts fixGAlts

infixGuardedAlt :: HsGuardedAlt -> Infix HsGuardedAlt
infixGuardedAlt (HsGuardedAlt sloc e1 e2)
   = do e1Fix <- infixExp e1
        e2Fix <- infixExp e2
        return $ HsGuardedAlt sloc e1Fix e2Fix

--------------------------------------------------------------------------------

-- crawl over statements

infixStmt :: HsStmt -> Infix HsStmt
infixStmt (HsGenerator sloc pat e)
   = do pFix <- infixPat pat 
        eFix <- infixExp e
        return $ HsGenerator sloc pFix eFix
infixStmt (HsQualifier e)
   = do eFix <- infixExp e
        return $ HsQualifier eFix
infixStmt (HsLetStmt decls)
   = do updateInfixMap decls 
        declsFix <- mapM infixDecl decls
        return $ HsLetStmt declsFix

--------------------------------------------------------------------------------

-- crawl over field updates

infixFieldUpdate :: HsFieldUpdate -> Infix HsFieldUpdate
infixFieldUpdate (HsFieldUpdate qName e)
   = do eFix <- infixExp e
        return $ HsFieldUpdate qName eFix

--------------------------------------------------------------------------------

-- crawl over declarations

infixDecl :: HsDecl -> Infix HsDecl
infixDecl (HsFunBind matches)
   = do fixMatches <- mapM infixMatch matches
        return $ HsFunBind fixMatches
infixDecl (HsPatBind sloc pat rhs decls)
   = do pFix <- infixPat pat 
        updateInfixMap decls
        fixRhs <- infixRhs rhs
        fixDecls <- mapM infixDecl decls
        return $ HsPatBind sloc pFix fixRhs fixDecls
infixDecl (HsClassDecl sloc cntxt className args decls)
   = do declsFix <- mapM infixDecl decls
        return $ HsClassDecl sloc cntxt className args declsFix
infixDecl (HsInstDecl sloc cntxt className args decls)
   = do declsFix <- mapM infixDecl decls
        return $ HsInstDecl sloc cntxt className args declsFix
infixDecl otherDecl = return otherDecl

-- matches
infixMatch :: HsMatch -> Infix HsMatch
infixMatch (HsMatch sloc name pats rhs decls)
   = do patsFix <- mapM infixPat pats 
        updateInfixMap decls
        fixRhs <- infixRhs rhs
        fixDecls <- mapM infixDecl decls
        return $ HsMatch sloc name patsFix fixRhs fixDecls

-- rhs
infixRhs :: HsRhs -> Infix HsRhs
infixRhs (HsUnGuardedRhs e)
   = do eFix <- infixExp e
        return $ HsUnGuardedRhs eFix
infixRhs (HsGuardedRhss rhss)
   = do rhssFix <- mapM infixGuardedRhs rhss
        return $ HsGuardedRhss rhssFix

-- guarded rhs
infixGuardedRhs :: HsGuardedRhs -> Infix HsGuardedRhs
infixGuardedRhs (HsGuardedRhs sloc e1 e2)
   = do e1Fix <- infixExp e1
        e2Fix <- infixExp e2
        return $ HsGuardedRhs sloc e1Fix e2Fix

--------------------------------------------------------------------------------

-- add new infix rules to an existing table
-- this is only for local infix decls (those not at the top-level of a 
-- module). These decls only cause unqualified names to be entered
-- into the infix map. 

insertLocalInfixDecls :: InfixMap -> [HsDecl] -> InfixMap
insertLocalInfixDecls imap [] = imap
insertLocalInfixDecls imap (HsInfixDecl sloc assoc prec ops : decls) 
   = insertLocalInfixDecls (insertLocalOps thisFixity ops imap) decls
   where
   thisFixity = Fixity { fixity_assoc = assoc, fixity_prec = prec }
   insertLocalOps :: Fixity -> [HsOp] -> InfixMap -> InfixMap
   insertLocalOps _fixity [] imap = imap
   insertLocalOps fixity (op:ops) infixMap
      = let opName = nameFromOp op
            map1   = addToFM infixMap (UnQual opName) fixity
        in insertLocalOps fixity ops map1 
insertLocalInfixDecls imap (otherDecl : decls) 
   = insertLocalInfixDecls imap decls 

-- as above but for top-level infix decls (which (sadly) can also occur inside
-- class declarations))
-- Top level infix decls cause unqualified AND
-- qualified names to be entered into the infix map
insertTopInfixDecls :: Module -> InfixMap -> [HsDecl] -> InfixMap
insertTopInfixDecls modName imap [] = imap
insertTopInfixDecls modName imap (HsInfixDecl sloc assoc prec ops : decls) 
   = insertTopInfixDecls modName (insertTopOps modName thisFixity ops imap) decls
   where
   thisFixity :: Fixity
   thisFixity = Fixity { fixity_assoc = assoc, fixity_prec = prec }
   insertTopOps :: Module -> Fixity -> [HsOp] -> InfixMap -> InfixMap
   insertTopOps modName _fixity [] imap = imap
   insertTopOps modName fixity (op:ops) infixMap
      = let opName = nameFromOp op
            -- insert qualified and unqualified versions of the name
            -- into the infix table
            map1 = addToFM infixMap (UnQual opName) fixity 
            map2 = addToFM map1 (Qual modName $ opName) fixity 
        in  insertTopOps modName fixity ops map2
-- class decls can have infix decls and these are treated in the same way
-- as top-level infix decls
insertTopInfixDecls modName imap (HsClassDecl _sloc _cntxt _name _args classDecls : decls)
   = insertTopInfixDecls modName (insertTopInfixDecls modName imap classDecls) decls
-- step over any other decl
insertTopInfixDecls modName imap (otherDecl : decls) 
   = insertTopInfixDecls modName imap decls 

{------------------------------------------------------------------------------- 
  
    Here is the important part.

   The parser does not take the fixity (precedence and associativity) of 
   infix operators into account when it parses a module. This is because
   imported operators may have their own fixity, so to know all the fixity
   information for a single module, the parser would have to know the fixity
   information for imported modules - which would require partial parsing of
   imported modules, and would be a mess.

   Instead it parses all infix applications as if they were left associative,
   with each operator having the same precedence, for example:

      1 + 2 ^ 4 * 6 - 8

   would be parsed as:

      ((((1 + 2) ^ 4) * 6) - 8)

  This is clearly wrong. To fix this we re-parse the expression taking fixity
  into account.

  The first step is to convert it into a more convenient form: from a parse tree
  to a list of expressions (alternating between expression and operator):

      [Arg 1, 
       Op (+) HsAssocLeft 6, 
       Arg 2, 
       Op (^) HsAssocRight 8,
       Arg 4,
       Op (*) HsAssocLeft 7,
       Arg 6,
       Op (-) HsAssocLeft 6,
       Arg 8]

   The conversion is done by hsExpToInfixList. This stage is not really necessary.
   However, I feel that it does contribute to the clarity of the program, and
   correctness is more important than efficiency here. 

   The second step is to reparse this expression using the well known 
   operator precedence parsing technique.

   The basic idea is to introduce two stacks: one for expressions (the arguments
   in between the operators), and one for operators. Parsing alternates between dealing with
   the next expression and dealing with the next operator.

   Expressions are always pushed onto the stack when they are encountered as the next
   item in the input list.

   Operators are pushed onto the operator stack iff they have a higher precedence than
   the operator on top of the stack (or the stack is empty). 

   If the next operator has a lower precedence than the top operator in the operator stack,
   a reduction is performed. The top operator on the stack is applied to the two topmost
   expressions on the expression stack, forming a new experssion (an infix application).
   This new expression replaces the two top expressions on the expression stack, we then
   continue.

   If the next operator and the top operator have the same precedence, then their 
   associativity determines what happens. If they are both left associative then a
   reduction is performed. If they are both right associative then the next operator
   is shifted. If they are a mixture of anything else (left, right, non-assoc) then
   a syntax error has occurred.

   Two non-associative operators are allowed in sequence iff they have different precedences.

   eStack            opStack    input
   {}                {}         1 + 2 ^ 4 * 6 - 8 
   1                 {}           + 2 ^ 4 * 6 - 8    (shift exp)
   1                 +              2 ^ 4 * 6 - 8    (shift op)
   1,2               +                ^ 4 * 6 - 8    (shift exp)
   1,2               +,^                4 * 6 - 8    (shift op)
   1,2,4             +,^                  * 6 - 8    (shift exp)
   1,(2^4)           +                    * 6 - 8    (reduce)
   1,(2^4)           +,*                    6 - 8    (shift op)
   1,(2^4),6         +,*                      - 8    (shift exp)
   1,((2^4)*6)       +                        - 8    (reduce)
   (1+((2^4)*6))     {}                       - 8    (reduce, b/c +,- are left associative)
   (1+((2^4)*6))     -                          8    (shift op)
   (1+((2^4)*6)),8   -                         {}    (shift exp)
   ((1+((2^4)*6))-8) {}                        {}    (reduce)

   The final state MUST have exactly one expression in the expression stack,
   zero operators in the operator stack, and zero items left in the input.

   All other final states are horrible errors, though in our case some parsing
   has already been done, and so some types of error will be caught earlier.

   Similar reasoning to the above applies for infix pattern applications, 
   as in:

   foo (x:y:z:zs)  is parsed as foo ((((x:y):z):zs)), which is wrong.
   The precedence parser has been made sufficiently generic to reparse
   expressions and patterns.

--------------------------------------------------------------------------------}

{- here we just convert the syntax trees of patterns and expressions into 
   something more convenient to manipulate for the later stage of
   precedence parsing -}

-- an operator or an argument (for an operator)
-- generalised to support expressions and patterns
data InfixExp op arg
   = Op op Fixity | Arg arg
   deriving Show

hsExpToInfixList :: HsExp -> Infix [InfixExp HsQOp HsExp]
hsExpToInfixList exp 
   = hsExpToInfixAcc exp [] 
   where
   hsExpToInfixAcc :: HsExp -> [InfixExp HsQOp HsExp] -> Infix [InfixExp HsQOp HsExp]
   hsExpToInfixAcc (HsInfixApp e1 op e2) acc
      = do fixity <- getFixityQOp op
           e2Fix <- infixExp e2
           hsExpToInfixAcc e1 (Op op fixity : Arg e2Fix : acc)
   hsExpToInfixAcc exp acc
      = do eFix <- infixExp exp
           return $ Arg eFix : acc 

-- as above but for patterns
hsPatToInfixList :: HsPat -> Infix [InfixExp HsQName HsPat]
hsPatToInfixList pat 
   = hsPatToInfixAcc pat [] 
   where
   hsPatToInfixAcc :: HsPat -> [InfixExp HsQName HsPat] -> Infix [InfixExp HsQName HsPat]
   hsPatToInfixAcc (HsPInfixApp p1 op p2) acc
      = do fixity <- getFixityQName op
           p2Fix <- infixPat p2
           hsPatToInfixAcc p1 (Op op fixity : Arg p2Fix : acc)
   hsPatToInfixAcc pat acc
      = do pFix <- infixPat pat 
           return $ Arg pFix : acc 

--------------------------------------------------------------------------------

-- operator precedence parsing

opPrecParse :: (arg -> op -> arg -> arg) -> [InfixExp op arg] -> arg 
opPrecParse combiner exps
   = reparseInfix combiner exps [] []
   where
   reparseInfix :: (arg -> op -> arg -> arg) 
                -> [InfixExp op arg]  
                -> [(op, Fixity)] 
                -> [arg] 
                -> arg 
   -- successful termination
   reparseInfix _combiner [] [] [arg] = arg 
   -- consume the remaining operators that have been stacked
   reparseInfix combiner [] ((op,_fixity):opStack) (a1:a2:argStack) 
      = reparseInfix combiner [] opStack (combiner a2 op a1 : argStack)
   -- always shift the expressions
   reparseInfix combiner (Arg a : args) opStack argStack
      = reparseInfix combiner args opStack (a : argStack)
   -- empty operator stack, just shift the operator
   reparseInfix combiner (Op thisOp thisFixity : rest) [] argStack
      = reparseInfix combiner rest [(thisOp,thisFixity)] argStack
   reparseInfix combiner (Op thisOp thisFixity : rest) (topOpStack:opStack) (topArg1:topArg2:argStack)
      -- shift the operator
      | thisPrec > topPrec 
           = reparseInfix combiner rest ((thisOp,thisFixity):topOpStack:opStack)
                               (topArg1:topArg2:argStack)
      -- reduce
      | thisPrec < topPrec 
           = reparseInfix combiner (Op thisOp thisFixity : rest) opStack
                          (combiner topArg2 topOp topArg1 : argStack)
      -- equal precedence, check the associativity
      | otherwise 
           = case (thisAssoc, topAssoc) of
                -- reduce
                (HsAssocLeft, HsAssocLeft) 
                   -> reparseInfix combiner (Op thisOp thisFixity : rest) opStack
                                   (combiner topArg2 topOp topArg1 : argStack)
                -- shift
                (HsAssocRight, HsAssocRight)
                   -> reparseInfix combiner rest ((thisOp,thisFixity):topOpStack:opStack)
                                        (topArg1:topArg2:argStack)
                         -- this error needs fixing
                other -> error "Syntax error"
      where
      (topOp, topFixity) = topOpStack
      topAssoc  = fixity_assoc topFixity
      topPrec   = fixity_prec topFixity
      thisAssoc = fixity_assoc thisFixity 
      thisPrec  = fixity_prec thisFixity
   -- XXX this error needs fixing
   reparseInfix _ _ _ _ = error "Syntax error" 

--------------------------------------------------------------------------------
