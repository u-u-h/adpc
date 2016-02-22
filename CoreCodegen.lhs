------------------------------------------------------------------------------
-- The ADP Compiler 
-- Copyright (C) 2001-2008 Peter Steffen, Christian Lang, Marco Ruether, 
--                         Georg Sauthoff, Stefanie Schirmer
--
-- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
-- Updates: http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html
------------------------------------------------------------------------------

This file is part of ADPC (The ADP Compiler).

ADPC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

ADPC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ADPC.  If not, see <http://www.gnu.org/licenses/>.



> module CoreCodegen where

> import List
> import CoreConstants
> import CoreSyntax
> import CoreTools
> import CoreTL


> type MType = ([(String, VarAccess)],  -- Binding: CoreVar -> TLVar
>               [VarAccess],            -- Temporaere Variablen
>               [String])               -- unbekannte Funktionen

> data SM a = SM (MType -> (a,MType))  -- The monadic type

> instance Monad SM where
>   -- defines state propagation
>   SM c1 >>= fc2         =  SM (\s0 -> let (r,s1) = c1 s0 
>                                           SM c2 = fc2 r in
>                                          c2 s1)
>   return k              =  SM (\s -> (k,s))

>  -- extracts the state from the monad
> readSM                  :: SM MType
> readSM                  =  SM (\s -> (s,s))

>  -- updates the state of the monad
> updateSM                :: (MType -> MType) -> SM ()  -- alters the state
> updateSM f              =  SM (\s -> ((), f s)) 

> -- run a computation in the SM monad
> runSM                   :: MType -> SM a -> (a,MType)
> runSM s0 (SM c)         =  c s0


> tvN n = Direct $ "t" ++ show n  


> newtv :: DataType -> SM VarAccess      
> newtv dt = SM (\(bind, tvs, fct) -> 
>     let v    = (tvN (length tvs + 1), dt)
>         tvs' = v:tvs
>     in (v, (bind, tvs', fct)))

> getBind :: String -> SM TLExp
> getBind v = SM (\(bind, tvs, fct) ->
>     let tv = case getVal bind v of
>                       Just tlv  -> TLVar tlv
>                       otherwise -> TLVar $ vaVar v
>     in (tv, (bind, tvs, fct)))

> addBind :: (String, VarAccess) -> SM ()
> addBind bnd = SM (\(bind, tvs, fct) ->
>     let bind' = bnd:bind
>     in  ((), (bind', tvs, fct)))

> getBinding :: SM [(String, VarAccess)]
> getBinding = SM (\(bind, tvs, fct) -> (bind, (bind, tvs, fct)))

> addFct :: String -> SM ()
> addFct f = SM (\(bind, tvs, fct) ->
>     let fct' | elem f (map fst infixOps) = fct
>              | otherwise                 = f:fct
>     in  ((), (bind, tvs, fct')))


> vaVar v = (Direct v, TLVoid)
> expVar = TLVar . vaVar

 chcFcts = ["maximum", "minimum", "sum", "foldl1"]

> cgProg tab defs = let
>                     res = map (cgDef tab) defs
>                     fcts = nub $ concatMap fst res
>                     comment = commentBox $ ["unbounded symbols found; please implement:"] ++ [sepList ", " fcts]
>                     tl   = concatMap snd res
>                     tl' = tl -- updateDataStructures tl
>                   in [comment] ++ tl'

> cgDef :: [String] -> Def -> ([String], [TL])
> cgDef tab (n, args, exp) = (fcts, [TLFD [] TLVoid n args decls (code ++ code_return) TLNil])
>       where 
>         ((v_result,code),(_, tvs, fcts)) = runSM ([],[],[]) (cgCore exp)
>         args  = [(["i"], TLInt), (["j"], TLInt)]
>         decls = mergeDecls (map tvd tvs) where 
>                       tvd (Direct n, dt) = ([n], dt)
>         code_return  = [TLAssign (ArrayElem [expVar "i", expVar "j"] (Direct n), TLVoid) (TLVar v_result)]

> mergeDecls  ds = map (\(dt, vl) -> (nub vl, dt)) $ mergeDecls' [] ds
> mergeDecls' cll []     = cll
> mergeDecls' cll ((vs,dt):ds)            = mergeDecls' (appendA cll dt vs) ds



> lcRelOps = ["<", "<=", ">", ">=", "==", "/="]


> getCOp op = case getVal infixOps op of
>               Just o  -> o
>               Nothing -> error $ "cannot compile infix operation " ++ op


> cgCore (Ap (Var nt) (Ap (Ap (Constr "(,)") i) j)) 
>       = do
>            v <- newtv TLInt
>            (i', code_i)   <- coreToTLExp i
>            (j', code_j)   <- coreToTLExp j
>            let  code        = code_i ++ code_j ++ [TLAssign v (TLVar $ (ArrayElem [i', j'] (Direct nt), TLVoid))]
>            return (v, code)

> cgCore (Ap (Ap (Var "foldl1") (Var chc)) exp) 
>                            = do vItr <- newtv $ makeListStruct TLInt
>                                 vRes <- newtv TLInt
>                                 (vExp, code_exp) <- cgCore exp
>                                 let code = code_exp ++
>                                            [TLAssign vItr (TLVar vExp)] ++
>                                            [TLAssign vRes (expVar "NULL_ELEM")] ++
>                                            [TLWhile (TLFA (vaVar "/=") [TLVar vItr, TLNil])
>                                                ([TLAssign vRes (TLFA (vaVar chc) [TLVar vRes, TLVar vItr])] ++ 
>                                                  cgListStep vItr)]
>                                 addFct (chc)
>                                 return (vRes, code)
>                                  

> cgCore (Lc (c:cs)) = cgLC (cs ++ [c])
>   where

>   cgLC ((exp@(Ap (Ap (Var f) e1) e2)):exps) 
>                   | elem f lcRelOps = do (v', body') <- cgLC exps
>                                          (exp', code_exp') <- coreToTLExp exp
>                                          assign <- assignNil v'
>                                          let code = code_exp' ++
>                                                     [TLIf exp' 
>                                                        body'   -- then 
>                                                        assign] -- else
>                                          return (v', code)
>                   | length exps == 0  = cgCore exp
>                   | otherwise = do -- TODO: was passier mit v_exp??
>                                   (v_exp,  code_exp) <- cgCore exp
>                                   (v_exps, code_exps) <- cgLC  exps
>                                   return (v_exps, code_exp ++ code_exps)

>   cgLC ((exp@((ArgVar k) :<- (bLoop :.. eLoop))):exps)
>                                     = do  tl_k  <- newtv TLInt
>                                           addBind (k, tl_k)
>                                           (v', body') <- cgLC exps
>                                           (bLoop', code_bLoop) <- coreToTLExp bLoop
>                                           (eLoop', code_eLoop) <- coreToTLExp eLoop
>                                           let code = code_bLoop ++ code_eLoop ++ [TLFor tl_k bLoop' eLoop' body']
>                                           return (v', code)
>   cgLC (((ArgVar g) :<- exp):exps)     = do  
>                                           (v_exp,  code_exp)  <- cgCore exp
>                                           vItr                <- newtv $ makeListStruct (snd v_exp)
>                                           addBind (g, vItr)

>                                           (v_exps, code_exps) <- cgLC   exps
>                                           (v_cll, listInit)   <- newList v_exps

>                                           let code = listInit ++ 
>                                                      code_exp ++ 
>                                                      [TLAssign vItr (TLVar v_exp)] ++ 
>                                                      [TLWhile (TLFA (vaVar "/=") [TLVar vItr, TLNil]) 
>                                                          (code_exps ++ 
>                                                           cgAppend v_cll v_exps v_cll ++ 
>                                                           cgListStep vItr)]
>                                           return (v_cll, code)

>   cgLC [exp] = cgCore exp




> cgCore (Ap (Ap (Var "++") e1) e2) = do
>                                           (v1, code1) <- cgCore e1
>                                           (v2, code2) <- cgCore e2
>                                           v3         <- newtv TLInt
>                                           let code = code1 ++ code2 ++ cgAppend v3 v1 v2 
>                                           return (v3, code)

 cgCore exp = pattErr "cgCore" exp

> cgCore exp = do
>                  (exp', code_exp)      <- coreToTLExp exp
>                  (v, code_assign)      <- assignTLExp exp'
>                  return (v, code_exp ++ code_assign)

> cgAppend v v1 v2  = [TLAssign v (TLFA (vaVar "append") [TLVar v1, TLVar v2])] 

> cgListStep v = [TLAssign v (TLFA (vaVar "step") [TLVar v])]

 assignTLExp exp = do
                        v        <- newtv (makeListStruct TLInt)
                        let code = [TLAssign v (TLFA (vaVar "mkList") [exp])]
                        return (v,code)

> assignTLExp exp = do
>                        v        <- newtv TLInt
>                        let code = [TLAssign v exp]
>                        return (v,code)

> assignNil v = return [TLAssign v TLNil]
>                    

> newList (_,dt) = do
>                 v <- newtv (makeListStruct dt)
>                 let code = [TLAssign v TLNil]
>                 return (v,code)

> coreToTLExp (Var v)      = do
>                              bind <- getBinding
>                              exp  <- case getVal bind v of
>                                               (Just tlv) -> return $ TLVar tlv
>                                               otherwise  -> do
>                                                              addFct v
>                                                              return $ TLVar $ vaVar v
>                              return (exp, [])

> coreToTLExp (Num n)      = return (TLNum $ fromIntegral n, [])
> coreToTLExp exp@(Lc _)   = do
>                              (v, exp') <- cgCore exp
>                              return (TLVar v, exp') 
> coreToTLExp (Ap (Ap (Var f) e1) e2) | elem f (map fst infixOps) = 
>                      do
>                         (e1', code1) <- coreToTLExp e1
>                         (e2', code2) <- coreToTLExp e2
>                         return $ (TLFA (vaVar (getCOp f)) [e1', e2'], code1 ++ code2)

> coreToTLExp (Ap (Ap (Var "!") (Var arr)) index) = 
>                      do
>                         (index', code_index) <- coreToTLExp index
>                         return $ (TLVar (ArrayElem [index'] (Direct arr), TLVoid), code_index)

> coreToTLExp (Ap (Var nt) (Ap (Ap (Constr "(,)") i) j)) = 
>                      do
>                         (i', code1) <- coreToTLExp i 
>                         (j', code2) <- coreToTLExp j 
>                         return $ (TLVar $ (ArrayElem [i', j'] (Direct nt), TLVoid), code1 ++ code2)

> coreToTLExp (If c t e) = 
>                      do
>                         (c', code_c) <- coreToTLExp c
>                         (t', code_t) <- coreToTLExp t
>                         (e', code_e) <- coreToTLExp e
>                         return $ (TLIfExp c' t' e', code_c ++ code_t ++ code_e)

> coreToTLExp e@(Constr c) = error $ "coreToTLExp: expresssion not allowed here: " ++ show e
> coreToTLExp e@(_ :<- _)  = error $ "coreToTLExp: expresssion not allowed here: " ++ show e
> coreToTLExp e@(_ :.. _)  = error $ "coreToTLExp: expresssion not allowed here: " ++ show e

 coreToTLExp exp = pattErr "cgCore" exp

> coreToTLExp fe@(Ap (Ap (Var "foldl1") (Var chc)) exp) = 
>                      do
>                           (v, code) <- cgCore fe
>                           return (TLVar v, code)

> coreToTLExp x  =
>                      do
>                           let (r, args) = findRedex x []
>                           res_args <- mapM coreToTLExp args
>                           let args' = map       fst res_args
>                           let code  = concatMap snd res_args
>                           let (fct,result) = case r of
>                                  (Constr _) -> error "constructors not allowed here"
>                                  (Var f)    -> (f, TLFA (vaVar f) args')
>                                  xs         -> error $ "coreToTLExp bind: expression not allowed here " ++ show xs
>                           addFct fct
>                           return (result, code)



----------------------------------------------------------------------------------------------------
Association data type
-----------------------

> type Assoc a b  = [(a,b)]

> lookupA :: (Eq a, Show a) => Assoc a b -> a -> [b]
> lookupA ass x         = [ b | (a,b) <- ass, a==x ] 

> deleteA :: (Eq a, Show a) => Assoc a b -> a -> Assoc a b
> deleteA [] x          = error $ "element " ++ show x ++ " not found."
> deleteA ((a,b):ass) x | x == a = ass
>                       | otherwise = (a,b): deleteA ass x

> insertA :: (Eq a, Show a) => Assoc a b -> a -> b -> Assoc a b
> insertA ass a b       = (a,b):ass

> replaceA :: (Eq a, Show a) => Assoc a b -> a -> b -> Assoc a b
> replaceA ass a b      | length found == 0 = (a,b) : ass
>                       | otherwise         = insertA (deleteA ass a) a b 
>   where
>     found = [ b | (a',b') <- ass, a' == a ]

> appendA :: (Eq a, Show a) => Assoc a [b] -> a -> [b] -> Assoc a [b]
> appendA ass a b      | length found == 0 = (a,b) : ass
>                      | otherwise         = insertA (deleteA ass a) a (b ++ bs)
>   where
>     found = [ b' | (a',b') <- ass, a' == a ]
>     bs    = head found




------------------------------------------------------------------------------------------------------------------
------ Aufsammeln der generierten Strukturen
------------------------------------------------------------------------------------------------------------------

> updateDataStructures :: [TL] -> [TL]
> updateDataStructures tl = decls' ++ tl'
>   where
>     (structs, tl') = collectDataStructures (0,[]) tl
>     decls          = mergeDataStructures structs
>     (_,decls')     = collectDataStructures structs decls

> collectDataStructures :: (Int, [(String, [(String, DataType)])]) -> [TL] ->
>                         ((Int, [(String, [(String, DataType)])]),   [TL])
> collectDataStructures = collectTLs cdsWorker

> cdsWorker :: WorkersTL (Int, [(String,[(String,DataType)])])
> cdsWorker = makeWorkersTL cdsWorker (NOP, NOP, NOP, NOP, NOP, NOP, DO wrkDataType, NOP) where

>   wrkDataType (n, dts) (StructOf na []) = ((n, dts), StructOf na [])
>   wrkDataType (n, dts) (StructOf _ strct) | found == [] = ((n'+1, (sName, strct): dts'), StructOf sName                [])
>                                           | otherwise   = ((n',                   dts'), StructOf (fst (head (found))) [])
>                              where
>                                found               = [ (name, s) | (name, s) <- dts', s == strct]
>                                sName               = ptstr prefixes ++ show (n'+1)
>                                ((n', dts'), struct') = cdsds (n,dts) strct

>                                cdsds (n, dts) []           = ((n, dts), [])
>                                cdsds (n, dts) ((na,dt):ds) = ((n'', dts''), ((na, dt'): ds'))
>                                 where
>                                   ((n',  dts'),  dt') = wrkDataType (n, dts)   dt
>                                   ((n'', dts''), ds') = cdsds       (n', dts') ds
>   wrkDataType cll x = collectDataType cdsWorker cll x

> mergeDataStructures :: (Int, [(String, [(String, DataType)])]) -> [TL]
> mergeDataStructures = cdsToTypeDecl.mergeStruct.reverse.snd

> -- Konvertierung der aufgesammelten dts-Liste in eine TypeDecl:
> --------------------------------------------------------------------------------------------------------------
> cdsToTypeDecl dts = [TLTypeDecls (map toTD dts)] 
>   where 
>     toTD (name, [("next", _), ("last", _), ("item", dt)]) = StructDecl name 
>                                                            [(["next"], PointerOf (StructOf name [])), 
>                                                             (["last"], PointerOf (StructOf name [])),
>                                                             (["item"], dt)]
>     toTD (name, vds) = StructDecl name (map (\(n,d) -> ([n],d)) vds)

> -- Dieses ist notwendig, um verschachtelte Strukturen durch neue Strukturnamen zu ersetzen:
> --------------------------------------------------------------------------------------------------------------
> mergeStruct :: [(String, [(String, DataType)])] -> [(String, [(String, DataType)])]
> mergeStruct dts = map ms dts
>   where
>   ms  (n, strct) = (n, (map ms' strct))
>   ms' (n,(StructOf name [])) = (n,StructOf name [])
>   ms' (n,(StructOf name dt)) | found == [] = (n,(StructOf name dt)) -- error $ "mergeStruct: unknown structure " ++ show dt
>                              | otherwise   = (n,StructOf (fst (head found)) [])
>                             where
>                                found = [ (name, s) | (name, s) <- dts, s == dt]
>   ms' (n, x) = (n, x)



