-- data

module Language where
    data Expr a = EVar Name                                 -- 变量
                    | ENum Int                              -- 数�?
                    | EConstr Int Int                       -- 构造函�?
                    | EAp (Expr a)  (Expr a)                -- 函数应用
                    | ELet IsRec [(a, Expr a)] (Expr a)     -- let 定义 IsRec �?否能递归定义 [(a, Expr a)] 定义 Expr
                    | ECase (Expr a) [Alter a]              -- case 表达�?
                    | ELam [a] (Expr a)                     -- lambda 表达�?

    type CoreExpr = Expr Name
    type Name = String
    type IsRec = Bool
    recursive  :: IsRec
    recursive = True
    nonRecursive :: IsRec
    nonRecursive = False



    bindersOf :: [(a, b)] -> [a]  -- Pack(tag, arity)
    bindersOf defns = [name | (name, _) <- defns]
    rhssOf :: [(a, b)] -> [b]
    rhssOf defns = [rhs | (_, rhs) <- defns]

    -- case
    type Alter a = (Int, [a], Expr a)
    type CoreAlt = Alter Name

    isAtomicExpr :: Expr a -> Bool
    isAtomicExpr (EVar _) = True
    isAtomicExpr (ENum _) = True
    isAtomicExpr _        = False

    type Program a = [ScDefn a]  -- ScDefn: supercombinator definitions
    type CoreProgram = Program Name
    type ScDefn a = (Name, [a], Expr a)  -- [a]: argument list Expr
    type CoreScDefn = ScDefn Name
    
    -- prelude
    {-
        I x             = x         --id
        K x y           = x         -- const
        K1 x y          = y         -- const . id
        S f g x         = f x (g x)
        compose f g x   = f (g x)
        twice f         = compose f f
    -}
    preludeDefs :: CoreProgram
    preludeDefs = [
        ("I", ["x"], EVar "x"),
        ("K", ["x", "y"], EVar "x"),
        ("K1", ["x", "y"], EVar "y"),
        ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))  (EAp (EVar "g") (EVar "x"))),
        ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
        ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))]

    mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
    mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
                        where
                        e2s = e2 : e2s





    -- Show CoreProgram
    data Iseq = INil
                | IStr String
                | IAppend Iseq Iseq
                | IIndent Iseq
                | INewline

    iNil :: Iseq
    iNil = INil

    iStr :: String -> Iseq
    iStr = IStr

    iAppend :: Iseq -> Iseq -> Iseq
    iAppend = IAppend

    iNewline :: Iseq
    iNewline = INewline

    instance Show Iseq where
        show = iDisplay

    iConcat :: [Iseq] -> Iseq
    iConcat = foldl iAppend iNil

    iInterleave :: Iseq -> [Iseq] -> Iseq
    iInterleave iseq = foldl iAppend' iNil
                        where iAppend' s1 = iAppend  (iAppend s1 iseq)

    iIndent :: Iseq -> Iseq
    iIndent = IIndent

    iNum :: Int -> Iseq
    iNum n = iStr (show n)

    iFWNum :: Int -> Int -> Iseq
    iFWNum width n =    iStr (space (width - length digits) ++ digits)
                        where
                        digits = show n

    iLayn :: [Iseq] -> Iseq
    iLayn seqs = let lay_item (n, s) = iConcat [iFWNum 4 n, iStr ") ", iIndent s, iNewline]
                 in iConcat (map lay_item (zip [1..] seqs))

    space :: Int -> String
    space n = take n spaces
              where spaces = ' ': spaces

    -- flatten which can keep track of the current column
    flatten2 :: Int -> [(Iseq, Int)] -> String
    flatten2 _   []                             = ""
    flatten2 col ((INil, _) : seqs)             = flatten2 col seqs
    flatten2 col ((IStr s, _) : seqs)           = s ++ flatten2 (col + length s) seqs
    flatten2 _   ((INewline, indent) : seqs)      = '\n' : space indent ++ flatten2 indent seqs
    flatten2 col ((IIndent s, _) : seqs)        = flatten2 col ((s, col) : seqs)
    flatten2 col ((IAppend s1 s2, _) : seqs)    = flatten2 col ((s1, col) : (s2, col) : seqs)



    iDisplay :: Iseq -> String
    iDisplay s = flatten2 0 [(s, 0)]

    printDefn :: (Name, CoreExpr) -> Iseq
    printDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (printExpr expr)]


    printDefns :: [(Name, CoreExpr)] -> Iseq
    printDefns defns = iInterleave sep (map printDefn defns)
                        where
                        sep = iConcat [ iStr " ", iNewline ]
    printAlt :: CoreAlt -> Iseq
    printAlt (i, xs, e) = iConcat [iStr "<", iStr (show i) , iStr "> ", iConcat(map iStr xs) ,iStr" -> ", printExpr e, iNewline]

    printAlts :: [CoreAlt] -> Iseq
    printAlts xs = iConcat (map printAlt xs)

    printAExpr :: CoreExpr -> Iseq
    printAExpr e = if isAtomicExpr e then printExpr e
                   else IStr "(" `IAppend` printExpr e `IAppend` IStr ")"

    printExpr :: CoreExpr -> Iseq
    printExpr (EVar v) = iStr ("EVar " ++ v)
    printExpr (ENum n) = iStr ("ENum " ++ show n)
    printExpr (EAp (EAp (EVar op) e1) e2) = let infix1 = getExprInfix e1
                                                infix2 = getInfix op
                                                infix3 = getExprInfix e2
                                            in let printExpr1 = if (infix1 < infix2) then printAExpr
                                                                else printExpr 
                                                   printExpr2 = if (infix3 < infix2) then printAExpr
                                                                else printExpr 
                                               in printExpr1 e1 `iAppend` iStr (" `" ++ op ++ "` ") `iAppend` printExpr2 e2
    printExpr (EAp e1 e2) = iConcat [iStr "(EAp (", printExpr e1, iStr ") " , printAExpr e2, iStr ")"]
    printExpr (ELet isrec defns expr) = iConcat [  iStr keyword,
                                                    iStr " ", iIndent (printDefns defns), iNewline,
                                                    iStr "in ", printExpr expr]
                                            where
                                            keyword | isrec = "letrec"
                                                    | otherwise = "let"
    printExpr (ECase expr alts) = iConcat [iStr "case ", printExpr expr, iStr " of ", iNewline,
                                             iIndent (printAlts alts) ]
  
    printCoreProgram :: CoreProgram -> Iseq 
    printCoreProgram [] = INil
    printCoreProgram ((name, vars, expr): xs) = iConcat [iStr name, iConcat (map (\x -> iStr (" EVar " ++ x)) vars), iStr " = " , iIndent (printExpr expr), iNewline, iNewline] `iAppend` printCoreProgram xs


    -- tests
    -- TODO: give printExpr an extra argument which indicates the precedence level of its context, and then use
    -- this to decide whether to add parentheses around the expression it produces.
    --
    -- x + y > p * length xs
    -- EAp (EAp (EVar "+") (EVar "x")) (EVar "y") infix
    -- EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")) infix
    --
    -- binop -> arithop | relop | boolop
    -- arithop -> + - * /
    -- relop -> < <== == ~= >= >
    -- boolop -> & |

    getExprInfix :: CoreExpr -> Int 
    getExprInfix (EAp e1 e2) = case e1 of 
                                        (EAp _ _)   -> getExprInfix e1
                                        EVar op     -> getInfix op
    getExprInfix _           = 5

    getInfix :: String -> Int
    getInfix op | op `elem` ["&", "|"]                          = 0
                | op `elem` ["<", "<=", "==", "!=", ">=", ">"]  = 1
                | op `elem` ["+", "-"]                          = 2
                | op `elem` ["*", "/"]                          = 3
                | otherwise                                     = 4
                    

    test :: CoreExpr
    test = ELet False [("x", EAp (EAp (EVar "+") (EVar "2")) (EVar "2"))] (EVar "y")

    test2 :: CoreExpr
    test2 = EAp (EAp (EVar "+") (EVar "2")) (EVar "2")

    test3 :: CoreExpr
    test3 = EAp (EAp (EVar "+") (EVar "x")) (EVar "y")

    test4 :: CoreExpr
    test4 = EAp (EAp (EVar "*") test3) (EAp (EVar "length") (EVar "xs"))

    test5 :: CoreExpr
    test5 = EAp (EAp (EVar ">") test3) test4

    -- A parser for the core language
    -- 
    type Token = (Int, String)

    isWhiteSpace :: Char -> Bool
    isWhiteSpace = (flip elem) " \t"

    isDigit :: Char -> Bool
    isDigit = (flip elem) ['0'..'9']

    isAlpha :: Char -> Bool
    isAlpha = (flip elem) (['a'..'z'] ++ ['A'..'Z'])

    isIdChar :: Char -> Bool
    isIdChar c = isAlpha c || isDigit c || c == '_'
    
    isNotEnter :: Char -> Bool 
    isNotEnter = (/=) '\n'

    twoCharOps :: [String]
    twoCharOps = ["==", "/=", ">=", "<=", "->"]

    clex :: Int -> String -> [Token]
    clex _ [] = []
    clex col s@(c:cs)   | c == '|'              =   case cs of            --ignore comments, which is indicated by '||'
                                                        ('|' : cs') -> clex col (dropWhile isNotEnter cs')
                                                        _           -> (col, "|") : clex col cs
                        | isWhiteSpace c        =   clex col cs
                        | not $ isNotEnter c    =   clex (col + 1) cs
                        | isDigit c             =   let num_token = (col, c : takeWhile isDigit cs)
                                                        rest_cs   = dropWhile isDigit cs
                                                    in  num_token : clex col rest_cs         
                        | isAlpha c             =   let var_token = (col, c : takeWhile isIdChar cs)
                                                        rest_cs   = dropWhile isIdChar cs
                                                    in  var_token : clex col rest_cs
                        | otherwise             =   let op = take 2 s 
                                                    in  if op `elem` twoCharOps then (col, op) : clex col (drop 2 s)
                                                        else (col, [c]) : clex col cs

    syntax :: [Token] -> CoreProgram 
    syntax tokens = fst $ head (pProgram tokens)

    parse :: String -> CoreProgram
    parse s = syntax (clex 0 s)

    -- test
    test6 :: String
    test6 = "y = x + x\n z = y + y || double y\n a = z * length z"

    type Parser a = [Token] -> [(a, [Token])]

    pAlt :: Parser a -> Parser a -> Parser a
    pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

    pHelloOrGoodbye :: Parser String 
    pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")
    
    pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    pThen combine p1 p2 toks = [(combine v1 v2, toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1]

    pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
    pThen3 combine p1 p2 p3 toks = [(combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2]

    pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
    pThen4 combine p1 p2 p3 p4 toks = [(combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2, (v4, toks4) <- p4 toks3]


    pGreeting :: Parser (String, String)
    pGreeting = pThen mk_pair pHelloOrGoodbye pVar
                where
                    mk_pair hg name = (hg, name)
    
    pZeroOrMore :: Parser a -> Parser [a]
    pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

    pOneOrMore :: Parser a -> Parser [a]
    pOneOrMore p =  take 1 . pThen (:) p (pZeroOrMore p)

    pEmpty :: a -> Parser a
    pEmpty x toks = [(x, toks)]

    pApply :: Parser a -> (a -> b) -> Parser b 
    pApply p f toks = [(f v1, toks1) | (v1, toks1) <- p toks]
 
    pGreetingsN :: Parser Int
    pGreetingsN = (pZeroOrMore pHelloOrGoodbye) `pApply` length
    -- test
    -- test7 :: [((String, String), [Token])]
    test7 =   [(1, "goodbye"), (1, "sep"), (1, "goodbye"), (1, "sep"),  (1, "goodbye"), (0, "James"), (2, "!")]

    pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
    pOneOrMoreWithSep p1 p2 =  pOneOrMore (pThen const p1 p2)

    test8 = pOneOrMoreWithSep (pLit "goodbye") (pLit "sep") $ test7

    pSat :: (String -> Bool) -> Parser String 
    pSat f ((_, tok') : toks)   | f tok'    = [(tok', toks)]
                                | otherwise = []
    pSat _ []                               = []

    pLit :: String -> Parser String 
    pLit s = pSat (==s)

    
    -- Parse aexpr
    keywords :: [String]
    keywords = ["let", "letrec", "case", "in", "of", "Pack"]

    isVariable :: String -> Bool
    isVariable tok@(c:cs) = (not $ tok `elem` keywords) && isAlpha c && all isIdChar cs

    pVar :: Parser String
    pVar = pSat isVariable

    isNum :: String -> Bool
    isNum = isDigit . head
    
    pNum :: Parser Int
    pNum =  pSat isNum `pApply` read 

    -- pConstr = pThen EConstr (pLit "Pack{" `pRight` pNum) (pNum `pLeft` pLit "}")
    
    -- pParanExpr = pLit "(" `pRight` pExpr `pLeft` pLit ")"

    -- pLet :: Parser (Expr )
    pAexpr :: Parser CoreExpr
    pAexpr = (pVar `pApply` EVar) `pAlt` (pNum `pApply` ENum)

    mk_sc :: Name -> [String] -> String -> CoreExpr -> CoreScDefn 
    mk_sc f vars eq expr | length eq == 0 = ("error", [], EVar eq)
                         | otherwise = (f, vars, expr)

    pSc :: Parser CoreScDefn
    pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr

    pProgram :: Parser CoreProgram
    pProgram = pOneOrMoreWithSep pSc (pLit ";")

    mk_ap_chain :: [CoreExpr] -> CoreExpr
    mk_ap_chain []          = EVar "error"
    mk_ap_chain [x]         = x
    mk_ap_chain (x1:x2:xs)  = let mk_ap_chain' chain (x:xs') = mk_ap_chain' (EAp chain x) xs'
                                  mk_ap_chain' chain []      = chain
                              in  mk_ap_chain' (EAp x1 x2) xs



    test10 = "f = 3;\nnss = 32;\ng x y = let z = x in z; \nh x = case (let y = x in y) of \n <1> -> 2; \n <2> -> 5;"
    test11 = clex 0 test10
    test12 = pProgram test11

    test13 = map (printCoreProgram.fst) test12

    test14 = (take 10 exprs) ++ [EVar "y"] where exprs = repeat (EAp (EVar "+")  (EVar "x"))
    test15 =  printExpr $ mk_ap_chain test14

    data PartialExpr = NoOp | FoundOp Name CoreExpr

    assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
    assembleOp e1 NoOp = e1
    assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

    pExpr :: Parser CoreExpr
    pExpr = pExpr1 
    
    pExpr1 :: Parser CoreExpr
    pExpr1 = pThen assembleOp pExpr2 pExpr1c
    
    pExpr1c :: Parser PartialExpr
    pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` pEmpty NoOp
    
    pExpr2 = pThen assembleOp pExpr3 pExpr2c
    pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` pEmpty NoOp
    
    pExpr3 = pThen assembleOp pExpr4 pExpr3c
    pExpr3c = (pThen FoundOp pRelops pExpr4) `pAlt` pEmpty NoOp

    relOps = twoCharOps ++ ["<", ">"]
    pRelops = pSat (`elem` relOps)
    
    pExpr4 = pThen assembleOp pExpr5 pExpr4c
    pExpr4c = (pThen FoundOp (pLit "+") pExpr4) `pAlt` (pThen FoundOp (pLit "-") pExpr5) `pAlt` (pEmpty NoOp)
    
    pExpr5 = pThen assembleOp pExpr6 pExpr5c
    pExpr5c = (pThen FoundOp (pLit "*") pExpr5) `pAlt` (pThen FoundOp (pLit "/") pExpr6) `pAlt` (pEmpty NoOp)
    
    pExpr6 = (pOneOrMore pAexpr) `pApply` mk_ap_chain
    



    