{
-- |
-- Module      :  Language.LLVM.Parser.Lexer
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
--                (c) Drexel University 2013
--                (c) Timo von Holtz 2014
-- License     :  BSD-style
-- Maintainer  :  tvh@tvholtz.de

{-# OPTIONS_GHC -w #-}

module LLVM.Quote.Parser.Lexer (
    lexToken
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Char
import Data.Loc
import Data.Ratio ((%))
import Text.PrettyPrint.Mainland

import LLVM.Quote.Parser.Tokens
import LLVM.Quote.Parser.Monad
}

$nondigit         = [a-z A-Z \_ \.]
$digit            = [0-9]
$nonzerodigit     = [1-9]
$octalDigit       = [0-7]
$hexadecimalDigit = [0-9A-Fa-f]
$whitechar = [\ \t\n\r\f\v]

@fractionalConstant = $digit* "." $digit+
                    | $digit+ "."
@exponentPart       = [eE] [\+\-]? $digit+

@floatingConstant   = @fractionalConstant @exponentPart?
                    | $digit+ @exponentPart

@decimalConstant     = $nonzerodigit $digit* | "0"
@octalConstant       = "0" $octalDigit*
@hexadecimalConstant = "0" [xX] $hexadecimalDigit+

@idText = [a-z A-Z \_ \. \-] [a-z A-Z \_ \. \- 0-9]*
@identifier = [@\%\!] ( @decimalConstant
                      | @idText)
@jumpLabel = @idText ":"

@integerType = "i" $nonzerodigit $digit*
@keyword = [a-z \_]+ ($nonzerodigit $digit*)?

tokens :-

<0> {
 "$dl:"           / { allowAnti } { lexAnti Tanti_dl }
 "$dlM:"           / { allowAnti } { lexAntiM Tanti_dl }
 "$tt:"           / { allowAnti } { lexAnti Tanti_tt }
 "$ttM:"           / { allowAnti } { lexAntiM Tanti_tt }
 "$def:"          / { allowAnti } { lexAnti Tanti_def }
 "$defM:"          / { allowAnti } { lexAntiM Tanti_def }
 "$defs:"         / { allowAnti } { lexAnti Tanti_defs }
 "$defsM:"         / { allowAnti } { lexAntiM Tanti_defs }
 "$bb:"           / { allowAnti } { lexAnti Tanti_bb }
 "$bbM:"           / { allowAnti } { lexAntiM Tanti_bb }
 "$bbs:"          / { allowAnti } { lexAnti Tanti_bbs }
 "$bbsM:"          / { allowAnti } { lexAntiM Tanti_bbs }
 "$instr:"        / { allowAnti } { lexAnti Tanti_instr }
 "$instrM:"        / { allowAnti } { lexAntiM Tanti_instr }
 "$instrs:"       / { allowAnti } { lexAnti Tanti_instrs }
 "$instrsM:"       / { allowAnti } { lexAntiM Tanti_instrs }
 "$type:"         / { allowAnti } { lexAnti Tanti_type }
 "$typeM:"         / { allowAnti } { lexAntiM Tanti_type }
 "$opr:"          / { allowAnti } { lexAnti Tanti_opr }
 "$oprM:"          / { allowAnti } { lexAntiM Tanti_opr }
 "$const:"        / { allowAnti } { lexAnti Tanti_const }
 "$constM:"        / { allowAnti } { lexAntiM Tanti_const }
 "$id:"           / { allowAnti } { lexAnti Tanti_id }
 "$idM:"           / { allowAnti } { lexAntiM Tanti_id }
 "$gid:"          / { allowAnti } { lexAnti Tanti_gid }
 "$gidM:"          / { allowAnti } { lexAntiM Tanti_gid }
 "$param:"        / { allowAnti } { lexAnti Tanti_param }
 "$paramM:"        / { allowAnti } { lexAntiM Tanti_param }
 "$params:"       / { allowAnti } { lexAnti Tanti_params }
 "$paramsM:"       / { allowAnti } { lexAntiM Tanti_params }
}

<0> {
 ";" .* ;
 $whitechar+          ;
 
 c \" { lexcStringTok }

 @identifier { identifier }
 @jumpLabel { jumpLabel }
 @integerType { numberedToken TintegerType }
 @keyword { keyword }

 @floatingConstant                    { lexFloat }
 @decimalConstant                     { lexInteger 0 decimal }
 @octalConstant                       { lexInteger 1 octal }
 @hexadecimalConstant                 { lexInteger 2 hexadecimal }

 \" { lexStringTok }
 
 "("   { token Tlparen }
 ")"   { token Trparen }
 "["   { token Tlbrack }
 "]"   { token Trbrack }
 "{"   { token Tlbrace }
 "}"   { token Trbrace }
 "<"   { token Tlt }
 ">"   { token Tgt }
 ","   { token Tcomma }
 "*"   { token Tstar }
 "="   { token Tassign }
 "-"   { token Tminus }
 "!"   { token Tbang }
 "..." { token Tpoints }
}

{
type Action = AlexInput -> AlexInput -> P (L Token)

inputString :: AlexInput -> AlexInput -> String
inputString beg end =
  (B.unpack . B.take (alexOff end - alexOff beg)) (alexInput beg)

locateTok :: AlexInput -> AlexInput -> Token -> L Token
locateTok beg end tok =
    L (Loc (alexPos beg) (alexPos end)) tok

token :: Token -> Action
token tok beg end =
    return $ locateTok beg end tok

identifier :: Action
identifier beg end = do
    v <- case head ident of
      '%' -> return Local
      '@' -> return Global
      '!' -> return Meta
    case isDigit $ head $ tail ident of
      False -> return $ locateTok beg end $ Tnamed v (tail ident)
      True  -> return $ locateTok beg end $ Tunnamed v (read $ tail ident)
  where
    ident :: String
    ident = inputString beg end

jumpLabel :: Action
jumpLabel beg end = do
    token (TjumpLabel $ init ident) beg end
  where
    ident :: String
    ident = inputString beg end

numberedToken :: (Num a, Read a) => (a -> Token) -> Action
numberedToken f beg end = do
    return $ locateTok beg end $ f (read $ tail ident)
  where
    ident :: String
    ident = inputString beg end

keyword :: Action
keyword beg end = do
    case Map.lookup ident keywordMap of
      Nothing             -> identError
      Just (tok, Nothing) -> token tok beg end
      Just (tok, Just i)  -> do isKw <- useExts i
                                if isKw then token tok beg end else identError
  where
    ident :: String
    ident = inputString beg end

    identError = fail $ "not a valid keyword: " ++ show ident

lexStringTok :: Action
lexStringTok beg _ = do
    s    <- lexString ""
    end  <- getInput
    return $ locateTok beg end (TstringConst s)
  where
    lexString :: String -> P String
    lexString s = do
        c <- nextChar
        case c of
          '"'  -> return (reverse s)
          '\\' -> do  c' <- lexCharEscape
                      lexString (c' : s)
          _    -> lexString (c : s)

lexAnti :: (String -> Token) -> Action
lexAnti antiTok = lexAntiM (antiTok . \s -> "return (" ++ s ++ ")")

lexAntiM :: (String -> Token) -> Action
lexAntiM antiTok beg end = do
    c <- nextChar
    s <- case c of
           '('                 -> lexExpression 0 ""
           _ | isIdStartChar c -> lexIdChars [c]
             | otherwise       -> lexerError beg (text "illegal anitquotation")
    return $ locateTok beg end (antiTok s)
  where
    lexIdChars :: String -> P String
    lexIdChars s = do
        maybe_c <- maybePeekChar
        case maybe_c of
          Just c | isIdChar c -> skipChar >> lexIdChars (c : s)
          _                   -> return (reverse s)

    lexExpression :: Int -> String -> P String
    lexExpression depth s = do
        maybe_c <- maybePeekChar
        case maybe_c of
          Nothing               -> do end' <- getInput
                                      parserError (Loc (alexPos beg) (alexPos end'))
                                                  (text "unterminated antiquotation")
          Just '('              -> skipChar >> lexExpression (depth+1) ('(' : s)
          Just ')' | depth == 0 -> skipChar >> return (unescape (reverse s))
                   | otherwise  -> skipChar >> lexExpression (depth-1) (')' : s)
          Just c                -> skipChar >> lexExpression depth (c : s)
      where
        unescape :: String -> String
        unescape ('\\':'|':'\\':']':s')  = '|' : ']' : unescape s'
        unescape (c:s')                  = c : unescape s'
        unescape []                     = []

    isIdStartChar :: Char -> Bool
    isIdStartChar '_' = True
    isIdStartChar c   = isLower c

    isIdChar :: Char -> Bool
    isIdChar '_'  = True
    isIdChar '\'' = True
    isIdChar c    = isAlphaNum c

lexCharEscape :: P Char
lexCharEscape = do
    cur  <- getInput
    c    <- nextChar
    case c of
      'a'  -> return '\a'
      'b'  -> return '\b'
      'f'  -> return '\f'
      'n'  -> return '\n'
      'r'  -> return '\r'
      't'  -> return '\t'
      'v'  -> return '\v'
      '\\' -> return '\\'
      '\'' -> return '\''
      '"'  -> return '"'
      '?'  -> return '?'
      'x'  -> chr <$> checkedReadNum isHexDigit 16 hexDigit
      n | isOctDigit n -> setInput cur >> chr <$> checkedReadNum isOctDigit 8 octDigit
      _c -> return c

lexcStringTok :: Action
lexcStringTok beg _ = do
    s    <- lexString ""
    end  <- getInput
    return $ locateTok beg end (TcstringConst s)
  where
    lexString :: String -> P String
    lexString s = do
        c <- nextChar
        case c of
          '"'  -> return (reverse s)
          '\\' -> do  c' <- lexCharHexEscape
                      lexString (c' : s)
          _    -> lexString (c : s)
      
lexCharHexEscape :: P Char
lexCharHexEscape = do
    c    <- nextChar
    case c of
      '\\' -> return '\\'
      '"'  -> return '"'
      n | isHexDigit n ->
        do
            cur <- getInput
            c' <- nextChar
            if isHexDigit c'
            then return . chr $ hexDigit c * 16 + hexDigit c'
            else do
                setInput cur
                return . chr $ hexDigit c 
      _c -> return c
      
lexInteger :: Int -> Radix -> Action
lexInteger ndrop radix@(_, isRadixDigit, _) beg end =
    case i of
      [n] -> return $ locateTok beg end (toToken n)
      _   -> fail "bad parse for integer"
  where
    num :: String
    num = (takeWhile isRadixDigit . drop ndrop)  s

    s :: String
    s = inputString beg end

    i :: [Integer]
    i = do  (n, _) <- readInteger radix num
            return n

    toToken :: Integer -> Token
    toToken n = TintConst n

lexFloat :: Action
lexFloat beg end =
    case i of
      [n] -> token (toToken n) beg end
      _   -> fail "bad parse for integer"
  where
    s :: String
    s = inputString beg end

    i :: [Rational]
    i = do  (n, _) <- readRational s
            return n

    toToken :: Rational -> Token
    toToken n = TfloatConst n

type Radix = (Integer, Char -> Bool, Char -> Int)

decDigit :: Char -> Int
decDigit c  | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in decimal constant"

octDigit :: Char -> Int
octDigit c  | c >= '0' && c <= '7' = ord c - ord '0'
            | otherwise            = error "error in octal constant"

hexDigit :: Char -> Int
hexDigit c  | c >= 'a' && c <= 'f' = 10 + ord c - ord 'a'
            | c >= 'A' && c <= 'F' = 10 + ord c - ord 'A'
            | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in hexadecimal constant"

decimal :: Radix
decimal = (10, isDigit, decDigit)

octal :: Radix
octal = (8, isOctDigit, octDigit)

hexadecimal :: Radix
hexadecimal = (16, isHexDigit, hexDigit)

readInteger :: Radix -> ReadS Integer
readInteger (radix, isRadixDigit, charToInt) =
    go 0
  where
    go :: Integer -> ReadS Integer
    go  x  []             = return (x, "")
    go  x  (c : cs)
        | isRadixDigit c  = go (x * radix + toInteger (charToInt c)) cs
        | otherwise       = return (x, c : cs)

readDecimal :: ReadS Integer
readDecimal = readInteger decimal

readRational :: ReadS Rational
readRational s = do
    (n, d, t)  <- readFix
    (x, t')     <- readExponent t
    return ((n % 1) * 10^^(x - toInteger d), t')
  where
    readFix :: [(Integer, Int, String)]
    readFix =
        return (read (i ++ f), length f, u)
      where
        (i, t) = span isDigit s
        (f, u) = case t of
                   '.' : u' -> span isDigit u'
                   _        -> ("", t)

    readExponent :: ReadS Integer
    readExponent ""                        = return (0, "")
    readExponent (e : s') | e `elem` "eE"  = go s'
                          | otherwise      = return (0, s')
      where
        go :: ReadS Integer
        go  ('+' : s'')  = readDecimal s''
        go  ('-' : s'')  = do (x, t) <- readDecimal s''
                              return (-x, t)
        go  s''          = readDecimal s''

checkedReadNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
checkedReadNum isDigit' base conv = do
    cur  <- getInput
    c    <- peekChar
    when (not $ isDigit' c) $
       illegalNumericalLiteral cur
    readNum isDigit base conv

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
readNum isDigit' base conv =
    readI 0
  where
    readI :: Int -> P Int
    readI n = do
        c <- peekChar
        if isDigit' c
          then do  let n' = n*base + conv c
                   n' `seq` skipChar >> readI n'
          else return n

lexToken :: P (L Token)
lexToken = do
    beg  <- getInput
    sc   <- getLexState
    st   <- get
    case alexScanUser st beg sc of
      AlexEOF              -> token Teof beg beg
      AlexError end        -> lexerError end (text rest)
                                where
                                  rest :: String
                                  rest = B.unpack $ B.take 80 (alexInput beg)
      AlexSkip end _       -> setInput end >> lexToken
      AlexToken end _len t  -> setInput end >> t beg end
}
