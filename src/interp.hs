module Main where
import Control.Monad.Error
import Data.IORef
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

-- ==========================================================================
-- -------------------------------- PARSER ----------------------------------
-- ==========================================================================

data LispVal = Undefined
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | Float Double
             | String String
             | Character Char
             | Bool Bool
             | Port Handle
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { body    :: [LispVal],
                      params  :: [String],
                      vararg  :: Maybe String,
                      closure :: Env }

hash      = char '#'
quote     = char '"'
backslash = char '\\'

escapedChar = do
  backslash
  c <- oneOf "nrt\"\\"
  return $ case c of
             'n'  -> "\n"
             'r'  -> "\r"
             't'  -> "\t"
             '"'  -> [c]
             '\\' -> [c]
 
parseString =
  between quote quote (many $ many1 (noneOf "\"\\") <|> escapedChar) >>=
  return . String . concat

symbol  = oneOf "!$%&|*+-/:<=?>@~_^#"
bracket = oneOf "(){}[]"

normalChar = digit    <|>
             quote    <|>
             space    <|>
             letter   <|>
             symbol   <|>
             bracket  <|>
             backslash
 
specialChar = do
  s <- string "space" <|> string "newline"
  return $ case s of
             "space"   -> ' '
             "newline" -> '\n'
 
parseCharacter = hash >> backslash >>
                 (specialChar <|> normalChar) >>=
                 (return . Character)

parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             ____ -> Atom atom

parseFloat = float >>= (return . Float . read)

parseNegFloat = char '-' >> float >>= (return . Float . read . (++) "-")

float = do
  intPart <- integer
  char '.'
  floatPart <- integer
  return (intPart ++ "." ++ floatPart)

parseInteger = integer >>= (return . Integer . read)

parseNegInt = char '-' >> integer >>= (return . Integer . read . (++) "-")

integer = many1 digit

spaces :: Parser ()
spaces = skipMany1 space

parseList = sepBy parseExpr spaces >>= (return . List)

parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseAnyList =
  between (char '(') (char ')') (try parseList <|> parseDottedList) >>= return

parseQuoted = do
  char '\''
  expr <- parseExpr
  (return . List) [Atom "quote", expr]

parseUndefined = eof >> return Undefined

parseExpr :: Parser LispVal
parseExpr =
  try parseCharacter <|>
  parseString        <|>
  try parseFloat     <|>
  try parseNegFloat  <|>
  parseInteger       <|>
  try parseNegInt    <|>
  parseAtom          <|>
  parseQuoted        <|>
  parseUndefined     <|>
  parseAnyList

-- ==========================================================================
-- ------------------------------- Evaluator --------------------------------
-- ==========================================================================

showLispVal :: LispVal -> String
showLispVal Undefined     = ""
showLispVal (Float f)     = show f
showLispVal (Character c) = show c
showLispVal (String s)    = show s
showLispVal (Integer n)    = show n
showLispVal (Atom name)   = name
showLispVal (Bool True)   = "#t"
showLispVal (Bool False)  = "#f"
showLispVal (Port _)      = "<IO port>"
showLispVal (IOFunc _)    = "<IO primitive>"

showLispVal (List xs)  = "(" ++ showLispList xs ++ ")"
showLispVal (DottedList head tail) =
  "(" ++ showLispList head ++ " . " ++ showLispVal tail ++ ")"

showLispVal (PrimitiveFunc _) = "<primitive function>"
showLispVal (Func _ params varargs _) =
  "(lambda (" ++ unwords params ++
    (case varargs of
       Nothing  -> ""
       Just arg -> " . " ++ arg) ++ ") ...)"

showLispList :: [LispVal] -> String
showLispList = unwords . map showLispVal

instance Show LispVal where show = showLispVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Undefined)   = return val
eval env val@(Bool _)      = return val
eval env val@(Float _)     = return val
eval env val@(String _)    = return val
eval env val@(Integer _)    = return val
eval env val@(Character _) = return val

eval env (Atom name) = getVar env name

eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", pred, success, failure]) = do
  result <- eval env pred
  eval env $ case result of
               Bool False -> failure
               otherwise  -> success

eval env (List [Atom "cond"]) = eval env Undefined
eval env (List [Atom "cond",  List [Atom "else", expr]]) = eval env expr
eval env (List (Atom "cond" : List [pred, expr] : rest)) = do
  result <- eval env pred
  eval env $ case result of
               Bool False -> List (Atom "cond" : rest)
               otherwise  -> expr

eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var

eval env (List [Atom "load", String path]) =
  load path >>= liftM last . mapM (eval env)

eval env (List (Atom "define" : List (Atom name : params) : body)) =
  makeNormalFunc env params body >>= defineVar env name

eval env (List (Atom "define" : DottedList (Atom name : params) varargs :
                body)) =
                makeVararg varargs env params body >>= defineVar env name

eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVararg varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVararg varargs env [] body
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

eval env badForm = throwError $ BadSpecialForm "Bad syntax" badForm

makeNormalFunc = makeFunc Nothing
makeVararg = makeFunc . Just . showLispVal
makeFunc varargs env params body =
  return $ Func body (map showLispVal params) varargs env

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = (liftThrows . func) args
apply (IOFunc func) args = func args
apply (Func body params varargs closure) args =
  if len params /= len args && varargs == Nothing
    then throwError $ NumArgs (len params) args
    else (liftIO $ bindVars closure $ zip params args) >>=
         bindVarArgs varargs >>= evalBody
  where len = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
                 Nothing   -> return env
                 Just name -> liftIO $ bindVars env [(name, List remainingArgs)]
        remainingArgs = drop (length params) args

apply notFunction _ = throwError $ NotFunction notFunction

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("boolean?", isBool),
               ("pair?", isPair),
               ("symbol?", isSymbol),
               ("number?", isNumber),
               ("char?", isChar),
               ("string?", isString),
               ("port?", isPort),
               ("procedure?", isProcedure),
               ("integer?", isInteger),
               ("+", addMany),
               ("-", subMany),
               ("*", mulMany),
               ("/", divideMany),
               ("sqrt", sqrtLisp),
               ("expt", exptLisp),
               ("floor", intUnop floor),
               ("ceiling", intUnop ceiling),
               ("round", intUnop round),
               ("quotient", integerBinop quot),
               ("mod", integerBinop mod),
               ("remainder", integerBinop rem),
               ("gcd", integerBinop gcd),
               ("lcm", integerBinop lcm),
               ("exp", floatUnop exp),
               ("log", floatUnop log),
               ("sin", floatUnop sin),
               ("cos", floatUnop cos),
               ("tan", floatUnop tan),
               ("asin", floatUnop asin),
               ("acos", floatUnop acos),
               ("atan", floatUnop atan),
               ("and", lispAndOr and),
               ("or",  lispAndOr or),
               ("=", numBoolBinop (==)),
               ("<", numBoolBinop (<)),
               (">", numBoolBinop (>)),
               ("<=", numBoolBinop (<=)),
               (">=", numBoolBinop (>=)),
               ("string=?", strBoolBinop (==)),
               ("string>?", strBoolBinop (>)),
               ("string<?", strBoolBinop (<)),
               ("string<=?", strBoolBinop (<=)),
               ("string>=?", strBoolBinop (>=)),
               ("car", car),
               ("cdr", cdr),
               ("cons", cons),
               ("eqv?", eqv),
               ("append", append),
               ("concat", cat),
               ("range", range) ]

isString [String _] = (return . Bool) True
isString [_] = (return . Bool) False
isString (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isChar [Character _] = (return . Bool) True
isChar [_] = (return . Bool) False
isChar (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isSymbol [Atom _] = (return . Bool) True
isSymbol [_] = (return . Bool) False
isSymbol (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isPair [List _] = (return . Bool) True
isPair [DottedList _ _] = (return . Bool) True
isPair [_] = (return . Bool) False
isPair (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isBool [Bool _] = (return . Bool) True
isBool [_] = (return . Bool) False
isBool (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isNumber [Integer _] = (return . Bool) True
isNumber [Float _] = (return . Bool) True
isNumber [_] = (return . Bool) False
isNumber (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isInteger [Integer _] = (return . Bool) True
isInteger [_] = (return . Bool) False
isInteger (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isPort [Port _] = (return . Bool) True
isPort [_] = (return . Bool) False
isPort (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

isProcedure [Func _ _ _ _] = (return . Bool) True
isProcedure [IOFunc _] = (return . Bool) True
isProcedure [PrimitiveFunc _] = (return . Bool) True
isProcedure [_] = (return . Bool) False
isProcedure (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)

numBinop op (Integer m) (Integer n) =
  numBinop op ((Float . fromIntegral) m) ((Float . fromIntegral) n)
numBinop op (Integer m) y@(Float _) =
  numBinop op ((Float . fromIntegral) m) y
numBinop op x@(Float _) (Integer n) =
  numBinop op x ((Float . fromIntegral) n)
numBinop op (Float x) (Float y) = (demote . Float) (x `op` y)

demote (Float f) = if (fromIntegral . floor) f == f
                     then (Integer . floor) f
                     else Float f

addMany    = foldOrThrow (numBinop (+))
mulMany    = foldOrThrow (numBinop (*))
subMany    = foldOrThrow (numBinop (-))
divideMany = foldOrThrow (numBinop (/))

foldOrThrow f args = if length args <= 1
                       then throwError $ NumArgs 2 args
                       else return $ foldl1 f args

exptLisp [Integer m, Integer n] = (return . Integer) (m ^ n)
exptLisp [Integer m, y@(Float _)] = exptLisp [(Float (fromIntegral m)), y]
exptLisp [x@(Float _), Integer n] = exptLisp [x, (Float (fromIntegral n))]
exptLisp [Float x, Float y] = (return . Float) (x ** y)
exptLisp singleVal@[_] = throwError $ NumArgs 2 singleVal
exptLisp tooMany@(a:b:c:rest)  = throwError $ NumArgs 2 tooMany
exptLisp (a:b:rest) = throwTypeError " numbers" (List (a:b:rest))

sqrtLisp [Integer n] = sqrtLisp [Float (fromIntegral n)]
sqrtLisp [Float x] = if x < 0
                       then throwError $ Default "No complex numbers yet!"
                       else (return . Float) (sqrt x)
sqrtLisp (a:b:rest) = throwError $ NumArgs 1 (a:b:rest)
sqrtLisp [notNum] = throwTypeError " a number" notNum

floatUnop op [Integer n] = floatUnop op [Float (fromIntegral n)]
floatUnop op [Float x] = (return . demote . Float . op) x
floatUnop op [notNum]  = throwTypeError " a number" notNum

intUnop op [Integer n] = intUnop op [Float (fromIntegral n)]
intUnop op [Float x] = (return . Integer . op) x
intUnop op [notNum]  = throwTypeError " a number" notNum

lispAndOr :: ([Bool] -> Bool) -> [LispVal] -> ThrowsError LispVal
lispAndOr f (a:b:rest) = (return . Bool . f . map toBool) (a:b:rest)
lispAndOr f badArgs = throwError $ NumArgs 2 badArgs

integerBinop  = regularBinop unpackInt Integer

numBoolBinop  = regularBinop unpackNum Bool
strBoolBinop  = regularBinop unpackStr Bool

regularBinop :: (LispVal -> ThrowsError a) -> (b -> LispVal) ->
                (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
regularBinop unpacker constructor op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do left  <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            (return . constructor) (left `op` right)

toBool :: LispVal -> Bool
toBool (Bool False) = False
toBool _ = True

unpackNum :: LispVal -> ThrowsError Double
unpackNum (Integer n) = (unpackNum . Float . fromIntegral) n
unpackNum (Float x) = return x
unpackNum notNum = throwTypeError "number" notNum

unpackInt :: LispVal -> ThrowsError Integer
unpackInt (Integer n) = return n
unpackInt notInteger = throwTypeError "integer" notInteger

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwTypeError "string" notString

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwTypeError "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = (return . List) xs
cdr [DottedList [_] y] = return y
cdr [DottedList (x:xs) y] = return $ DottedList xs y
cdr [badArg] = throwTypeError "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = (return . List) (x:xs)
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

append :: [LispVal] -> ThrowsError LispVal
append [List xs, y] = (return . List) (xs ++ [y])
append [notList, _] = throwTypeError "list" notList

cat :: [LispVal] -> ThrowsError LispVal
cat [List l1, List l2] = (return . List) (l1 ++ l2)
cat [notList, List _] = throwTypeError "list" notList
cat [List _, notList] = throwTypeError "list" notList

throwTypeError expected found = throwError $ TypeError expected found

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Atom x), (Atom y)]           = (return . Bool) (x == y)
eqv [(Bool x), (Bool y)]           = (return . Bool) (x == y)
eqv [(Float x), (Float y)]         = (return . Bool) (x == y)
eqv [(Integer x), (Integer y)]     = (return . Bool) (x == y)
eqv [(String x), (String y)]       = (return . Bool) (x == y)
eqv [(Character x), (Character y)] = (return . Bool) (x == y)
eqv [(DottedList xs x), (DottedList ys y)] = eqv [l1, l2]
  where l1 = List (xs ++ [x])
        l2 = List (ys ++ [y])
eqv [(List x), (List y)] = (return . Bool) $
  (length x == length y) && (and $ map eqvPair (zip x y))
  where eqvPair (p, q) = case eqv [p, q] of
                           Left err -> False
                           Right (Bool val) -> val
eqv [_, _] = (return . Bool) False
eqv badArgList = throwError $ NumArgs 2 badArgList

range :: [LispVal] -> ThrowsError LispVal
range [Integer n] = (return . List . map Integer) [0..n-1]
range [Integer start, Integer end] =
  (return . List . map Integer) [start..end - 1]
range [Integer start, Integer end, Integer step] =
  (return . List . map Integer) [start, start + step..end - 1]
range (a:b:c:d:rest) = throwError $ NumArgs 3 (a:b:c:d:rest)
range badArgs =
  throwError $ Default "Possible type error. Expecting 1 or more integers."

-- ==========================================================================
-- ----------------------------- Exceptions ---------------------------------
-- ==========================================================================

data LispError = NumArgs Integer [LispVal]
               | TypeError String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction LispVal
               | UnboundVar String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) =
  "Expected " ++ m ++ " arguments. Found " ++ n ++ ": " ++ f where
    m = show expected
    n = (show . length) found
    f = showLispList found
showError (TypeError expected found) =
  "Invalid type. Expected " ++ expected ++ ", found " ++ show found
showError (Parser err) = "Parse error: " ++ show err
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction func) = show func ++ " is not a function"
showError (UnboundVar varName) = varName ++ " is not defined"
showError (Default string) = string

instance Show LispError where show = showError

instance Error LispError where
  noMsg  = Default "You fucked up big"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- ==========================================================================
-- ---------------------------------- STATE ---------------------------------
-- ==========================================================================

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . (maybe False (const True)) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- (liftIO . readIORef) envRef
  maybe (throwError $ UnboundVar var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef name val = do
  env <- (liftIO . readIORef) envRef
  maybe (throwError $ UnboundVar name)
        (liftIO . (flip writeIORef val))
        (lookup name env)
  return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef name val = do
  isDefined <- liftIO $ isBound envRef name
  if isDefined
    then setVar envRef name val >> return val
    else liftIO $ do
      valRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef ((name, valRef) : env)
      return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (name, val) = do valRef <- newIORef val
                                    return (name, valRef)

primitiveBindings :: IO Env
primitiveBindings = newEnv >>= (flip bindVars $
                                map (makeFunc PrimitiveFunc) primitives ++
                                map (makeFunc IOFunc) ioPrimitives)
  where makeFunc constructor (name, func) = (name, constructor func)

newEnv :: IO Env
newEnv = newIORef []

-- ==========================================================================
-- ---------------------------------- REPL ----------------------------------
-- ==========================================================================

greeting = "(yet-another (yet-another (interpreter scheme)))\n"

main = do
  putStrLn greeting
  args <- getArgs
  if null args then primitiveBindings >>= loop else runFile args

loop env = r >>= e >>= p >> l where r = readPrompt "  "
                                    e = evalString env
                                    p = putStrLn
                                    l = loop env

runFile :: [String] -> IO ()
runFile args = do
  env <- primitiveBindings >>= flip bindVars [("--args",
                                              (List . map String . tail) args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
  loop env

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows . readExpr) expr >>= eval env

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "scheme" input of
                             Left  err -> throwError $ Parser err
                             Right val -> return val

readExpr  = readOrThrow parseExpr
readExprs = readOrThrow (endBy parseExpr spaces)

-- ==========================================================================
-- ----------------------------------- IO -----------------------------------
-- ==========================================================================

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc),
                 ("open-input-file", makePort ReadMode),
                 ("open-output-file", makePort WriteMode),
                 ("close-input-port", closePort),
                 ("close-output-port", closePort),
                 ("read", readProc),
                 ("write", writeProc),
                 ("read-contents", readContents),
                 ("read-all", readAll) ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String path] = liftM Port $ liftIO $ openFile path mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = (liftIO . hClose) port >> (return . Bool) True
closePort _ = (return . Bool) False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO . hGetLine) port >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [stuff] = writeProc [stuff, Port stdout]
writeProc [stuff, Port p] = liftIO $ hPrint p stuff >> (return . Bool) True

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String path] = (liftM String . liftIO . readFile) path

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String path] = (liftM List . load) path

load :: String -> IOThrowsError [LispVal]
load path = liftIO (readFile path) >>= liftThrows . readExprs

