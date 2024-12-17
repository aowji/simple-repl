import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Functor
import Data.Map
import Data.Maybe (fromMaybe)
import Data.Void
import Debug.Trace
import System.Console.Haskeline
import System.Environment
import Test.Hspec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

-- syntax tree

type Name = String

data Expr
    = Integer Int
    | Float Double
    | Boolean Bool
    | Null
    | Char Char
    | Str String
    | List [Expr]
    | Error String
    | Var Name
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | BinOp BinOp Expr Expr
    | Asign Expr Expr
    | Block [Expr]
    deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Div deriving (Eq, Show)

-- parser

type Parser = Parsec Void String

pInteger :: Parser Expr
pInteger = Integer <$> lexeme (L.decimal <* notFollowedBy (char '.'))

pFloat :: Parser Expr
pFloat = Float <$> lexeme L.float

pBool :: Parser Expr
pBool = Boolean <$> lexeme (string "true" $> True <|> string "false" $> False)

pNull :: Parser Expr
pNull = Null <$ lexeme (string "null")

pChar :: Parser Expr
pChar = Char <$> lexeme (between (char '\'') (char '\'') L.charLiteral)

pStr :: Parser Expr
pStr = Str <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pList :: Parser Expr
pList = List <$> (symbol "[" *> sepBy val comma <* symbol "]")
  where
    val = try pInteger <|> pFloat <|> pBool <|> pChar <|> pStr

pVar :: Parser Expr = Var <$> lexeme pIdentifier

pCall :: Parser Expr = Call <$> pIdentifier <*> parens (sepBy pExpr (symbol ","))

pFunction :: Parser Expr
pFunction = Function <$> pIdentifier <*> parens (sepBy pVar (symbol ",")) <*> pExpr

pTerm =
    parens pExpr
        <|> try pFloat
        <|> pInteger
        <|> pBool
        <|> pNull
        <|> pChar
        <|> pStr
        <|> pList
        <|> try pFunction
        <|> try pCall
        <|> pVar

pOperator :: Parser Expr =
    makeExprParser pTerm operatorTable
  where
    operatorTable =
        [
            [ InfixL (BinOp Mul <$ symbol "*")
            , InfixL (BinOp Div <$ symbol "/")
            ]
        ,
            [ InfixL (BinOp Add <$ symbol "+")
            , InfixL (BinOp Sub <$ symbol "-")
            ]
        ]

pAsign :: Parser Expr = Asign <$> pVar <* symbol "=" <*> pOperator

pBlock :: Parser Expr = Block <$> braces (sepBy pExpr (symbol ";"))

pExpr = spaceConsumer *> (pBlock <|> try pAsign <|> pOperator)

parseExpr str = fromMaybe (Error "parse failed") (parseMaybe (pExpr <* eof) str)

-- lexer

spaceConsumer :: Parser ()
spaceConsumer = space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer :: String -> Parser String

parens = between (symbol "(") (symbol ")") :: Parser a -> Parser a

braces = between (symbol "{") (symbol "}")

angles = between (symbol "<") (symbol ">")

brackets = between (symbol "[") (symbol "]")

semicolon = symbol ";"

comma = symbol ","

colon = symbol ":"

dot = symbol "."

pIdentifier :: Parser String
pIdentifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

-- eval

isVar e = case e of
    Var _ -> True
    _ -> False

isValue e = case e of
    Integer _ -> True
    Float _ -> True
    Boolean _ -> True
    Char _ -> True
    Str _ -> True
    Null -> True
    List _ -> True
    _ -> False

add :: Expr -> Expr -> Expr
add (Integer a) (Integer b) = Integer (a + b)
add (Float a) (Float b) = Float (a + b)
add (Float a) (Integer b) = Float (a + fromIntegral b)
add (Integer a) (Float b) = Float (fromIntegral a + b)
add a b = Error ("can not add " ++ show a ++ " and " ++ show b)

sub :: Expr -> Expr -> Expr
sub (Integer a) (Integer b) = Integer (a - b)
sub (Float a) (Float b) = Float (a - b)
sub (Float a) (Integer b) = Float (a - fromIntegral b)
sub (Integer a) (Float b) = Float (fromIntegral a - b)
sub l r = Error ("can not sub " ++ show l ++ " and " ++ show r)

mul :: Expr -> Expr -> Expr
mul (Integer a) (Integer b) = Integer (a * b)
mul (Float a) (Float b) = Float (a * b)
mul (Float a) (Integer b) = Float (a * fromIntegral b)
mul (Integer a) (Float b) = Float (fromIntegral a * b)
mul l r = Error ("can not mul " ++ show l ++ " and " ++ show r)

div :: Expr -> Expr -> Expr
div (Integer a) (Integer b) = Integer (a `Prelude.div` b)
div (Float a) (Float b) = Float (a / b)
div (Float a) (Integer b) = Float (a / fromIntegral b)
div (Integer a) (Float b) = Float (fromIntegral a / b)
div l r = Error ("can not div " ++ show l ++ " and " ++ show r)

type EvalState = Map String Expr

eval' :: Expr -> State EvalState Expr
eval' (Asign (Var name) value) = do
    evaluatedValue <- eval' value
    modify (insert name evaluatedValue)
    return Null
eval' (Asign l r) = pure $ Error ("can not asign " ++ show l ++ " with " ++ show r)
eval' (BinOp op l r) = evalBinOp op <$> eval' l <*> eval' r
  where
    evalBinOp Add = add
    evalBinOp Sub = sub
    evalBinOp Mul = mul
    evalBinOp Div = Main.div
eval' (Var name) = gets (fromMaybe Null . Data.Map.lookup name)
eval' (Block [x]) = eval' x
eval' (Block (x : xs)) = eval' x *> eval' (Block xs)
eval' x = pure x

evalPartial :: Expr -> EvalState -> (Expr, EvalState)
evalPartial expr = runState (eval' expr)

eval :: Expr -> Expr
eval expr = evalState (eval' expr) (Data.Map.fromList [("a", Integer 2)])

-- repl

print val = case val of
    Integer x -> show x
    Boolean x -> show x
    Float x -> show x
    Char x -> show x
    Str x -> show x
    Null -> "null"
    List xs -> show xs
    Error msg -> "Error: " ++ msg
    Var name -> name
    Call name args -> name ++ "(" ++ unwords (Prelude.map Main.print args) ++ ")"
    Function name params body -> "Function " ++ name ++ "(" ++ unwords (Prelude.map Main.print params) ++ ") " ++ Main.print body
    BinOp op l r -> Main.print l ++ " " ++ show op ++ " " ++ Main.print r
    Asign l r -> Main.print l ++ " = " ++ Main.print r
    Block exprs -> "{ " ++ unwords (Prelude.map Main.print exprs) ++ " }"

repl = runInputT defaultSettings (loop Data.Map.empty)
  where
    loop s0 = do
        minput <- getInputLine "Repl> "
        case minput of
            Nothing -> return ()
            Just "exit" -> return ()
            Just "quit" -> return ()
            Just input -> do
                let expr = parseExpr input
                let (result, s1) = evalPartial expr s0
                outputStrLn $ Main.print result
                loop s1

-- unit tests

tests = hspec $ do
    describe "parse" $ do
        it "parse int literal" $ do
            parseMaybe pExpr "123" `shouldBe` Just (Integer 123)

        it "parse float literal" $ do
            parseMaybe pExpr "123.0" `shouldBe` Just (Float 123)

        it "parse bool literal" $ do
            parseMaybe pExpr "true" `shouldBe` Just (Boolean True)
            parseMaybe pExpr "false" `shouldBe` Just (Boolean False)

        it "parse char literal" $ do
            parseMaybe pExpr "'h'" `shouldBe` Just (Char 'h')

        it "parse string literal" $ do
            parseMaybe pExpr "\"hello\"" `shouldBe` Just (Str "hello")

        it "parse list literal" $ do
            parseMaybe pExpr "[ 1, 1.2, true, false, 'o', \"haha\" ]" `shouldBe` Just (List [Integer 1, Float 1.2, Boolean True, Boolean False, Char 'o', Str "haha"])

        it "parse operator" $ do
            parseExpr "a + b" `shouldBe` BinOp Add (Var "a") (Var "b")
            parseExpr "a - b" `shouldBe` BinOp Sub (Var "a") (Var "b")
            parseExpr "a * b" `shouldBe` BinOp Mul (Var "a") (Var "b")
            parseExpr "a / b" `shouldBe` BinOp Div (Var "a") (Var "b")
            parseExpr "a + b * c" `shouldBe` BinOp Add (Var "a") (BinOp Mul (Var "b") (Var "c"))
            parseExpr "a * (b + c)" `shouldBe` BinOp Mul (Var "a") (BinOp Add (Var "b") (Var "c"))
            parseExpr "1 * (2 + 3)" `shouldBe` BinOp Mul (Integer 1) (BinOp Add (Integer 2) (Integer 3))

        it "parse variable" $ do
            parseMaybe pExpr "a" `shouldBe` Just (Var "a")

        it "parse call" $ do
            parseMaybe pExpr "a(b, c)" `shouldBe` Just (Call "a" [Var "b", Var "c"])

        it "parse function" $ do
            parseMaybe pExpr "a(b, c) b" `shouldBe` Just (Function "a" [Var "b", Var "c"] (Var "b"))

        it "parse asign" $ do
            parseExpr "a = 1" `shouldBe` Asign (Var "a") (Integer 1)

        it "parse block" $ do
            parseExpr "{ a = 1; 2}" `shouldBe` Block [Asign (Var "a") (Integer 1), Integer 2]

        it "eval" $ do
            eval (parseExpr "1 + 2") `shouldBe` Integer 3
            eval (parseExpr "1 + 2 + 3 - 1") `shouldBe` Integer 5
            eval (parseExpr "1 + 2 + 3 * 2") `shouldBe` Integer 9
            eval (parseExpr "2*(3-1)") `shouldBe` Integer 4
            eval (parseExpr "{ a=1; 2}") `shouldBe` Integer 2
            eval (parseExpr "{ a=1;a+2}") `shouldBe` Integer 3

        it "eval block" $ do
            eval (parseExpr "{ a=1; b = 3; c=a+b; c*2}") `shouldBe` Integer 8

main = do
    args <- getArgs
    case args of
        [] -> repl
        "--tests":xs -> withArgs xs $ tests
        args | "--repl" `elem` args -> repl
        _ -> Prelude.print "wrong args"
