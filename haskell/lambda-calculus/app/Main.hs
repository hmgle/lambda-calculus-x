module Main where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isSpace, toLower)
import Data.List (intercalate, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lib
    ( CalculusError(..)
    , Expr(..)
    , Strategy(..)
    , defaultLimit
    , formatResult
    , normalizeWithStrategyLimit
    , parseExpr
    , prettyExpr
    , renderCalculusError
    , renderParserError
    )
import qualified Lambda.Calculus.Combinators as Comb
import System.Environment (getArgs)
import System.IO (hFlush, hPutStrLn, isEOF, stdout, stderr)
import Text.Read (readMaybe)

data RunConfig = RunConfig
    { cfgLimit    :: Int
    , cfgStrategy :: Strategy
    , cfgEnv      :: Map String Expr
    }

main :: IO ()
main = do
    args <- getArgs
    if "--help" `elem` args
        then putStrLn usage
        else case parseArgs args of
            Left err -> hPutStrLn stderr err
            Right (config, Nothing) -> repl config
            Right (config, Just expr) -> runOnce config expr
  where
    usage = unlines
        [ "lambda-calculus-exe [--max-steps N] [--strategy normal|applicative] [EXPR]"
        , "Run EXPR through the evaluator or start a REPL when EXPR is omitted."
        , "REPL commands: :quit, :set steps N, :set strategy normal|applicative, :show, :let name = expr"
        ]

runOnce :: RunConfig -> String -> IO ()
runOnce config expr =
    case evaluateWithConfig config expr of
        Left err  -> hPutStrLn stderr err
        Right out -> putStrLn out

repl :: RunConfig -> IO ()
repl config0 = do
    putStrLn "Enter lambda expressions. Type :quit to exit."
    loop config0
  where
    loop config = do
        putStr "\\> "
        hFlush stdout
        eof <- isEOF
        if eof
            then putStrLn ""
            else do
                line <- getLine
                let trimmed = trimStart line
                case () of
                    _ | null trimmed -> loop config
                      | Just rest <- stripPrefix ":let" trimmed ->
                          case handleLet config rest of
                              Left err -> putStrLn ("Error: " ++ err) >> loop config
                              Right (config', pretty) -> putStrLn pretty >> loop config'
                      | otherwise ->
                          case words trimmed of
                              [":quit"] -> putStrLn "Bye."
                              [":show"] -> putStrLn (describeConfig config) >> loop config
                              [":set", "steps", n] ->
                                  case readMaybe n of
                                      Nothing     -> putStrLn "Invalid step count." >> loop config
                                      Just limit' -> putStrLn ("Max steps set to " ++ show limit' ++ ".")
                                                       >> loop config { cfgLimit = limit' }
                              [":set", "strategy", name] ->
                                  case parseStrategy name of
                                      Left err -> putStrLn err >> loop config
                                      Right strategy' ->
                                          putStrLn ("Strategy set to " ++ show strategy' ++ ".")
                                          >> loop config { cfgStrategy = strategy' }
                              _ ->
                                  case evaluateWithConfig config trimmed of
                                      Left err  -> putStrLn ("Error: " ++ err) >> loop config
                                      Right out -> putStrLn out >> loop config

parseArgs :: [String] -> Either String (RunConfig, Maybe String)
parseArgs = go defaultConfig Nothing
  where
    defaultConfig = RunConfig
        { cfgLimit = defaultLimit
        , cfgStrategy = NormalOrder
        , cfgEnv = preludeEnv
        }

    go config expr [] = Right (config, expr)
    go config expr ("--max-steps":n:rest) =
        case readMaybe n of
            Nothing -> Left "--max-steps expects an integer"
            Just m  -> go (config { cfgLimit = m }) expr rest
    go config expr ("--strategy":name:rest) =
        case parseStrategy name of
            Left err       -> Left err
            Right strategy -> go (config { cfgStrategy = strategy }) expr rest
    go config Nothing args = Right (config, Just (unwords args))
    go config expr args = Right (config, expr <|> Just (unwords args))

parseStrategy :: String -> Either String Strategy
parseStrategy raw =
    case map toLower raw of
        "normal"           -> Right NormalOrder
        "normal-order"     -> Right NormalOrder
        "applicative"      -> Right ApplicativeOrder
        "applicative-order"-> Right ApplicativeOrder
        "cbv"              -> Right ApplicativeOrder
        "cbn"              -> Right NormalOrder
        _                  -> Left "Unknown strategy. Expected one of: normal, applicative"

describeConfig :: RunConfig -> String
describeConfig cfg =
    let names = Map.keys (cfgEnv cfg)
        bindingSummary
            | null names = "Bindings: (none)"
            | otherwise  = "Bindings: [" ++ intercalate ", " names ++ "]"
    in "Current strategy: " ++ show (cfgStrategy cfg)
        ++ ", max steps: " ++ show (cfgLimit cfg)
        ++ ", " ++ bindingSummary

handleLet :: RunConfig -> String -> Either String (RunConfig, String)
handleLet config rest = do
    let afterCommand = trimStart rest
    (name, afterName) <-
        case span isIdentChar afterCommand of
            ([], _) -> Left "Expected binding name after :let"
            pair@(n, _)
                | isIdentStart (head n) -> Right pair
                | otherwise             -> Left "Binding names must start with a letter"
    let afterNameTrimmed = trimStart afterName
    case afterNameTrimmed of
        ('=':bodyRaw) -> do
            let body = trimStart bodyRaw
            if null body
                then Left "Expected expression on the right-hand side"
                else do
                    expanded <- parseAndExpand config body
                    let newEnv = Map.insert name expanded (cfgEnv config)
                        pretty = name ++ " = " ++ renderLambda (prettyExpr expanded)
                    Right (config { cfgEnv = newEnv }, pretty)
        _ -> Left "Expected '=' after binding name"

parseAndExpand :: RunConfig -> String -> Either String Expr
parseAndExpand config input =
    case parseExpr input of
        Left err   -> Left (renderParserError err)
        Right expr -> Right (Comb.expandWith (cfgEnv config) expr)

evaluateWithConfig :: RunConfig -> String -> Either String String
evaluateWithConfig config input = do
    expr <- parseAndExpand config input
    case normalizeWithStrategyLimit (cfgStrategy config) (cfgLimit config) expr of
        Left err   -> Left (renderCalculusError (EvalFailure err))
        Right res  -> Right (renderLambda (formatResult res))

renderLambda :: String -> String
renderLambda = map replace
  where
    replace '\\' = 'λ'
    replace c   = c

isIdentStart :: Char -> Bool
isIdentStart = isAlpha

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

trimStart :: String -> String
trimStart = dropWhile isSpace

preludeEnv :: Map String Expr
preludeEnv = Map.fromList
    [ ("add", addExpr)
    , ("succ", succExpr)
    , ("mul", mulExpr)
    ]
  where
    addExpr =
        Lam "m" $ Lam "n" $ Lam "f" $ Lam "x" $
            App (App (Var "m") (Var "f"))
                (App (App (Var "n") (Var "f")) (Var "x"))

    succExpr =
        Lam "n" $ Lam "f" $ Lam "x" $
            App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))

    mulExpr =
        Lam "m" $ Lam "n" $ Lam "f" $ Lam "x" $
            App (App (Var "m") (App (Var "n") (Var "f"))) (Var "x")
