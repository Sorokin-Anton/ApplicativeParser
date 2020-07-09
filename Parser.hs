module JSON.Parser (parse) where



import Control.Applicative
import Data.Bifunctor ( first )
import Data.Char ( isSpace, isDigit )

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null

parse :: String -> Maybe Value
parse s = do
    ( result, "" ) <- parseValue $$ s
    return result


parseValue :: Parser Value
parseValue = Parser $ \z -> do
    ( a, z' ) <- (parseNull <|> parseStringLiteral <|> parseNumber <|> parseBool
                  <|> parseArray <|> parseObject) $$ dropWhile isSpace z
    return ( a, dropWhile isSpace z' )

newtype Parser a = Parser { runParser :: String -> Maybe ( a, String ) }

($$) :: Parser a -> String -> Maybe ( a, String )
a $$ b = runParser a b

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
    pure x = Parser $ \z -> Just ( x, z )

    f <*> g = Parser $ \z -> do
        ( a, z' ) <- f $$ z
        ( b, z'' ) <- g $$ z'
        return ( a b, z'' )

instance Alternative Parser where
    empty = Parser $ const Nothing

    (Parser f) <|> (Parser g) = Parser $ \z -> f z <|> g z

parseCharCond :: (Char -> Bool) -> Parser Char
parseCharCond cond = Parser f
  where
    f (x : xs)
        | cond x = Just ( x, xs )
    f _ = Nothing

parseChar :: Char -> Parser Char
parseChar c = parseCharCond (== c)

parseWhile :: (Char -> Bool) -> Parser String
parseWhile cond = Parser $ \z -> case parseCharCond cond $$ z of
    Nothing -> Just ( "", z )
    Just ( x, z' ) -> first (x :) <$> parseWhile cond $$ z'

parseString :: String -> Parser String
parseString = traverse parseChar

parseNull :: Parser Value
parseNull = Null <$ parseString "null"

parseBool :: Parser Value
parseBool = Boolean <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ parseString "true"

    parseFalse = False <$ parseString "false"

parsePosNumber :: Parser Double
parsePosNumber = parseD <|> parseN
  where
    digits = notNull $ parseWhile isDigit

    parseN = Parser $ \z -> do
        ( x, s ) <- notNull digits $$ z
        if head x == '0' && length x > 1 then Nothing else Just ( read x, s )

    parseD = Parser $ \z -> do
        ( a, z' ) <- digits $$ z
        ( '.', z'' ) <- parseChar '.' $$ z'
        ( b, z''' ) <- digits $$ z''
        f ( a ++ "." ++ b, z''' )
      where
        f :: ( String, String ) -> Maybe ( Double, String )
        f ( x, s )
            | head x == '0' && length (takeWhile (/= '.') x) > 1
                || last x == '0' && length x > 1 = Nothing
            | otherwise = Just ( read x, s )

parseNumber :: Parser Value
parseNumber = Parser $ \z -> case parseChar '-' $$ z of
    Nothing -> (Number <$> parsePosNumber) $$ z
    Just ( '-', z' ) -> (Number . negate <$> parsePosNumber) $$ z'

parseStringLiteral :: Parser Value
parseStringLiteral = String
    <$> (parseChar '\"' *> parseWhile (/= '\"') <* parseChar '\"')

parseArray :: Parser Value
parseArray = Array <$> (parseChar '[' *> values <* parseChar ']')
  where
    values = Parser $ (\z -> case parseValue $$ z of
        Nothing -> Just ( [], z )
        Just ( v, z' ) -> case parseChar ',' $$ z' of
            Nothing -> Just ( [ v ], z' )
            Just ( _, z'' ) -> first (v :) <$> values $$ z'') . dropWhile isSpace

parseObject :: Parser Value
parseObject = Object <$> (parseChar '{' *> values <* parseChar '}')
  where
    values = Parser $ (\z -> if null z
        then Nothing else if head z == '}' then Just ( [], z ) else do
            ( k, z' ) <- parseStringLiteral $$ dropWhile isSpace z
            ( _, z'' ) <- parseChar ':' $$ dropWhile isSpace z'
            ( v, z''' ) <- parseValue $$ dropWhile isSpace z''
            case parseChar ',' $$ z''' of
                Nothing -> Just ( [ ( k, v ) ], z''' )
                Just ( _, z'''' ) -> first (( k, v ) :) <$> values $$ z'''') . dropWhile isSpace

notNull :: Foldable t => Parser (t a) -> Parser (t a)
notNull p = Parser $ \z -> do
    ( x, z' ) <- p $$ z
    if null x then Nothing else return ( x, z' )
