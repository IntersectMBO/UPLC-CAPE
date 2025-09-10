# Haskell Code Style

## Lanugage extensions

If the BlockArguments extension is enabled, avoid using infix `$` to apply blocks.

BAD:

```haskell
handleErrors $ do
  pure $ 3 * 2
```

```haskell
forM_ testCases $ \original -> ...
```

GOOD:

```haskell
handleErrors do
  pure $ 3 * 2
```

```haskell
forM_ testCases \original -> ...
```

If the LambdaCase extension is enabled, prefer using `\case` where otherwise a function argument is bound to a name and then immidiately pattern matched.

## Indentation

### case expressions

Inside function definitions, case should not follow = on the same line.

BAD:

```haskell
parseBuiltinDataText :: Text -> Either ParseError Data
parseBuiltinDataText input = case runParser (sc *> parseData <* eof) "" input of
  Left err -> Left (ParseError err)
  Right result -> Right result
```

GOOD:

```haskell
parseBuiltinDataText :: Text -> Either ParseError Data
parseBuiltinDataText input =
  case runParser (sc *> parseData <* eof) "" input of
    Left err -> Left (ParseError err)
    Right result -> Right result
```

### Imports

Group wildcard imports separately before named and qualified imports.
