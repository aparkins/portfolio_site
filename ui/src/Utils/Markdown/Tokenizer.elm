module Utils.Markdown.Tokenizer exposing (..)

import String exposing (fromList)

type Token
  = Asterisk
  | Backtick
  | Newline
  | Tilde
  | Underscore
  | Text String


tokenizedChar : Char -> Token
tokenizedChar char =
    case char of
        '*' -> Asterisk
        '`' -> Backtick
        '\n' -> Newline
        '~' -> Tilde
        '_' -> Underscore
        _ -> fromList [char] |> Text


collapseTokenIntoList : Token -> List Token -> List Token
collapseTokenIntoList token collapsedTokens =
    case collapsedTokens of
        headToken::otherTokens ->
            case headToken of
                Text headString ->
                    case token of
                        Text newString ->
                            Text (newString ++ headString)::otherTokens
                        _ -> token::headToken::otherTokens
                _ -> token::headToken::otherTokens
        _ -> token::collapsedTokens


collapsedTokens : List Token -> List Token
collapsedTokens tokens =
    List.foldr collapseTokenIntoList [] tokens


tokenizedString : String -> List Token
tokenizedString string =
    List.map tokenizedChar (String.toList string)
        |> collapsedTokens