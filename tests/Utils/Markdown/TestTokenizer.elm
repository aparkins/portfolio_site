module Utils.Markdown.TestTokenizer exposing (..)

import Utils.Markdown.Tokenizer exposing(..)

import Char
import Fuzz exposing (..)
import Test exposing (..)
import Expect

suite : Test
suite =
  describe "The Tokenizer Module"
    [ describe "tokenizedChar"
        [ test "Asterisk" <|
            \_ -> tokenizedChar '*'
               |> Expect.equal Asterisk
        , test "Backtick" <|
            \_ -> tokenizedChar '`'
               |> Expect.equal Backtick
        , test "Newline" <|
            \_ -> tokenizedChar '\n'
               |> Expect.equal Newline
        , test "Tilde" <|
            \_ -> tokenizedChar '~'
               |> Expect.equal Tilde
        , test "Underscore" <|
            \_ -> tokenizedChar '_'
               |> Expect.equal Underscore
        , fuzzWith { runs = 100 }
                   plainCharFuzzer
                   "Text for any other character" <|
            \char -> tokenizedChar char
               |> Expect.equal (Text <| String.fromList [char])
        ]
    , describe "collapseTokenIntoList"
        [ fuzzWith { runs = 25 }
                   tokenFuzzer
                   "Token into empty list" <|
            \token -> collapseTokenIntoList token []
               |> Expect.equal [token]
        , fuzzWith { runs = 200 }
                   (Fuzz.tuple ( textTokenFuzzer
                               , Fuzz.list nonTextTokenFuzzer
                               ))
                   "Text token into list with only Non-Text tokens" <|
            \(textToken, nonTextTokens) ->
                  collapseTokenIntoList textToken nonTextTokens
               |> Expect.equal (textToken::nonTextTokens)
        , fuzzWith { runs = 200 }
                   (Fuzz.tuple3 ( textTokenFuzzer
                                , nonTextTokenFuzzer
                                , Fuzz.list nonTextTokenFuzzer
                                ))
                   "Non-Text token into list with leading Text token" <|
            \(textToken, nonTextToken, otherNonTextTokens) ->
                  collapseTokenIntoList nonTextToken (textToken::otherNonTextTokens)
               |> Expect.equal (nonTextToken::textToken::otherNonTextTokens)
        , fuzzWith { runs = 300 }
                   (Fuzz.tuple3 ( plainStringFuzzer
                                , plainStringFuzzer
                                , Fuzz.list nonTextTokenFuzzer
                                ))
                   "Text token into list with leading Text token" <|
            \(firstString, secondString, nonTextTokens) ->
                  collapseTokenIntoList
                        (Text firstString)
                        ((Text secondString)::nonTextTokens)
               |> Expect.equal ((Text (firstString ++ secondString))::nonTextTokens)
        , fuzzWith { runs = 300 }
                   (Fuzz.tuple4 ( plainStringFuzzer
                                , plainStringFuzzer
                                , nonEmptyListFuzzer nonTextTokenFuzzer
                                , Fuzz.list nonTextTokenFuzzer
                                ))
                   "Text token into list with leading Non-Text token, but a Text token inside the list" <|
            \(firstString, secondString, leadingNonTextTokens, trailingNonTextTokens) ->
                  collapseTokenIntoList
                        (Text firstString)
                        (leadingNonTextTokens ++ [Text secondString] ++ trailingNonTextTokens)
               |> Expect.equal ( [Text firstString]
                              ++ leadingNonTextTokens
                              ++ [Text secondString]
                              ++ trailingNonTextTokens
                               )
        ]
    , describe "collapsedTokens"
        [ test "Empty list" <|
            \_ -> collapsedTokens []
               |> Expect.equal []
        , fuzzWith { runs = 100 }
                   tokenFuzzer
                   "Singleton list" <|
            \token -> collapsedTokens [token]
               |> Expect.equal [token]
        , fuzzWith { runs = 100 }
                   (Fuzz.list nonTextTokenFuzzer)
                   "Non-Text token list" <|
            \nonTextTokens ->
                  collapsedTokens nonTextTokens
               |> Expect.equal nonTextTokens
        , fuzzWith { runs = 200 }
                   (nonEmptyListFuzzer plainStringFuzzer)
                   "List of Text Tokens into a singleton Text Token list" <|
            \strings ->
                  List.map Text strings |> collapsedTokens
               |> Expect.equal [Text (String.concat strings)]
        , test "Partitioned Text Tokens stay separate" <|
            \_ -> collapsedTokens
                    [ Text "Hello"
                    , Asterisk
                    , Text "World"
                    , Text " this is "
                    , Text "the end"
                    , Asterisk
                    , Backtick
                    , Text "foo"
                    , Text "bar"
                    ]
               |> Expect.equal
                    [ Text "Hello"
                    , Asterisk
                    , Text "World this is the end"
                    , Asterisk
                    , Backtick
                    , Text "foobar"
                    ]
        ]
    ]


-- Transform a fuzzer of some item into a fuzzer of lists of those items,
-- with the guarantee that the generated list has at least one item contained
nonEmptyListFuzzer : Fuzzer a -> Fuzzer (List a)
nonEmptyListFuzzer fuzzer =
       Fuzz.list fuzzer
    |> Fuzz.map2 (::) fuzzer


plainCharFuzzer : Fuzzer Char
plainCharFuzzer =
    Fuzz.oneOf
      [ Fuzz.map Char.fromCode (Fuzz.intRange 32 41)
      , Fuzz.map Char.fromCode (Fuzz.intRange 43 94)
      , Fuzz.map Char.fromCode (Fuzz.intRange 97 125)
      ]


plainStringFuzzer : Fuzzer String
plainStringFuzzer =
     Fuzz.list plainCharFuzzer
  |> Fuzz.map String.fromList


textTokenFuzzer : Fuzzer Token
textTokenFuzzer = plainCharFuzzer
               |> Fuzz.map String.fromChar
               |> Fuzz.map Text


nonTextTokenFuzzer : Fuzzer Token
nonTextTokenFuzzer =
    Fuzz.oneOf
      [ Fuzz.constant Asterisk
      , Fuzz.constant Backtick
      , Fuzz.constant Newline
      , Fuzz.constant Tilde
      , Fuzz.constant Underscore
      ]


tokenFuzzer : Fuzzer Token
tokenFuzzer =
    Fuzz.frequency
      [ (1, textTokenFuzzer)
      , (5, nonTextTokenFuzzer)
      ]