module Email exposing
    ( Email
    , fromString
    )

{-|

@docs Email

-}

import Parser exposing (..)


{-| The type of an Email
-}
type alias Email =
    { localPart : String
    , domain : String
    , tld : List String
    , tags : List String
    , comments : List String
    }


{-| Parse an Email address from a String
-}
fromString :
    String
    -> Maybe Email
fromString =
    Result.toMaybe << run parseEmail


{-| Render Email to a String
-}
toString : Email -> String
toString { localPart, tags, domain, tld } =
    String.join ""
        [ localPart
        , case tags of
            [] ->
                ""

            _ ->
                "+" ++ String.join "+" tags
        , "@"
        , domain
        , String.join "." tld
        ]



-- Parser


parseEmail : Parser Email
parseEmail =
    succeed
        (\localPart domain tlds ->
            { localPart = localPart
            , domain = domain
            , tld = tlds
            , tags = []
            , comments = []
            }
        )
        |= parseLocalPart
        |. symbol "@"
        |= parseDomain
        |= loop []
            (\r ->
                oneOf
                    [ succeed (\tld -> Loop (tld :: r))
                        |. symbol "."
                        |= parseTld
                    , succeed ()
                        |> map (\_ -> Done (List.reverse r))
                    ]
            )
        |. end


parseLocalPart : Parser String
parseLocalPart =
    succeed ()
        |. chompWhile
            (\a ->
                Char.isAlphaNum a
                    || (a == '+')
                    || (a == '.')
                    || (a == '-')
            )
        |> getChompedString


parseDomain : Parser String
parseDomain =
    succeed ()
        |. chompWhile
            (\a ->
                Char.isAlphaNum a
                    || (a == '-')
            )
        |. chompUntil "."
        |> getChompedString


parseTld : Parser String
parseTld =
    succeed ()
        |. chompWhile
            (\a ->
                Char.isUpper a
                    || Char.isLower a
            )
        |> getChompedString
        |> andThen validateTld


validateTld : String -> Parser String
validateTld string =
    if String.length string >= 2 then
        succeed string

    else
        problem "Tld needs to be at least 2 chars long."
