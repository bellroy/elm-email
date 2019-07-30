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
    , tags : List String
    , domain : String
    , tld : List String
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
    let
        split char parser =
            loop []
                (\r ->
                    oneOf
                        [ succeed (\tld -> Loop (tld :: r))
                            |. symbol (String.fromChar char)
                            |= parser
                        , succeed ()
                            |> map (\_ -> Done (List.reverse r))
                        ]
                )
    in
    succeed
        Email
        |= parseLocalPart
        |= split '+' parseLocalPart
        |. symbol "@"
        |= parseDomain
        |= split '.' parseTld
        |. end


parseLocalPart : Parser String
parseLocalPart =
    succeed ()
        |. chompWhile
            (\a ->
                Char.isAlphaNum a
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
        |> andThen
            (\a ->
                if String.length a >= 2 then
                    succeed a

                else
                    problem "Tld needs to be at least 2 chars long."
            )
