module Email exposing (Email, fromString, toString)

{-|

@docs Email, fromString, toString

-}

import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompWhile, end, getChompedString, loop, map, oneOf, problem, run, succeed, symbol)


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
fromString string =
    case run parseEmail string of
        Ok result ->
            result

        _ ->
            Nothing


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
        , "."
        , String.join "." tld
        ]



-- Parser


parseEmail : Parser (Maybe Email)
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
        (\localPart tags domain tlds ->
            let
                fullLocalPart =
                    String.join ""
                        [ localPart
                        , case tags of
                            [] ->
                                ""

                            _ ->
                                "+" ++ String.join "+" tags
                        ]
            in
            if String.length fullLocalPart > 64 then
                Nothing

            else if List.length tlds < 1 then
                Nothing

            else
                Just
                    { localPart = localPart
                    , tags = tags
                    , domain = domain
                    , tld = tlds
                    }
        )
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
                (a /= '+')
                    && (a /= '@')
                    && (a /= '\\')
                    && (a /= '"')
            )
        |> getChompedString
        |> andThen
            (\localPart ->
                if String.startsWith "." localPart || String.endsWith "." localPart || String.indexes ".." localPart /= [] then
                    problem "localPart can't start or end with a dot, nor can there be double dots"

                else
                    succeed localPart
            )


parseDomain : Parser String
parseDomain =
    succeed ()
        |. chompWhile
            (\a ->
                (Char.isAlphaNum a
                    || (a == '-')
                )
                    && (a /= '@')
                    && (a /= '.')
            )
        |> getChompedString
        |> andThen
            (\a ->
                if String.length a < 1 then
                    problem "Domain has to be atleast 1 character long."

                else
                    succeed a
            )


parseTld : Parser String
parseTld =
    succeed ()
        |. chompWhile
            (\a ->
                Char.isUpper a
                    || Char.isLower a
                    || (a == '-')
            )
        |> getChompedString
        |> andThen
            (\a ->
                if String.length a >= 2 then
                    succeed a

                else
                    problem "Tld needs to be at least 2 character long."
            )
