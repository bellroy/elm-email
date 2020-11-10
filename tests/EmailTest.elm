module EmailTest exposing (suite)

import Email exposing (fromString, toString)
import Expect
import Test exposing (Test, describe, test)


{-| Valid email addresses

This list is taken and extended from <https://en.wikipedia.org/wiki/Email_address#Examples> .

-}
validEmailAddresses : List String
validEmailAddresses =
    [ "simple@example.com"
    , "Simple@example.com.au"
    , "very.common@example.com"
    , "disposable.style.email.with+symbol@example.com"
    , "other.email-with-hyphen@example.com"
    , "fully-qualified-domain@example.com"
    , "user.name+tag+sorting@example.com"
    , "x@example.com"
    , "example-indeed@strange-example.com"
    , "example@s.example"
    , "example!#$%&'*+-/=?^_`{|}~@example.com"
    , "ab.c@example.com"
    , "simple@com.plex-domain.org"

    -- The following email addresses are valid, but currently look like to big of an edge case to handle
    -- , "\" \"@example.org"
    -- , "\"john..doe\"@example.org"
    ]


{-| Invalid email addresses

This list is taken from <https://en.wikipedia.org/wiki/Email_address#Examples> .

-}
invalidEmailAddresses : List String
invalidEmailAddresses =
    [ -- no @ character
      "Abc.example.com"

    -- no domain
    , "Abc@"

    -- no top level domain
    , "Abc@domain"

    -- only one @ is allowed outside quotation marks
    , "A@b@c@example.com"

    -- none of the special characters in this local-part are allowed outside quotation marks
    , "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"

    -- quoted strings must be dot separated or the only element making up the local-part
    , "just\"not\"right@example.com"

    -- spaces, quotes, and backslashes may only exist when within quoted strings and preceded by a backslas
    , "this is\"not\u{0007}llowed@example.com"

    -- even if escaped (preceded by a backslash), spaces, quotes, and backslashes must still be contained by quotes
    , "this\\ still\"not\\allowed@example.com"

    -- [Disabled] local part is longer than 64 characters
    , "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"

    -- dot (.) can't be the first or last character unless quoted.
    , ".abc@example.com"

    -- dot (.) can't be the first or last character unless quoted.
    , "abc.@example.com"

    -- There can only be one dot (.) a time
    , "ab..c@example.com"

    -- There can't be a space at the end of the local part
    , "ab @example.com"

    -- There can't be a space at the start of the domain
    , "ab@ example.com"

    -- There can't be a space at the start of the TLD
    , "ab@example. com"
    ]


suite : Test
suite =
    describe "Email"
        [ describe "Email - Valid Email Addresses" <|
            List.map
                (\a ->
                    test ("Email - Valid Email (" ++ a ++ ")") <|
                        \_ ->
                            case fromString a of
                                Just _ ->
                                    Expect.pass

                                Nothing ->
                                    Expect.fail (a ++ " is a valid email address but it has failed parsing.")
                )
                validEmailAddresses
        , describe "Email - Invalid Email Addresses" <|
            List.map
                (\a ->
                    test ("Email - Invalid Email (" ++ a ++ ")") <|
                        \_ ->
                            case fromString a of
                                Just b ->
                                    Expect.fail (a ++ " is an invalid email address but it succeeded parsing.")

                                Nothing ->
                                    Expect.pass
                )
                invalidEmailAddresses
        , describe "Email - toString" <|
            List.map
                (\a ->
                    test ("Email - Parse and then render as string (" ++ a ++ ")") <|
                        \_ ->
                            case fromString a of
                                Just email ->
                                    Expect.equal a <| toString email

                                Nothing ->
                                    Expect.fail (a ++ " is a valid email address but it has failed parsing.")
                )
                validEmailAddresses
        , describe "Email - Manual Examples"
            [ test "Parses tags"
                (\_ ->
                    Expect.equal
                        (fromString "elvin+tag+tag2@gmail.com")
                        (Just
                            { localPart = "elvin"
                            , tags = [ "tag", "tag2" ]
                            , domain = "gmail"
                            , tld = [ "com" ]
                            }
                        )
                )
            ]
        ]
