# elm-email

Parses email addresses safely

# Motivation

Validating email addresses using Regex is a pain.

So far I personally have never found the one Regex to rule them all.

Having the ability to parse email addresses from String not only means they are validated,
but also adds the ability to then add your own additional validations on parts of the email address that might be specific to your application or such.

## example

```elm
import Email

parsedEmail: Maybe Email
parsedEmail =
    Email.fromString "simple+tag@example.com.au"


```

Result
```elm
  { localPart = "simple"
  , tags = [ "tag" ]
  , domain = "example"
  , tld = [ "com", "au"]
  }

```


# ToDo

- [ ] Make all the specs work
