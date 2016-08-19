# quack

> If it looks like a quack, then it makes sense some other time.

Super simple parser combinators for URI query strings!

It just piggy-backs on existing high-performance parser combinators like
[attoparsec](https://hackage.haskell.org/package/attoparsec) and
[aeson](https://hackage.haskell.org/package/aeson) to parse data that looks
like `[(Text, Maybe Text)]`. Check it out:

```haskell
import Data.Attoparsec.Text
import Data.Uri.Query


runParser (many (overEquals (,) (attoparsec double) (attoparsec double)))
  [("123", Just "456"), ("123", Nothing), ("123", Just "456")]
```

returns

```
Right [(123,456)]
```

It tries to follow the same semantics as attoparsec; backtrack on failure.
It's implemented with a really really simple zipper between "parsed so far"
and "to parse", if that makes sense - the head of "to parse" is the subject,
while failure just reverts the append.
