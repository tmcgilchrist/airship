## Authorisation

Authorisation is about whether a request to a resource is allowed. It's closely
connected to authentication, which is concerned with who the request is from.
Airship doesn't directly provide a mechanism for authentication, you are free to
choose how this is done.

To force authorisation on a resource airship provides an `isAuthorised`
callback.
e.g.

``` haskell

    dirigibleResourceV1 :: (Applicative m, MonadIO m) => Resource State m
    dirigibleResourceV1 = defaultResource {
        allowedMethods = return [ HTTP.methodGet ]
      , isAuthorised = checkAuthorisation
    }

    checkAuthorisation :: (Applicative m, MonadIO m) => Webmachine s m Bool
    checkAuthorisation = do
      headers <- lift $ requestHeaders <$> request
      let header = lookup hAuthorization headers
      return . isJust $ header

```
