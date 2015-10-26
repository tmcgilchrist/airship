## Versioning APIs

Often when building APIs we need a way to version them. Versioning allows
evolving APIs without adding hacks around how to detect which version has been requested.
There are two good approaches available for versioning APIs with Airship (Webmachine).

### By URL
Present different urls prefixed by the version of the api
e.g.
` /v1/dirigible/` and `/v2/dirigible`

Define the routing urls like so:

``` haskell

    routes :: RoutingSpec State IO ()
    routes = do
      "v1" </> "dirigible" #> dirigibleResourceV1
      "v2" </> "dirigible" #> dirigibleResourceV2

```

Then define the 2 versions of your resource. Here we handle a json `GET` and
json response.

``` haskell

    dirigibleResourceV1 :: (Applicative m, MonadIO m) => Resource State m
    dirigibleResourceV1 = defaultResource {
        allowedMethods = return [ HTTP.methodGet ]
        , knownContentType = contentTypeMatches ["application/json"]
        , contentTypesProvided = return [("application/json", handleJsonV1)]
    }


    dirigibleResourceV2 :: (Applicative m, MonadIO m) => Resource State m
    dirigibleResourceV2 = defaultResource {
        allowedMethods = return [ HTTP.methodGet ]
      , knownContentType = contentTypeMatches ["application/json"]
      , contentTypesProvided = return [("application/json", handleJsonV2)]
    }

```

You can test this by using curl against the correct url.
` curl -X GET http://localhost:port/v1/dirigible -H "Content-Type: application/json"`
or
` curl -X GET http://localhost:port/v2/dirigible -H "Content-Type: application/json"`

### By Content-Type
Place the API version into the HTTP Content-Type header.

``` haskell

    routes :: RoutingSpec State IO ()
    routes = do
      "dirigible" #> dirigibleResource
```

Then within the resource we handle both versions.

``` haskell

    dirigibleResource :: (Applicative m, MonadIO m) => Resource State m
    dirigibleResource = defaultResource {
        allowedMethods = return [ HTTP.methodGet ]
      , knownContentType = contentTypeMatches ["application/json"]
      , knownContentTypes = contentTypesProvided ["application/v1+json", "application/v2+json"]
      , contentTypesProvided = return [ ("application/v1+json", handleJsonV1)
                                      , ("application/v2+json", handleJsonV2)]
    }

```

Testing this using curl requires setting a custom content type header.

` curl -X GET http://localhost:port/dirigible -H "Content-Type: application/v1+json"`

or

` curl -X GET http://localhost:port/dirigible -H "Content-Type: application/v2+json"`

If you don't supply a `Content-Type` you'll get a 406 Not Acceptable, indicating that no content-type matches. 

Also it's important to note that when testing using `curl` it will by default set an `Accept: */*` indicating that you accept any content-type. This will perform matching on the `contentTypesProvided` list starting at the beginning. So in the example above, V1 appears before V2 which means an `Accept */*` will match V1 first and never get to V2. 
