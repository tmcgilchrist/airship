# Airship

Airship is a Haskell library for handling and serving HTTP requests in a RESTful fashion. It is heavily inspired by [Webmachine](https://github.com/basho/webmachine) and piggybacks on the excellent [Warp](https://hackage.haskell.org/package/warp) web server. It aims to be small, fast, and flexible.

The API is highly experimental and subject to rapid, dizzying change. Caveat programmator.

# How does it work?

Airship resources are represented with a [`Resource` record type](https://github.com/helium/airship/blob/master/src/Airship/Resource.hs#L36-L64). Each field in `Resource` corresponds to an action taken in the [Webmachine decision tree](https://raw.githubusercontent.com/wiki/basho/webmachine/images/http-headers-status-v3.png). Airship provides a `defaultResource` with sensible defaults for each of these actions; you build web services by overriding fields in the default resource with your own.

Routes are declared with a simple monadic syntax:

```haskell
routes = do
    root                        #> someRootResource
    "account" </> var "name"    #> accountResource
```

For a simple example that handles HTTP GET and POST requests, please check [`bin/Main.hs`](https://github.com/helium/airship/blob/master/bin/Main.hs). For a slightly more involved example that generates HTML and manages a pool of resources, please check the [blimp](https://github.com/patrickt/blimp) repository.

Airship is copyright &copy; 2015 Helium Systems, Inc., and released to the public under the terms of the MIT license.
