# Airship

[![Join the chat at https://gitter.im/helium/airship](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/helium/airship?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Airship is a Haskell library for handling and serving HTTP requests in a RESTful fashion. It is heavily inspired by [Webmachine](https://github.com/basho/webmachine)
and works with any [WAI](https://hackage.haskell.org/package/wai)-compatible web server such as [Warp](https://hackage.haskell.org/package/warp).

It aims to be small, fast, and flexible.

# How does it work?

Airship resources are represented with a [`Resource` record type](https://github.com/helium/airship/blob/master/airship/src/Airship/Resource.hs#L39-L117).
Each field in `Resource` corresponds to an action taken in the [Webmachine decision tree](https://raw.githubusercontent.com/wiki/Webmachine/webmachine/images/http-headers-status-v3.png).
Airship provides a `defaultResource` with sensible defaults for each of these actions; you build web services by overriding fields in the default resource with your own.

Routes are declared with a simple monadic syntax:

```haskell
routes = do
    root                        #> someRootResource
    "account" </> var "name"    #> accountResource
```

For a simple example that handles HTTP GET and POST requests, please check [`example/Basic.hs`](https://github.com/helium/airship/blob/master/example/Basic.hs).
For a slightly more involved example that generates HTML and manages a pool of resources, please check the [blimp](https://github.com/patrickt/blimp) repository.

Airship is copyright &copy; 2015 Helium Systems, Inc., and released to the public under the terms of the MIT license.
