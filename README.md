# Airship

[![GitHub CI][github-shield]][github-ci] [![docs][docs-shield]][docs]

Airship is a Haskell library for handling and serving HTTP requests in a RESTful fashion. It is heavily inspired by [Webmachine](https://github.com/basho/webmachine)
and works with any [WAI](https://hackage.haskell.org/package/wai)-compatible web server such as [Warp](https://hackage.haskell.org/package/warp).

It aims to be small, fast, and flexible.

# How does it work?

Airship resources are represented with a [`Resource` record type](https://github.com/tmcgilchrist/airship/blob/master/airship/src/Airship/Resource.hs#L39-L117).
Each field in `Resource` corresponds to an action taken in the [Webmachine decision tree](https://raw.githubusercontent.com/wiki/Webmachine/webmachine/images/http-headers-status-v3.png).
Airship provides a `defaultResource` with sensible defaults for each of these actions; you build web services by overriding fields in the default resource with your own.

Routes are declared with a simple monadic syntax:

```haskell
routes = do
    root                        #> someRootResource
    "account" </> var "name"    #> accountResource
```

For a simple example that handles HTTP GET and POST requests, please check [`example/Basic.hs`](https://github.com/tmcgilchrist/airship/blob/master/example/Basic.hs).
For a slightly more involved example that generates HTML and manages a pool of resources, please check the [blimp](https://github.com/patrickt/blimp) repository.

Airship is copyright &copy; 2015 Helium Systems, Inc., and released to the public under the terms of the MIT license.

[github-shield]: https://github.com/tmcgilchrist/airship/actions/workflows/ci.yml/badge.svg
[github-ci]: https://github.com/tmcgilchrist/airship/actions/workflows/ci.yml

[docs-shield]:https://img.shields.io/badge/doc-online-blue.svg
[docs]: https://tmcgilchrist.github.io/airship/index.html
