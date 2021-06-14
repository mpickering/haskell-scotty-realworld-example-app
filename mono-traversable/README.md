## mono-traversable mega-repo

[![Build Status](https://dev.azure.com/snoyberg/mono-traversable/_apis/build/status/snoyberg.mono-traversable?branchName=master)](https://dev.azure.com/snoyberg/mono-traversable/_build/latest?definitionId=9&branchName=master)

This repository contains packages in the mono-traversable and classy-prelude
families. Please see the individual READMEs for more details:

You probably want to view [the README for mono-traversable
itself](https://github.com/snoyberg/mono-traversable/tree/master/mono-traversable#readme).

Additional packages in this repository:

* [mono-traversable](https://github.com/snoyberg/mono-traversable/tree/master/mono-traversable#readme)
  providing a set of classes for dealing with monomorphic data structures (like `ByteString` and `Text`)
  in a similar way to how the standard libraries treat polymorphic structures like lists
    * [mono-traversable-instances](https://github.com/snoyberg/mono-traversable/tree/master/mono-traversable-instances#readme),
      containing orphans instances for mono-traversable classes
* [chunked-data](https://github.com/snoyberg/mono-traversable/tree/master/chunked-data#readme),
  providing typeclasses for dealing with various chunked data representations
* [mutable-containers](https://github.com/snoyberg/mono-traversable/tree/master/mutable-containers#readme),
  abstactions and concrete implementations of mutable containers
* [conduit-combinators](https://github.com/snoyberg/mono-traversable/tree/master/conduit-combinators#readme),
  commonly used conduit functions, for both chunked and unchunked data
* [classy-prelude](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude#readme),
  a Prelude replacement based around the above packages (and many others)
    * [classy-prelude-conduit](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude-conduit#readme),
      extends classy-prelude with [conduit support](https://github.com/snoyberg/conduit)
    * [classy-prelude-yesod](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude-yesod#readme),
      extends classy-prelude-conduit with [Yesod web framework support](http://www.yesodweb.com)
* [minlen](https://github.com/snoyberg/mono-traversable/tree/master/minlen#readme),
  provided a newtype wrapper with type-level annotation of minimum container
  length. This is a generalization of the `Data.NonNull` module in `mono-traversable`
