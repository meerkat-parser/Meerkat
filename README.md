# Meerkat

The Meerat library enables general combinator-style parsing in Scala. 
Meerkat parsers provide:

- Support for all context-free grammars (including left recursive and ambiguous grammars)
- Produce Shared Packed Parse Forest (SPPF) in cubic time and space

To make Meerkat parsers practical for parsing programming languages, we also provide the following features:

- Scannerless parsing and character-level disambiguation combinators
- Declarative operator precedence combinators
- A user-friendly Tree format for processing parse results
- Support for Semantic actions
- Data-dependent parsing to deal with context-sensitive constructs

At the moment, the Meerkat library is a prototype, and we are trying to fix the remaining
issues and complete the documentation **in the coming weeks**. In the meantime, if you encounter
any problem, please open an issue or contact us.

Currently the only way to use the Meerkat library is to build it from the source.
You need to first install [SBT](http://www.scala-sbt.org/). To build the Meerkat library,
clone this repository, and run `sbt package`.

More information about the Meerkat libary can be found in [Wiki](https://github.com/Anastassija/Meerkat/wiki).

The grammars written using the Meerkat library are available [here](https://github.com/afroozeh/Meerkat-Grammars).

The Meerkat library is created by Anastasia Izmaylova and Ali Afroozeh and is released under the BSD license.

