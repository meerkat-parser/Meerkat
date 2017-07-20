
## Build
Meerkat is a [Scala SBT](http://www.scala-sbt.org) project. To build Meerkat from command line run:

```
sbt compile
```

## IDE Support
Meerkat uses [Scala Macros](http://scalamacros.org), and IDE support for Scala Macros is not at the moment very satisfactory.

### Eclipse
[Scala IDE](http://scala-ide.org), which is based on Eclipse, is the best choice for developing Meerkat, as it can recognize Macros (with some effort). To use Scala IDE for developing Meerkat, do the following steps:

- In the Meerkat directory, run `sbt eclipse`. This command will generate `.project` and `.classpath` files, which are necessary for Eclipse projects.
- Open Eclipse, and import the Meerkat directory as an existing Eclipse project.
- Make sure that the following Scala Compiler settings (Preferences > Scala > Compiler > Build Manager) are set:
  - recompileOnMacroDef
  - useScopesCompiler
- Comment the content of the file `src/macros/scala/org/meerkat/Syntax.scala` and save. This will trigger building of non-macro files.
- Uncomment the content of `Syntax.scala` and save. This will trigger the compiliation of macros.

Now, syntax errors should disapear from the files.
