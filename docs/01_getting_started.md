
# Getting started

## Build setup

To use scala-offheap one has to add following lines to their sbt build:

```scala
libraryDependencies += "sh.den" % "scala-offheap_2.11" % "0.1"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

This includes scala-offheap's latest 0.1 release and current (as of this writing)
release of macro paradise. Macro paradise is only necessary to expand `@data` and
`@variant` annotations and can be omitted if those features are not used.

Example sbt project with simple setup is
[available on github](https://github.com/densh/scala-offheap-example).

## Example repl setup

All examples in this guide assume the following wildcard import:

```scala
scala> import offheap._
```

## Supported environments

At the moment scala-offheap supports Oracle and Open JDK 7 and 8 running on Intel
x86-64 hardware and recent Linux, Mac OS operating systems. The library
hasn't been tested on other software/hardware configurations and might
or might not work for them.

If you've managed to successfully use the library on other environments let us know.
