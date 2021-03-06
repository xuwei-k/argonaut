# Argonaut

[![Join the chat at https://gitter.im/argonaut-io/argonaut](https://badges.gitter.im/argonaut-io/argonaut.svg)](https://gitter.im/argonaut-io/argonaut?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.argonaut/argonaut_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.argonaut/argonaut_2.12)


### What is Argonaut?

Argonaut is a JSON library for Scala, providing a rich library for parsing, printing and manipulation as well as convenient codecs for translation to and from scala data types.

Argonaut is licenced under BSD3 (see `LICENCE`). See more at [http://argonaut.io](http://argonaut.io).


### Documentation

* [User Docs](http://argonaut.io/doc/)
* [Scala Docs](http://argonaut.io/scaladocs/)
* [Examples](https://github.com/argonaut-io/argonaut/tree/master/argonaut/shared/src/test/scala/argonaut/example)


### SBT Settings

Just add argonaut as a dependency.

Stable:

```scala
libraryDependencies += "io.argonaut" %% "argonaut" % "6.3.5"
```

Note that the 6.2.x releass supports scala `2.11.*`, `2.12.*` and `2.13.*` with scalaz `7.2.*`.

Note that the 6.3.x releass supports scala `2.12.*`, `2.13.*` and `3.0.x` with scalaz `7.3.*`.


### Release

Add to `~/.sbt/1.0/sonatype.sbt`


    credentials += Credentials("Sonatype Nexus Repository Manager",
                               "oss.sonatype.org",
                               "<username>",
                               "<password>")


For a snapshot build run:
    ./sbt +publish

For a release build run:

    ./sbt "release cross"

Note for a release build you will want to enter the details for the
release build number and then the subsequent build number. At this
step it is fine to enter the original build number as the next number
(for example when doing Milestone or RC builds). As an example:

    Release version [6.0] : 6.0-M3
    Next version [6.1-SNAPSHOT] : 6.0-SNAPSHOT


### Provenance

Argonaut was initially developed to support products at [Ephox](http://ephox.com), who have now kindly relinquished control to the community.

The library was open-sourced under a [BSD License](https://github.com/argonaut-io/argonaut/blob/master/LICENSE), drawing users, support and improvements from a number of contributors.
