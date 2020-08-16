import build._

val argonaut = argonautCrossProject(
    "argonaut"
  , Seq(JVMPlatform)
).settings(
  InfoSettings.all ++ Seq[Sett](
    name := "argonaut"
  , (sourceGenerators in Compile) += ((sourceManaged in Compile) map Boilerplate.gen).taskValue
  , libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
  )
)

val argonautJVM = argonaut.jvm


val argonautScalaz = argonautCrossProject(
    "argonaut-scalaz"
  , Seq(JVMPlatform)
).settings(
  Seq(
    name := "argonaut-scalaz"
  , libraryDependencies ++= Seq(
      "org.scalaz"                   %%% "scalaz-core"               % scalazVersion
    )
  )
).platformsSettings(JVMPlatform)(
  libraryDependencies += "org.scalaz" %%% "scalaz-scalacheck-binding" % scalazVersion % "test",
  libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
).dependsOn(argonaut % "compile->compile;test->test")

val argonautScalazJVM = argonautScalaz.jvm


val argonautMonocle = argonautCrossProject(
    "argonaut-monocle"
  , Seq(JVMPlatform)
).settings(
  Seq[Sett](
    name := "argonaut-monocle"
  , libraryDependencies ++= Seq(
      "com.github.julien-truffaut"   %%% "monocle-core"              % monocleVersion
    , "com.github.julien-truffaut"   %%% "monocle-macro"             % monocleVersion
    , "com.github.julien-truffaut"   %%% "monocle-law"               % monocleVersion % "test"
    )
  , libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
  )
).dependsOn(argonaut % "compile->compile;test->test", argonautScalaz % "compile->compile;test->test")

val argonautMonocleJVM = argonautMonocle.jvm


val argonautCats = argonautCrossProject(
    "argonaut-cats"
  , Seq(JVMPlatform)
).settings(
  Seq(
    name := "argonaut-cats"
  , libraryDependencies ++= Seq(
      "org.typelevel"                %%% "cats-core"                 % catsVersion
    , "org.typelevel"                %%% "cats-laws"                 % catsVersion              % "test"
    , "org.typelevel"                %%% "discipline-specs2"         % "1.1.0"                  % "test"
    )
  )
  , libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
).dependsOn(argonaut % "compile->compile;test->test")

val argonautCatsJVM = argonautCats.jvm


val argonautJawn = argonautCrossProject(
    "argonaut-jawn"
  , Seq(JVMPlatform)
).settings(
  Seq(
    name := "argonaut-jawn"
  , libraryDependencies ++= Seq(
      "org.typelevel"               %%%  "jawn-parser"               % "1.0.0"
    )
  , libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautJawnJVM = argonautJawn.jvm

val argonautBenchmark = Project(
  id = "argonaut-benchmark"
, base = file("argonaut-benchmark")
).settings(
  base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-benchmark"
  , fork in run := true
  , publishArtifact := false
  , mimaFailOnNoPrevious := false
  , libraryDependencies ++= Seq(
      "com.google.caliper"           %   "caliper"                   % "0.5-rc1"
    , "com.fasterxml.jackson.core"   %   "jackson-core"              % "2.11.2"
    )
  , javaOptions in run ++= ((fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }).value
  )
).dependsOn(argonautJVM)


val jvmProjects = Seq(
  argonautJVM, argonautScalazJVM, argonautMonocleJVM, argonautCatsJVM, argonautJawnJVM, argonautBenchmark
)

lazy val noPublish = Seq(
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {},
  publishLocal := {},
  publishArtifact in Compile := false,
  publish := {}
)

val jvmParent = project
  .settings(
    base
  , noPublish
  ).aggregate(
    jvmProjects.map(p => p: ProjectReference) : _*
  )

val argonautParent = Project(
  id = "argonaut-parent"
, base = file(".")
).settings(
    base
  , ReleasePlugin.projectSettings
  , mimaFailOnNoPrevious := false
  , PublishSettings.all
  , noPublish
  , name := "argonaut-parent"
  , fork in run := true
).aggregate(
  (jvmProjects).map(p => p: ProjectReference) : _*
)
