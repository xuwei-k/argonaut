import sbt._
import Keys._
import Tools.onVersionTask

object ScalaSettings {
  type Sett = Def.Setting[_]

  val on210or211 = Seq("-Yinline-warnings", "-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps")

  lazy val all: Seq[Sett] = Seq(
    scalaVersion := "2.11.1"
  , crossScalaVersions := "2.10.4" :: scalaVersion.value :: Nil
  , fork in test := true
  , scalacOptions += "-Xlint"
  , scalacOptions <++= onVersionTask(
      all = Seq("-deprecation", "-unchecked", "-optimise")
    , on210 = on210or211
    , on211 = on210or211
    )
  )
}
