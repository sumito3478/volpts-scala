import sbt._
import Keys._

object Build extends Build {
  // TODO: I've added options/dependencies that is not needed yet, so cleaning up is necessary in the future.
  // TODO: Create a project for sbt plugin - need to avoid a circular dependency, or prepare a bootstrap binary?

  lazy val defaultSettings = Seq(
    javaOptions := Seq("-Xmx1024m"),
    organization := "info.sumito3478",
    scalaVersion := "2.10.1",
    crossScalaVersions := Seq("2.10.1", "2.11.0-M2"),
    autoCompilerPlugins := true,
    scalacOptions ++= Seq(
      "-target:jvm-1.7",
      "-deprecation",
      "-unchecked"),
    resolvers ++= Seq(
      "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots",
      "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "1.2.+",
      "org.scalatest" %% "scalatest" % "2.0.+" % "test"))

  lazy val volpts = Project("volpts", file(".")).settings(defaultSettings: _*).aggregate(
    corelib, meta, collection, parsing, compiler)

  lazy val compiler = Project("volpts-compiler", file("compiler")).settings(defaultSettings: _*).settings(sbtantlr.SbtAntlrPlugin.antlrSettings: _*).settings(
    libraryDependencies ++= Seq(
      "com.assembla.scala-incubator" %% "graph-dot" % "1.6.+",
      "org.scalanlp" %% "breeze-core" % "0.2.+",
      "org.ow2.asm" % "asm-all" % "4.+")).
    dependsOn(parsing, corelib)

  lazy val parsing = Project("volpts-parsing", file("parsing")).settings(defaultSettings: _*).settings(
    libraryDependencies ++= Seq(
      "com.assembla.scala-incubator" %% "graph-dot" % "1.6.+",
      "org.scalanlp" %% "breeze-core" % "0.2.+",
      "org.ow2.asm" % "asm-all" % "4.+")).
    dependsOn(collection, meta, corelib)

  lazy val collection = Project("volpts-collection", file("collection")).settings(defaultSettings: _*).settings(
    libraryDependencies ++= Seq(
      "com.assembla.scala-incubator" %% "graph-core" % "1.6.+")).
    dependsOn(meta, corelib)

  lazy val meta = Project("volpts-meta", file("meta")).settings(defaultSettings: _*).settings(
    libraryDependencies <++= scalaVersion(v => Seq(
      "org.scala-lang" % "scala-compiler" % v))).
    dependsOn(corelib)

  lazy val corelib = Project("volpts-corelib", file("corelib")).settings(defaultSettings: _*).settings(
    libraryDependencies ++= Seq(
      "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.+",
      "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.+",
      "com.ibm.icu" % "icu4j" % "51.1",
      "com.google.guava" % "guava" % "14.+"))
}
