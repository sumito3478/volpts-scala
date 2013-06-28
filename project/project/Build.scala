import sbt._

object Plugin extends Build {
  override lazy val projects = Seq(plugins)

  lazy val plugins = Project("plugins", file(".")).dependsOn(ensime, genIdea, sbtAntlr)

  lazy val ensime = uri("git://github.com/4e6/ensime-sbt-cmd.git#0.13.0-M1")

  lazy val genIdea = uri("git://github.com/mpeltonen/sbt-idea.git#sbt-0.13")

  lazy val sbtAntlr = uri("git://github.com/sumito3478/sbt-antlr.git#managed-source-dirs-fix")

  // sbteclipse 2.x does not support sbt 0.13, and sbteclipse 3.x is for sbt 0.13, but not ready for production use yet.
  // lazy val sbtelipse = ...
}
