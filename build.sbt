name := "scrimage-lib"

version := "0.3-SNAPSHOT"

organization := "com.billding"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.4.4"

//resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

val monocleVersion = "1.4.0-M1"

//classpath.addRepository("https://oss.sonatype.org/content/repositories/snapshots")
libraryDependencies ++= Seq(
  "com.sksamuel.scrimage" %% "scrimage-core" % "3.0.0-SNAPSHOT",
  "com.sksamuel.scrimage" %% "scrimage-io-extra" % "2.1.7",
  "com.sksamuel.scrimage" %% "scrimage-filters" % "2.1.7",
  "com.lihaoyi" %% "ammonite-ops" % "0.7.8",
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  "org.typelevel" %% "cats" % "0.9.0"
)
