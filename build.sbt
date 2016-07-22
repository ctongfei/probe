name := "probe"
organization := "me.tongfei"
version := "0.6.6-SNAPSHOT"
scalaVersion := "2.11.8"
isSnapshot := true

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "de.bwaldvogel" % "liblinear" % "1.95"

scalacOptions in ThisBuild ++= Seq("-optimise", "-Yclosure-elim", "-Yinline", "-deprecation", "-Yinline-warnings")

autoAPIMappings := true

scalacOptions in (Compile, doc) += "-diagrams"


publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false
