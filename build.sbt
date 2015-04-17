name := "feature"

organization := "me.tongfei"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.4"

isSnapshot := true

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions in ThisBuild ++= Seq("-optimise", "-Yclosure-elim", "-Yinline")

autoAPIMappings := true

scalacOptions in (Compile, doc) += "-diagrams"
