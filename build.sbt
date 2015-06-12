name := "feature"

organization := "me.tongfei"

version := "0.1.2-SNAPSHOT"

scalaVersion := "2.11.6"

isSnapshot := true

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions in ThisBuild ++= Seq("-optimise", "-Yclosure-elim", "-Yinline")

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

pomExtra :=
  <url>http://github.com/ctongfei/feature</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:ctongfei/feature.git</url>
      <connection>scm:git:git@github.com:ctongfei/feature.git</connection>
    </scm>
    <developers>
      <developer>
        <id>ctongfei</id>
        <name>Tongfei Chen</name>
        <url>http://tongfei.me/</url>
      </developer>
    </developers>

