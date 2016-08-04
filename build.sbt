name := "probe"
organization := "me.tongfei"
version := "0.7.1-SNAPSHOT"
scalaVersion := "2.11.8"
isSnapshot := true

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "Artifactory Realm" at "http://sparsity.ad.hltcoe.jhu.edu:8081/artifactory/ivy-repo"

libraryDependencies += "de.bwaldvogel" % "liblinear" % "1.95"
libraryDependencies += "me.tongfei" %% "granite" % "4.10.3-SNAPSHOT" % Test

scalacOptions in ThisBuild ++= Seq("-optimise", "-Yclosure-elim", "-Yinline", "-deprecation", "-Yinline-warnings")

autoAPIMappings := true

scalacOptions in (Compile, doc) += "-diagrams"


credentials += Credentials(Path.userHome/".ivy2"/".credentials")

publishMavenStyle := true

publishTo := {
	if (isSnapshot.value) Some("snapshots" at "http://sparsity.ad.hltcoe.jhu.edu:8081/artifactory/libs-snapshot-local")
	else Some("releases" at "http://sparsity.ad.hltcoe.jhu.edu:8081/artifactory/libs-release-local")
}

publishArtifact in Test := false
