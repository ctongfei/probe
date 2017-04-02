import sbt.Keys._

lazy val commonSettings = Seq(
	name := "probe",
	organization := "edu.jhu.hlt",
	version := "0.9.0",
	scalaVersion := "2.11.8",
	isSnapshot := false,

	resolvers += Resolver.sonatypeRepo("snapshots"),
	resolvers += "Artifactory Realm" at "http://sparsity.ad.hltcoe.jhu.edu:8081/artifactory/ivy-repo",

	libraryDependencies += "de.bwaldvogel" % "liblinear" % "1.95",
	libraryDependencies += "edu.jhu.hlt" %% "granite" % "4.12.0" % Test,

	scalacOptions in ThisBuild ++= Seq("-optimise", "-Yclosure-elim", "-Yinline", "-deprecation", "-Yinline-warnings"),

	autoAPIMappings := true,
	scalacOptions in (Compile, doc) += "-diagrams",
	credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),

	publishMavenStyle := true,

	publishTo := {
		if (isSnapshot.value) Some("snapshots" at "http://sparsity.ad.hltcoe.jhu.edu:8081/artifactory/libs-snapshot-local")
		else Some("releases" at "http://sparsity.ad.hltcoe.jhu.edu:8081/artifactory/libs-release-local")
	},

	publishArtifact in Test := false
)

lazy val core = (project in file("core")).settings(commonSettings: _*).settings(
	name := "probe"
)

lazy val search = (project in file("search")).settings(commonSettings: _*)
	.dependsOn(core)
	.settings(
		name := "probe-search",
		libraryDependencies ++= Seq(
			"me.tongfei" %  "progressbar" % "0.5.3",
			"me.tongfei" %% "poly-io" % "0.3.2",
	    "org.apache.lucene" % "lucene-core" % "6.5.0",
      "org.apache.lucene" % "lucene-analyzers-common" % "6.5.0",
      "org.apache.lucene" % "lucene-queryparser" % "6.5.0",
      "org.apache.lucene" % "lucene-backward-codecs" % "6.5.0",
      "com.lihaoyi" %% "fastparse" % "0.3.7"
		)
	)
