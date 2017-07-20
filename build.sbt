
name := "Meerkat"

organization := "org.meerkat"

version := "0.1.0"

scalaVersion := "2.12.2"

parallelExecution in Test := false

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "macros" / "scala"

libraryDependencies ++= Seq(
	"org.scalactic" %% "scalactic" % "3.0.1",
	"org.scalatest" %% "scalatest" % "3.0.1" % "test",
	"com.google.guava" % "guava-testlib" % "22.0",
	"commons-io" % "commons-io" % "2.4",
	"org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0"
)

// SBT Eclipse configuration
EclipseKeys.withSource := true

