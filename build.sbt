
name := "Meerkat"

organization := "org.meerkat"

version := "0.1.0"

scalaVersion := "2.12.1"

parallelExecution in Test := false

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "macros"

libraryDependencies ++= Seq(
	"org.scalactic" %% "scalactic" % "3.0.1",
	"org.scalatest" %% "scalatest" % "3.0.1" % "test",
	"com.google.guava" % "guava-testlib" % "18.0",
	"commons-io" % "commons-io" % "2.4",
	"org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0"
)

// SBT Eclipse configuration

EclipseKeys.eclipseOutput in Compile := Some("bin/main/scala")
EclipseKeys.eclipseOutput in Test := Some("bin/test/scala")

EclipseKeys.withSource := true

