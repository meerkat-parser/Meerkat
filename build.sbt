
name := "Meerkat"

organization := "com.meerkat"

version := "0.1.0"

scalaVersion := "2.11.6"

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "macros"

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
	"junit" % "junit" % "4.11",
	"com.google.guava" % "guava-testlib" % "18.0",
	"commons-io" % "commons-io" % "2.4",
	"org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0"
)

// SBT Eclipse configuration

EclipseKeys.eclipseOutput in Compile := Some("bin/main/scala")
EclipseKeys.eclipseOutput in Test := Some("bin/test/scala")

EclipseKeys.withSource := true

