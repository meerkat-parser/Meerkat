
name in ThisBuild := "Meerkat"

organization in ThisBuild := "com.meerkat"

version in ThisBuild := "0.1.0"

scalaVersion in ThisBuild := "2.11.6"

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
	"junit" % "junit" % "4.11",
	"com.google.guava" % "guava-testlib" % "18.0",
	"commons-io" % "commons-io" % "2.4",
	"org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0"
)