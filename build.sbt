name := "Faithful visualization of categorical datasets"

version := "1.0"

scalaVersion := "2.11.7"

// Set these to use the profiler
// fork in run := true
// javaOptions += "-Xshare:off"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

scalacOptions ++= Seq("-Xdisable-assertions")

scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/root-doc.txt")
  
scalacOptions in (Compile, doc) ++= Seq("-doc-title", "Faithful visualization")

libraryDependencies  ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "0.12",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.12",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scala-lang" % "scala-swing" % "2.11+",
  "org.jfree" % "jfreechart" % "1.0.19"
)


resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
