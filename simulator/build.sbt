import AssemblyKeys._

assemblySettings

name := "cilib-simulator"

description := "Simulator environment fo running experiments using the CIlib library"

scalacOptions += "-deprecation"

parallelExecution in Test := false

mainClass := Some("simulator.Main")

libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.10-M1" % "test",
    "org.mockito" % "mockito-all" % "1.8.4" % "test",
    "org.hamcrest" % "hamcrest-all" % "1.1" % "test"
)

autoScalaLibrary := false

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  deps :+ ("org.scala-lang" % "scala-compiler" % sv) :+ ("org.scala-lang" % "jline" % sv)
}

// jansi is already packaged up inside org.scala-lang/jline
excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter { c => List("jansi") exists { c.data.getName contains _ } }
}

resourceDirectory in Test <<= baseDirectory { _ / "simulator" }
