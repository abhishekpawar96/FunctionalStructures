name := "Functional Type Classes"

version := "0.1"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

scalaVersion := "2.13.0"

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

scalacOptions += "-Ymacro-annotations"