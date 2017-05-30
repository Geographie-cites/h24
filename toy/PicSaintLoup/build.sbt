name := "PicSaintLoup"

version := "1.0"

scalaVersion := "2.12.2"

resolvers += Resolver.bintrayRepo("content/netlogo", "NetLogo-JVM")
libraryDependencies ++= Seq(
  "org.nlogo" % "netlogo" % "6.0.1-7243aaa" intransitive,
  "org.picocontainer" % "picocontainer" % "2.13.6",
  "org.ow2.asm" % "asm-all" % "5.0.3",
  "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6",
  "org.parboiled" %% "parboiled" % "2.1.4")
