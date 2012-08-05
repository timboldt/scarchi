name := "Archipeligo"

version := "1.0"

scalaVersion := "2.9.1"
   
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
    
libraryDependencies ++= Seq (
  "com.typesafe.akka" % "akka-actor" % "2.0.1",
  "org.scalatest" %% "scalatest" % "1.7.2" % "test",
  "org.scala-lang" % "scala-swing" % "2.9.1"
)
