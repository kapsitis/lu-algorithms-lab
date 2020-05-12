name := "discrete-lab03"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.1"
libraryDependencies += "com.vladsch.flexmark" % "flexmark-all" % "0.35.10"


testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")


// // https://mvnrepository.com/artifact/com.typesafe.play/play-json
// libraryDependencies += "com.typesafe.play" %% "play-json" % "2.8.1"
// // https://mvnrepository.com/artifact/org.scalafx/scalafx
// libraryDependencies += "org.scalafx" %% "scalafx" % "12.0.2-R18"
// // https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor
// libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.6.3"
// // https://mvnrepository.com/artifact/io.socket/socket.io-client
// libraryDependencies += "io.socket" % "socket.io-client" % "1.0.0"
// libraryDependencies += "com.corundumstudio.socketio" % "netty-socketio" % "1.7.12"
// libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.28"



