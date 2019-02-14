val enumeratum = "1.5.13"
val enumeratumCirce = "1.5.18"

lazy val rsDependencies = Seq(
  "com.beachape" %% "enumeratum" % enumeratum,
  "com.beachape" %% "enumeratum-circe" % enumeratumCirce,
  "com.slamdata" %% "matryoshka-core" % "0.21.3"
)
lazy val database = Seq(
  "org.scalikejdbc"     %% "scalikejdbc-async" % "0.11.+",
  "org.scalikejdbc"     %% "scalikejdbc-config" % "3.3.1",
  "com.github.mauricio" %% "postgresql-async"  % "0.2.+",
  "org.postgresql"      % "postgresql"         % "42.2.+"
)

lazy val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.8"

lazy val rsResolvers = Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  "restful-scala" at "https://dl.bintray.com/restfulscala/maven",
  "nexus-snapshots" at "http://nexus-repo.bhs.lan:8081/nexus/content/repositories/snapshots/",
  "nexus-releases" at "http://nexus-repo.bhs.lan:8081/nexus/content/repositories/releases/"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ypartial-unification",
  "-Yno-adapted-args",
  "-Xfatal-warnings",
  "-Xfuture"
)

organization := "slasch"
name := "scalaua2019"
scalaVersion := "2.12.8"
resolvers ++= rsResolvers
libraryDependencies ++= (rsDependencies ++ database)
addCompilerPlugin(kindProjector)
