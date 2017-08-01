import sbt._

// MyBuild extends Build?
object Dependencies {

  // Declare a project in the root directory of the build with ID "root".
  lazy val root = Project("root", file("."))
  .dependsOn(RootProject(uri("git://github.com/ruippeixotog/scala-scraper.git") /*, "scala-scraper"*/))

  // // Declare an execution dependency on sub1.
  // aggregate(sub1)

  // // Declare a project with ID 'sub1' in directory 'a'.
  // // Declare a classpath dependency on sub2 in the 'test' configuration.
  // lazy val sub1: Project = Project("sub1", file("a")) dependsOn(sub2 % "test")

  // // Declare a project with ID 'sub2' in directory 'b'.
  // // Declare a configuration dependency on the root project.
  // lazy val sub2 = Project("sub2", file("b"), delegates = root :: Nil)


  // // not remotely synced with build.sbt, is this used?
  // lazy val scalaTest = "org.sca1atest" %% "scalatest" % "3.0.1"

}
