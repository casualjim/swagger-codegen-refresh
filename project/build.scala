import sbt._
import sbt.Keys._

object RefreshBuild extends Build {
  lazy val root = Project("codegen-refresh", file("."))
}
