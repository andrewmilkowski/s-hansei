import sbt._

class SHanseiProject(info : ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins
{
  val sxr = compilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.6")
  val continuations = compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.8.0")
  override def compileOptions = CompileOption("-P:continuations:enable") :: CompileOption("-P:sxr:base-directory:" + mainScalaSourcePath.asFile.getAbsolutePath) :: super.compileOptions.toList

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5" % "test"
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test"
}
