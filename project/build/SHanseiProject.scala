import sbt._

class SHanseiProject(info : ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = CompileOption("-Xplugin:lib/selectivecps-plugin.jar") :: super.compileOptions.toList
  val specs = "org.scala-tools.testing" % "specs" % "1.6.0" % "test->default" from "http://specs.googlecode.com/svn/maven2/org/scala-tools/testing/specs/1.6.0-2.8.0/specs-1.6.0-2.8.0.jar"
}
