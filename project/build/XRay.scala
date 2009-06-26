import sbt._

class XRay(info: ProjectInfo) extends DefaultProject(info) {
  val jquery_version = "1.3.2"
  val js = config("js") hide
  val jquery = "jquery" % "jquery" % jquery_version % "js->default" from ("http://jqueryjs.googlecode.com/files/jquery-" + jquery_version + ".min.js")
  override def mainResources = super.mainResources +++ descendents(configurationPath(js) ##, "*.js")

  override def crossScalaVersions = Set("2.7.2", "2.7.3", "2.7.4", "2.7.5")
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}