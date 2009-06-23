import sbt._

class XRay(info: ProjectInfo) extends DefaultProject(info) {
	val jsConfig = config("js")
	val defaultConfig = Configurations.Default
	val jquery_version = "1.3.2"
	val jquery = "jquery" % "jquery" % jquery_version % "js->default" from("http://jqueryjs.googlecode.com/files/jquery-" + jquery_version + ".min.js")
	
	override def mainResources = super.mainResources +++ configurationClasspath(jsConfig)
	override def crossScalaVersions = Set("2.7.2", "2.7.3", "2.7.4", "2.7.5")
}
