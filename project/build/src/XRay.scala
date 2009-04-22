import sbt._

class XRay(info: ProjectInfo) extends DefaultProject(info) {
	val resources_managed = path("src_managed") / "main" / "resources"
	val jquery_version = "1.3.2"
	val jquery_js = resources_managed / ("jquery-" + jquery_version + ".min.js")
	
	lazy val jquery = fileTask(jquery_js :: Nil) {
		import FileUtilities._ 
		val remote_js = new java.net.URL("http://jqueryjs.googlecode.com/files/jquery-" + jquery_version + ".min.js")
		
		readStream(remote_js, log) { inputStream =>
			writeStream(jquery_js.asFile, log) { outputStream => 
				transfer(inputStream, outputStream, log)
			}
		}
	}
	override def mainResources = super.mainResources +++ descendents(resources_managed ##, "*")
	override def packageAction = super.packageAction dependsOn jquery
}
