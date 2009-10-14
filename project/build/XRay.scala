import sbt._

class XRay(info: ProjectInfo) extends DefaultProject(info) {
  val jquery_version = "1.3.2"
  val js = config("js") hide
  val jquery = "jquery" % "jquery" % jquery_version % "js->default" from ("http://jqueryjs.googlecode.com/files/jquery-" + jquery_version + ".min.js")
  val jquery_scrollto_version = "1.4.2"
  val jquery_scrollto = "jquery" % "jquery-scrollto" % jquery_scrollto_version % "js->default" from 
    ("http://flesler-plugins.googlecode.com/files/jquery.scrollTo-" + jquery_scrollto_version + "-min.js")
  val jquery_qtip_version = "1.0.0-rc3"
  var jquery_qtip = "jquery" % "jquery-qtip" % jquery_qtip_version % "js->default" from
    ("http://craigsworks.com/projects/qtip/packages/1.0.0-rc3/jquery.qtip-" + jquery_qtip_version + ".min.js")
  val jsManaged = descendents(configurationPath(js) ##, "*.js")
  val jqueryAll = (outputPath ##) / "jquery-all.js"

  lazy val combineJquery = fileTask(jqueryAll from jsManaged) {
    import FileUtilities._ 
    FileUtilities.clean(jqueryAll, log)
    (jsManaged.get map { _.asFile } toList) sort { _.getName < _.getName } flatMap { js =>
      readStream(js, log) { in =>
        appendStream(jqueryAll.asFile, log) { out => transfer(in, out, log) }
      }
    } match {
      case Seq() => log.info("Wrote combined js to " + jqueryAll.asFile); None
      case s => Some(s mkString ",")
    }
  }

  override def mainResources = super.mainResources +++ jqueryAll
	override protected def packageAction = super.packageAction dependsOn(combineJquery)

  override def crossScalaVersions = Set("2.7.2", "2.7.3", "2.7.4", "2.7.5", "2.7.6")//, "2.8.0-SNAPSHOT")
  val snapshots = ScalaToolsSnapshots
  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}