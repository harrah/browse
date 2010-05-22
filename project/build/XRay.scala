import sbt._
import FileUtilities.{appendStream, clean => delete, readStream, transfer}

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
    delete(jqueryAll, log)
    val jsIn =  jsManaged.get.toList sort { _.name < _.name }
    if(jsIn.isEmpty) Some("No javascript files found.  Please run 'update'.") else combine(jsIn)
  }
  def combine(jsIn: Seq[Path]) =
    jsIn flatMap appendJs match {
      case Seq() => log.info("Wrote combined js to " + jqueryAll.absolutePath); None
      case s => Some(s mkString ",")
    }
  def appendJs(js: Path): Option[String] =
    readStream(js asFile, log) { in =>
      appendStream(jqueryAll.asFile, log) { out => transfer(in, out, log) }
    }

  override def mainResources = super.mainResources +++ jqueryAll
    override protected def packageAction = super.packageAction dependsOn(combineJquery)

  val snapshots = ScalaToolsSnapshots
  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}