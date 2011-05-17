	import sbt._
	import Keys._

object XRay extends Build
{
	lazy val projects = Seq(main)
	lazy val main = Project("sxr", file(".")) settings(
		name := "sxr",
		organization := "org.scala-tools.sxr",
		version := "0.2.7",
		scalaVersion := "2.9.0",
		crossScalaVersions += "2.8.1",
		ivyConfigurations += js,
		libraryDependencies ++= dependencies,
		jqueryAll <<= target(_ / "jquery-all.js"),
		combineJs <<= combineJquery,
		resourceGenerators in Compile <+= combineJs.identity,
		credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
		publishTo := Some( "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/" )
	)

	val js = config("js") hide
	
	val combineJs = TaskKey[Seq[File]]("combine-js")
	val jqueryAll = SettingKey[File]("jquery-all")
	
	val jquery_version = "1.3.2"
	val jquery_scrollto_version = "1.4.2"
	val jquery_qtip_version = "1.0.0-rc3"

	def dependencies = Seq(
		"jquery" % "jquery"          % jquery_version          % "js->default" from ("http://jqueryjs.googlecode.com/files/jquery-" + jquery_version + ".min.js"),
		"jquery" % "jquery-scrollto" % jquery_scrollto_version % "js->default" from ("http://flesler-plugins.googlecode.com/files/jquery.scrollTo-" + jquery_scrollto_version + "-min.js"),
		"jquery" % "jquery-qtip"     % jquery_qtip_version     % "js->default" from ("http://craigsworks.com/projects/qtip/packages/1.0.0-rc3/jquery.qtip-" + jquery_qtip_version + ".min.js")
	)

	lazy val combineJquery = (update, jqueryAll, streams) map { (report, jsOut, s) =>
		IO.delete(jsOut)
		inputs(report) foreach { in => appendJs(in, jsOut) }
		s.log.info("Wrote combined js to " + jsOut.getAbsolutePath)
		Seq(jsOut)
	}
	def inputs(report: UpdateReport) = report.select( configurationFilter(js.name)) sortBy { _.name }
	def appendJs(js: File, to: File): Unit =
		Using.fileInputStream(js) { in =>
			Using.fileOutputStream(append = true)(to) { out => IO.transfer(in, out) }
		}
}
