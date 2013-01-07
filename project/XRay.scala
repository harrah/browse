	import sbt._
	import Keys._

object XRay extends Build
{
	lazy val main = Project("sxr", file(".")) settings(
		name := "sxr",
		organization := "org.scala-sbt.sxr",
		version := "0.3.0-SNAPSHOT",
		scalaVersion := "2.10.0",
		crossScalaVersions ++= Seq("2.9.2"),
		ivyConfigurations += js,
		libraryDependencies ++= dependencies,
		libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "provided"),
		jqueryAll <<= target(_ / "jquery-all.js"),
		combineJs <<= combineJquery,
		resourceGenerators in Compile <+= combineJs.identity
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
