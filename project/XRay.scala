	import sbt._
	import Keys._
	import Configurations.CompilerPlugin

object XRay extends Build
{
	lazy val main = Project("sxr", file(".")) settings(
		name := "sxr",
		organization in ThisBuild := "org.scala-sbt.sxr",
		version in ThisBuild := "0.3.1-SNAPSHOT",
		scalaVersion in ThisBuild := "2.10.2",
		scalacOptions += "-deprecation",
		ivyConfigurations += js,
		exportJars := true,
		libraryDependencies ++= dependencies,
		libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
		jqueryAll := target.value / "jquery-all.js",
		combineJs := combineJquery(update.value, jqueryAll.value, streams.value.log),
		resourceGenerators in Compile <+= combineJs
	)

	lazy val test = project.dependsOn(main % CompilerPlugin).settings(testProjectSettings: _*)

	lazy val testLink = project.dependsOn(main % CompilerPlugin, test).settings(testProjectSettings: _*).settings(
		scalacOptions += {
			val _ = clean.value
			val linkFile = target.value / "links"
			val testLinkFile = classDirectory.in(test, Compile).value.getParentFile / "classes.sxr"
			IO.write(linkFile, testLinkFile.toURI.toURL.toExternalForm)
			s"-P:sxr:link-file:$linkFile"
		}
	)

	def testProjectSettings = Seq(
		autoCompilerPlugins := true,
		compile in Compile <<= (compile in Compile).dependsOn(clean),
		Keys.test := {
			val _ = (compile in Compile).value
			val out = (classDirectory in Compile).value
			val base = baseDirectory.value
			checkOutput(out / "../classes.sxr", base / "expected", streams.value.log)
		}
	)

	val js = config("js").hide
	
	val combineJs = TaskKey[Seq[File]]("combine-js")
	val jqueryAll = SettingKey[File]("jquery-all")
	
	val jquery_version = "1.3.2"
	val jquery_scrollto_version = "1.4.2"
	val jquery_qtip_version = "2.1.1"

	def dependencies = Seq(
		"jquery" % "jquery"          % jquery_version          % "js->default" from ("https://code.jquery.com/jquery-" + jquery_version + ".min.js"),
		"jquery" % "jquery-scrollto" % jquery_scrollto_version % "js->default" from ("http://cdn.jsdelivr.net/jquery.scrollto/" + jquery_scrollto_version + "/jquery.scrollTo.min.js"),
		"jquery" % "jquery-qtip"     % jquery_qtip_version     % "js->default" from ("http://qtip2.com/v/" + jquery_qtip_version + "/jquery.qtip.min.js")
	)

	def combineJquery(report: UpdateReport, jsOut: File, log: Logger): Seq[File] =
	{
		IO.delete(jsOut)
		inputs(report) foreach { in => appendJs(in, jsOut) }
		log.info("Wrote combined js to " + jsOut.getAbsolutePath)
		Seq(jsOut)
	}
	def inputs(report: UpdateReport) = report.select( configurationFilter(js.name)) sortBy { _.name }
	def appendJs(js: File, to: File): Unit =
		Using.fileInputStream(js) { in =>
			Using.fileOutputStream(append = true)(to) { out => IO.transfer(in, out) }
		}


	def checkOutput(sxrDir: File, expectedDir: File, log: Logger) {
		val actual = filesToCompare(sxrDir)
		val expected = filesToCompare(expectedDir)
		val actualRelative = actual._2s
		val expectedRelative = expected._2s
		if(actualRelative != expectedRelative) {
			val actualOnly = actualRelative -- expectedRelative
			val expectedOnly = expectedRelative -- actualRelative
			def print(n: Iterable[String]): String = n.mkString("\n\t", "\n\t", "\n")
		 	log.error(s"Actual filenames not expected: ${print(actualOnly)}Expected filenames not present: ${print(expectedOnly)}")
			error("Actual filenames differed from expected filenames.")
		}
		val different = actualRelative filterNot { relativePath =>
			val actualFile = actual.reverse(relativePath).head
			val expectedFile = expected.reverse(relativePath).head
			val same = sameFile(actualFile, expectedFile)
			if(!same) log.error(s"$relativePath\n\t$actualFile\n\t$expectedFile")
			same
		}
		if(different.nonEmpty)
			error("Actual content differed from expected content")
	}
	def filesToCompare(dir: File): Relation[File,String] = {
		val mappings = dir ** ("*.html" | "*.index") x relativeTo(dir)
		Relation.empty ++ mappings
	}
	def sameFile(actualFile: File, expectedFile: File): Boolean =
		IO.read(actualFile) == IO.read(expectedFile)
}
