	import sbt._
	import Keys._
	import Configurations.CompilerPlugin

object XRay extends Build
{
	lazy val main = Project("sxr", file(".")) settings(
		name := "sxr",
		organization in ThisBuild := "org.scala-sbt.sxr",
		version in ThisBuild := "0.3.1-SNAPSHOT",
		scalaVersion in ThisBuild := "2.10.3",
		scalacOptions += "-deprecation",
		exportJars := true,
		libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
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
			sys.error("Actual filenames differed from expected filenames.")
		}
		val different = actualRelative filterNot { relativePath =>
			val actualFile = actual.reverse(relativePath).head
			val expectedFile = expected.reverse(relativePath).head
			val same = sameFile(actualFile, expectedFile)
			if(!same) log.error(s"$relativePath\n\t$actualFile\n\t$expectedFile")
			same
		}
		if(different.nonEmpty)
			sys.error("Actual content differed from expected content")
	}
	def filesToCompare(dir: File): Relation[File,String] = {
		val mappings = dir ** ("*.html" | "*.index") x relativeTo(dir)
		Relation.empty ++ mappings
	}
	def sameFile(actualFile: File, expectedFile: File): Boolean =
		IO.read(actualFile) == IO.read(expectedFile)
}
