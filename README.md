Browsable Scala source code in HTML with:

- syntax highlighting
- types/applied implicits in tooltips
- references/definition highlighted on mouseover
- links to definition

See <http://harrah.github.com/browse/samples/index.html> for samples.

Still in development.  Bugs are features and features are accidental.

To build with sbt (see <https://github.com/harrah/xsbt/wiki/Setup> for setup instructions):

```
$ sbt "+ package"
```

This produces a compiler plugin in target/.

Usage

Add the following options to your compile command for your project:

```
  -Xplugin:<path-to-sxr>/sxr-0.2.7.jar
  -P:sxr:base-directory:<src-dir>
```

If you are using sbt (0.10), add sxr as a plugin and configure the sxr plugin:

```scala
addCompilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.7")

scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }
```

You will get a directory <classes-output>.sxr that mirrors the directory structure of your sources relative
to the specified base directory with one HTML file for each source file.  You can make simple
changes to the syntax highlighting in the style.css file in the root output directory.  The linked.js
file implements the highlighting of refererences, among other things.

Other options include specifying the output format and linking to other sxr sources.

To link to other sxr sources (produced with sxr 0.2.5 or later), follow these two steps.

1. put the URLs of the other sxr sources in a file, say 'sxr.links'.  The URLs should point to the base directories, not to 'index.html' or any specific file.
2. Specify the location of this file in the 'link-file' sxr option.  For example, in addition to the settings above, use:

```scala
scalacOptions <+= baseDirectory map { base =>
  val linkFile = base / "sxr.links"
  "-P:sxr:link-file:" + linkFile.getAbsolutePath)
}
```
