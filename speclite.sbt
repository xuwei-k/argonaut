val scalazV = "7.1.0-M7"
val specLiteURL = s"https://raw.github.com/scalaz/scalaz/v${scalazV}/tests/src/test/scala/scalaz/SpecLite.scala"
val specLite = SettingKey[List[String]]("specLite")

specLite := {
  println(s"downloading from ${specLiteURL}")
  val lines = IO.readLinesURL(url(specLiteURL))
  println("download finished")
  lines
}

def specLiteFile(dir: File, contents: List[String]): File = {
  val file = dir / "SpecLite.scala"
  IO.writeLines(file, contents)
  file
}

sourceGenerators in Test += task{
  Seq(specLiteFile((sourceManaged in Test).value, specLite.value))
}
