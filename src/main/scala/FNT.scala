package xyz.hyperreal.basic

import scala.util.Using

class FNT(file: String) {

  private val fnt =
    Using(new java.io.FileInputStream(file))(_.readAllBytes).getOrElse(sys.error("couldn't load FNT file"))

  def lookup(char: Char): Vector[Seq[Boolean]] = {
    val ch = char.toInt

    require(0x20 <= ch && ch <= 0x7F, s"character out of range: $ch")

    val idx = if (ch <= 0x5F) ch - 0x20 else ch

    fnt.slice(idx * 8, idx * 8 + 8) map (b => for (i <- 7 to 0 by -1) yield if (((b >> i) & 1) == 1) true else false) toVector
  }

  def show(char: Char): Unit = println(lookup(char) map (r => r map (c => if (c) '*' else ' ') mkString) mkString "\n")

}
