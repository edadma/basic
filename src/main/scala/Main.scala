package xyz.hyperreal.basic

import java.awt.geom.Rectangle2D

import scala.swing._
import scala.swing.Swing._
import scala.util.Using

object Main extends App {

  val ataripl =
    Using(new java.io.FileInputStream("ATARI.FNT"))(_.readAllBytes).getOrElse(sys.error("couldn't load FNT file"))

  def lookup(fnt: Array[Byte], char: Char) = {
    val ch = char.toInt

    require(0x20 <= ch && ch <= 0x7F, s"character out of range: $ch")

    val idx = if (ch <= 0x5F) ch - 0x20 else ch

    fnt.slice(idx * 8, (idx + 8) * 8)
  }

  def show(fnt: Array[Byte], char: Char): Unit = {
    val bitmap = lookup(fnt, char)

    for (i <- 0 until 8) {
      for (j <- 7 to 0 by -1)
        print(if (((bitmap(i) >> j) & 1) == 1) '*' else ' ')

      println()
    }
  }

  object UI extends MainFrame {
    val screen = new AtariScreen

    title = "Atari 400"
    contents = screen
  }

  class AtariScreen extends Panel {
    private val ON = new Color(0, 255, 0)
    private val OFF = new Color(0, 0, 0)
    private val PIXEL = 4
    private val CHAR = 8
    private val WIDTH = 320
    private val CWIDTH = WIDTH / 8
    private val HEIGHT = 192
    private val CHEIGHT = HEIGHT / 8
    private val screen = Array.ofDim[Boolean](WIDTH, HEIGHT)
    private val text = Array.fill[Char](CWIDTH, CHEIGHT)(' ')
    private var cursoron = true
    private var cx = 0
    private var cy = 0

    preferredSize = (WIDTH * PIXEL, HEIGHT * PIXEL)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)

      for (i <- 0 until WIDTH; j <- 0 until HEIGHT) {
        g.setColor(if (screen(i)(j)) ON else OFF)
        g.fillRect(i * PIXEL, j * PIXEL, PIXEL, PIXEL)
      }

      if (cursoron)
        drawCursor(cx, cy)
    }

    def linefeed(): Unit = {}

    def puts(s: String): Unit = s foreach putchar

    def putchar(c: Char): Unit =
      c match {
        case '\b' =>
          if (cx > 0)
            cx -= 1
        case '\r' => cx = 0
        case '\n' =>
          if (cy == CHEIGHT - 1)
            linefeed()
          else
            cy += 1

          cx = 0
        case _ if c < ' ' || c > '~' => putchar('?')
        case _ =>
          char(cx, cy, c)

          if (cx == CWIDTH - 1) {
            if (cy == CHEIGHT - 1)
              linefeed()
            else
              cy += 1

            cx = 0
          } else
            cx += 1
      }

    def char(x: Int, y: Int, c: Char): Unit = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      text(x)(y) = c
      drawChar(x, y, c)
    }

    def string(x: Int, y: Int, s: String): Unit = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      for (i <- 0 until s.length)
        char(x + i, y, s(i))
    }

    def drawPixel(x: Int, y: Int, on: Boolean): Unit = {
      require(0 <= x && x <= WIDTH, s"x out of range: $x")
      require(0 <= y && y <= HEIGHT, s"y out of range: $y")

      screen(x)(y) = on
      repaint(new Rectangle(x * PIXEL, y * PIXEL, PIXEL, PIXEL))
    }

    def drawCursor(x: Int, y: Int): Unit = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      for (i <- 0 until 8; j <- 0 until 8)
        drawPixel(x * CHAR + j, y * CHAR + i, on = true)
    }

    def drawChar(x: Int, y: Int, c: Char): Unit = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      val bitmap = lookup(ataripl, c)

      for (i <- 0 until 8; j <- 0 until 8)
        drawPixel(x * CHAR + j, y * CHAR + i, if (((bitmap(i) >> (7 - j)) & 1) == 1) true else false)
    }
  }

  UI.visible = true
  UI.screen.puts(
    "Atari's entry into the home computing market put out some very capable machines with all sorts of hardware tricks (the creative geniuses behind it would go on to form Amiga).")

  //  show(ataripl, 'a')
//  show(ataripl, 'A')
//  show(ataripl, 'X')
//  show(ataripl, '0')
//  show(ataripl, '!')

  //  val program =
//    """
//      |100 LET a = 3  'assign 3 to the variable 'a'
//      |105 DIM b[10]
//      |106 b[0] = 4
//      |107 print b[0]
//      |110 PRINT "a is "; a; " and", "a + 1 is ";a*(4+5)
//      |115 print sqrt(a)
//      |120 END  'end program
//      |130 PRINT "this line doesn't get executed"
//      |""".stripMargin
//  val parser = new BasicParser
//  val interp = new Interpreter
//  val ast = BasicParser.parseProgram(program, parser)
//
//  interp.load(ast)
//  interp.list(None, None)
//  interp.run(None)

}
