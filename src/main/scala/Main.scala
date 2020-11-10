package xyz.hyperreal.basic

import java.awt.geom.Rectangle2D

import scala.swing._
import scala.swing.Swing._
import scala.util.Using

object Main extends App {

  val atariFont =
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
    private val screen = Array.fill[Boolean](HEIGHT, WIDTH)(false)
    private val text = Array.fill[Char](CHEIGHT, CWIDTH)(' ')
    private var showcursor = true
    private var cx = 0
    private var cy = 0

    preferredSize = (WIDTH * PIXEL, HEIGHT * PIXEL)

    override def paintComponent(g: Graphics2D): Unit = {
      def paintPixel(x: Int, y: Int, on: Boolean): Unit = {
        g.setColor(if (on) ON else OFF)
        g.fillRect(x * PIXEL, y * PIXEL, PIXEL, PIXEL)
      }

      super.paintComponent(g)

      for (i <- 0 until WIDTH; j <- 0 until HEIGHT)
        paintPixel(i, j, screen(j)(i))

      if (showcursor) {
        val x = cx * CHAR
        val y = cy * CHAR

        for (i <- x until x + CHAR; j <- y until y + CHAR)
          paintPixel(i, j, !screen(j)(i))
      }
    }

    def cursonOn_=(on: Boolean): Unit = {
      showcursor = on
      repaint()
    }

    def cursonOn: Boolean = showcursor

    def linefeedNoRepaint(): Unit = {
      Array.copy(screen, CHAR, screen, 0, HEIGHT - CHAR)

      for (i <- 0 until CHAR)
        screen(HEIGHT - CHAR + i) = Array.fill[Boolean](WIDTH)(false)

      Array.copy(text, 1, text, 0, CHEIGHT - 1)
      text(CHEIGHT - 1) = Array.fill[Char](CWIDTH)(' ')
    }

    def position_=(p: (Int, Int)): Unit = {
      cx = p._1
      cy = p._2
      repaint()
    }

    def position: (Int, Int) = (cx, cy)

    def puts(s: String): Unit = s foreach putchar

    def putchar(c: Char): Unit =
      c match {
        case '\b' =>
          if (cx > 0) {
            cx -= 1
            char(cx, cy, ' ')
          }
        case '\r' =>
          cx = 0
          repaint()
        case '\n' =>
          if (cy == CHEIGHT - 1)
            linefeedNoRepaint()
          else
            cy += 1

          cx = 0
          repaint()
        case _ if c < ' ' || c > '~' => putchar('?')
        case _ =>
          char(cx, cy, c)

          if (cx == CWIDTH - 1) {
            if (cy == CHEIGHT - 1)
              linefeedNoRepaint()
            else
              cy += 1

            cx = 0
          } else
            cx += 1

          repaint()
      }

    def at(x: Int, y: Int): Char = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      text(y)(x)
    }

    def char(x: Int, y: Int, c: Char): Unit = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      text(y)(x) = c
      drawChar(x, y, c)
    }

    def string(x: Int, y: Int, s: String): Unit = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      for (i <- 0 until s.length)
        char(x + i, y, s(i))
    }

    def drawPixelNoRepaint(x: Int, y: Int, on: Boolean): Unit = screen(y)(x) = on

    def drawPixel(x: Int, y: Int, on: Boolean): Unit = {
      require(0 <= x && x <= WIDTH, s"x out of range: $x")
      require(0 <= y && y <= HEIGHT, s"y out of range: $y")

      drawPixelNoRepaint(x, y, on)
      repaint(new Rectangle(x * PIXEL, y * PIXEL, PIXEL, PIXEL))
    }

    def drawChar(x: Int, y: Int, c: Char): Unit = {
      require(0 <= x && x <= CWIDTH, s"x out of range: $x")
      require(0 <= y && y <= CHEIGHT, s"y out of range: $y")

      val bitmap = lookup(atariFont, c)

      for (i <- 0 until 8; j <- 0 until 8)
        drawPixelNoRepaint(x * CHAR + j, y * CHAR + i, if (((bitmap(i) >> (7 - j)) & 1) == 1) true else false)

      repaint(new Rectangle(x * PIXEL, y * PIXEL, PIXEL * CHAR, PIXEL * CHAR))
    }
  }

  UI.visible = true

  for (i <- 1 to 24)
    UI.screen.puts(s"testing $i\n")

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
