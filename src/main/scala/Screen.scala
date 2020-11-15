package xyz.hyperreal.basic

import java.io.OutputStream

import scala.swing.{Color, Graphics2D, Panel, Rectangle}
import scala.swing.Swing._
import scala.swing.event.{Event, KeyTyped}

case class Keyboard(c: Char) extends Event

class Screen(width: Int, height: Int, pixel: Int, oncolor: Color, offcolor: Color, fnt: BitmapFont) extends Panel {
  private val cwidth = width / 8
  private val cheight = height / fnt.height
  private val screen = Array.fill[Boolean](height, width)(false)
  private val text = Array.fill[Char](cheight, cwidth)(' ')
  private var showcursor = true
  private var cx = 0
  private var cy = 0

  focusable = true
  requestFocus
  preferredSize = (width * pixel, height * pixel)
  background = offcolor
  listenTo(keys)

  reactions += {
    case KeyTyped(_, c, _, _) => publish(Keyboard(c))
  }

  val out: OutputStream = (b: Int) => putchar(b.toChar)

  override def paintComponent(g: Graphics2D): Unit = {
    def paintPixel(x: Int, y: Int, on: Boolean): Unit = {
      g.setColor(if (on) oncolor else offcolor)
      g.fillRect(x * pixel, y * pixel, pixel - 1, pixel - 1)
    }

    super.paintComponent(g)

    for (i <- 0 until width; j <- 0 until height)
      paintPixel(i, j, screen(j)(i))

    if (showcursor) {
      val x = cx * 8
      val y = cy * fnt.height

      for (i <- x until x + 8; j <- y until y + fnt.ascent)
        paintPixel(i, j, !screen(j)(i))
    }
  }

  def cursonOn_=(on: Boolean): Unit = {
    showcursor = on
    repaint()
  }

  def cursonOn: Boolean = showcursor

  def linefeedNoRepaint(): Unit = {
    Array.copy(screen, fnt.height, screen, 0, height - fnt.height)

    for (i <- 0 until fnt.height)
      screen(height - fnt.height + i) = Array.fill[Boolean](width)(false)

    Array.copy(text, 1, text, 0, cheight - 1)
    text(cheight - 1) = Array.fill[Char](cwidth)(' ')
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
          repaint()
        }
      case '\r' =>
        cx = 0
        repaint()
      case '\n' =>
        if (cy == cheight - 1)
          linefeedNoRepaint()
        else
          cy += 1

        cx = 0
        repaint()
      case _ if c < ' ' || c > '~' => putchar('?')
      case _ =>
        char(cx, cy, c)

        if (cx == cwidth - 1) {
          if (cy == cheight - 1)
            linefeedNoRepaint()
          else
            cy += 1

          cx = 0
        } else
          cx += 1

        repaint()
    }

  def at(x: Int, y: Int): Char = {
    require(0 <= x && x <= cwidth, s"x out of range: $x")
    require(0 <= y && y <= cheight, s"y out of range: $y")

    text(y)(x)
  }

  def char(x: Int, y: Int, c: Char): Unit = {
    require(0 <= x && x <= cwidth, s"x out of range: $x")
    require(0 <= y && y <= cheight, s"y out of range: $y")

    text(y)(x) = c
    drawChar(x, y, c)
  }

  def string(x: Int, y: Int, s: String): Unit = {
    require(0 <= x && x <= cwidth, s"x out of range: $x")
    require(0 <= y && y <= cheight, s"y out of range: $y")

    for (i <- 0 until s.length)
      char(x + i, y, s(i))
  }

  def drawPixelNoRepaint(x: Int, y: Int, on: Boolean): Unit = screen(y)(x) = on

  def drawPixel(x: Int, y: Int, on: Boolean): Unit = {
    require(0 <= x && x <= width, s"x out of range: $x")
    require(0 <= y && y <= height, s"y out of range: $y")

    drawPixelNoRepaint(x, y, on)
    repaint(new Rectangle(x * pixel, y * pixel, pixel, pixel))
  }

  def drawChar(x: Int, y: Int, c: Char): Unit = {
    require(0 <= x && x <= cwidth, s"x out of range: $x")
    require(0 <= y && y <= cheight, s"y out of range: $y")

    val bitmap = fnt.lookup(c)

    for (i <- 0 until 8; j <- 0 until fnt.height)
      drawPixelNoRepaint(x * 8 + i, y * fnt.height + j, bitmap(j)(i))

    repaint(new Rectangle(x * pixel * 8, y * pixel * fnt.height, pixel * 8, pixel * fnt.height))
  }
}
