package xyz.hyperreal.basic

import java.io.PrintStream

import scala.swing._
import scala.util.Using

object Main extends App {

  val vdm1Font = new BitmapFont(VDM1.font)

  object UI extends MainFrame {
    val screen = new Screen(512, 192, 4, new Color(0, 255, 0), new Color(0, 0, 0), vdm1Font)
    val out = new PrintStream(screen.out)
    val linebuf = new StringBuilder
    var command: String => Unit = _ => ()

    contents = screen
    listenTo(screen)

    reactions += {
      case Keyboard(c) =>
        c match {
          case '\n' =>
            val s = linebuf.toString

            screen.putchar(c)
            linebuf.clear
            command(s)
          case '\b' =>
            if (linebuf.nonEmpty) {
              screen.putchar(c)
              linebuf.replace(linebuf.length - 1, linebuf.length, "")
            }
          case _ =>
            screen.putchar(c)
            linebuf += c
        }
    }
  }

  var interp: Interpreter = _

  load("")

  def load(code: String): Unit = {
    interp = new Interpreter(UI.out)
    interp.load(new BasicParser().parseProgram(code))
  }

  UI.title = "Tiny Basic"
  UI.out.print(s"""Welcome to Tiny Basic v0.1.0
                  |Type commands or enter program code.
                  |
                  |] """.stripMargin)
  UI.visible = true
  UI.command = c => {
    val a = c.trim.split("\\s+")

    a(0) = a(0).toUpperCase

    a.toList match {
      case List("") => UI.out.print("] ")
      case List("LOAD", file) =>
        load(Using(io.Source.fromFile(file))(_.mkString).get)
        UI.out.print("\n Ok\n] ")
      case List("LIST") =>
        interp.list(None, None)
        UI.out.print("\n Ok\n] ")
      case List("RUN") =>
        new Thread {
          override def run(): Unit = {
            interp.run(None)
            UI.out.print("\n Ok\n] ")
          }
        }.start()
      case com :: _ =>
        UI.out.println(s"unrecognized command: $com")
        UI.out.print("\n] ")
    }
  }

}
