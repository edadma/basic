package xyz.hyperreal.basic

import java.io.PrintStream

import scala.swing._

object Main extends App {

  val atariFont = new FNT("ATARI.FNT")

  object UI extends MainFrame {
    val screen = new Screen(320, 192, new Color(0, 255, 0), new Color(0, 0, 0), atariFont)
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

  val program =
    """100 FOR I = 1 TO 3
      |110   PRINT "I = "; I
      |120 NEXT I
      |130 END
      |""".stripMargin
  val parser = new BasicParser
  val interp = new Interpreter(UI.out)
  val ast = BasicParser.parseProgram(program, parser)

  interp.load(ast)

  UI.title = "Tiny Basic"
  UI.out.print(s"""Welcome to Tiny Basic v0.1.0
                  |Type commands or enter program code.
                  |
                  |] """.stripMargin)
  UI.visible = true
  UI.command = c => {
    c.trim.toUpperCase match {
      case "" =>
        UI.out.print("\n Ok\n] ")
      case "LIST" =>
        interp.list(None, None)
        UI.out.print("\n Ok\n] ")
      case "RUN" =>
        new Thread {
          override def run(): Unit = {
            interp.run(None)
            UI.out.print("\n Ok\n] ")
          }
        }.start()
      case com =>
        UI.out.println(s"unrecognized command: $com")
        UI.out.print("\n] ")
    }
  }

}
