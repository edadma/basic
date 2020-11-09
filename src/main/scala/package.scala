package xyz.hyperreal

import scala.util.parsing.input.Position

package object basic {

  def problem(pos: Position, error: String): Nothing =
    if (pos eq null)
      sys.error(error)
    else if (pos.line == 1)
      sys.error(s"error: $error\n${pos.longString}")
    else
      sys.error(s"error on line ${pos.line}: $error\n${pos.longString}")

}
