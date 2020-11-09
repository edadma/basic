package xyz.hyperreal.basic

object Main extends App {

  val program =
    """
      |100 LET a = 3  'assign 3 to the variable 'a'
      |105 DIM b[10]
      |106 b[0] = 4
      |107 print b[0]
      |110 PRINT "a is "; a; " and", "a + 1 is ";a*(4+5)
      |115 print sqrt(a)
      |120 END  'end program
      |130 PRINT "this line doesn't get executed"
      |""".stripMargin
  val parser = new BasicParser
  val interp = new Interpreter
  val ast = BasicParser.parseProgram(program, parser)

  interp.load(ast)
  interp.list(None, None)
  interp.run(None)

}
