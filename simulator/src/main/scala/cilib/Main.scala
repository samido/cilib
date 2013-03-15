package simulator

import scala.tools.nsc._
import interpreter._
import scala.util.Properties.{ jdkHome, javaVersion, versionString, javaVmName }
import java.io._

object REPL {

  val packageImports = List("scalaz._", "Scalaz._", "cilib._")

  // Stolen from: https://github.com/jsuereth/scala-embedded-interpreter-sample/blob/master/src/main/scala/test/InterpreterWrapper.scala
  class CIlibInterpreterLoop(out: PrintWriter) extends ILoop(None, out) {
    override def loop() {
      if (isAsync) awaitInitialized()
      bindSettings()
      super.loop()
    }

    /** Bind the settings so that evaluated code can modify them */
    def bindSettings() {
      intp beQuietDuring {
        // for ( (name, (clazz, value)) <- bindings) {
        //   intp.bind(name, clazz.getCanonicalName, value)
        // }
        echo("Loading required packages and imports...")
        for (importString <- packageImports) {
          intp interpret ("import " + importString)
        }
      }
    }
    override def helpCommand(line: String): Result = {
      if (line == "") echo("This needs to be customized!!!" /*helpMsg*/ )

      super.helpCommand(line)
    }

    override def printWelcome(): Unit = {
      val welcomeMsg =
        """|Welcome to CIlib (Scala %s, %s, %s)
           |Type in expressions to have them evaluated.
           |Type :help for more information.
           |""".stripMargin.format(versionString, javaVmName, javaVersion)
      out.println(welcomeMsg)
      out.flush()
    }

    override def prompt = "\ncilib> "
  }

  def main(args: Array[String]): Unit = {
    val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(System.out)))
    val settings = new GenericRunnerSettings(out.println)

    settings.usejavacp.value = true
    val interpreter = new CIlibInterpreterLoop(out)
    interpreter process settings
    ()
  }
}
