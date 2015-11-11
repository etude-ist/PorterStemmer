import java.io.{PrintWriter, File}

import scala.io.Source

import PorterStemmer._


object PorterStemmerClient {

  /**
  *  Usage: sbt "run-main PorterStemmerClient --read_path <file> --write_path <file>"
  * */

  def runStemmer(readPath: String, writePath: String): Unit = {
    var rst: Array[String] = Array()
    val writer = new PrintWriter(new File(writePath))
    for (line <- Source.fromFile(readPath).getLines()) {
      val words = line.split(" ")
      words.foreach(w => {
        println(w)
        // println(stem(w))
        // writer.write(stem(w))
      })
    }
    writer.close()
  }

  private class ArgumentsParser(args: Seq[String]) {
    var isReady: Boolean = false
    var writePath: String = null
    var readPath: String = null

    private def parseOpts(opts: Seq[String]): Unit = {

      parse(opts)

      def parse(opts: Seq[String]): Unit = opts match {
        case ("--write_path") :: value :: tail =>
          writePath = value
          parse(tail)
        case ("--read_path") :: value :: tail =>
          readPath = value
          parse(tail)
        case name :: value :: tail if name.startsWith("-") =>
          println("Unrecognized option")
          parse(tail)
        case Nil =>
      }

    }

    private def checkRequiredArguments(): Unit = {
      if (args.length == 0) {
        println("Usage: sbt 'run-main PorterStemmerClient --read_path <file> --write_path <file>'")
        System.exit(1)
      }
      if (readPath == null) {
        println("--read_path was not specified")
        System.exit(1)
      }
      if (writePath == null) {
        println("--write_path was not specified")
        System.exit(1)
      } else {
        isReady = true
      }
    }

    parseOpts(args.toList)
    checkRequiredArguments()

  }

  def main(args: Array[String]): Unit = {

    val appArgs = new ArgumentsParser(args)

    if (appArgs.isReady) {
      println("Starting stemming of file: %s".format(appArgs.readPath) ++ " Please wait...\n")
      runStemmer(appArgs.readPath, appArgs.writePath)
      println("Result was stored in: %s".format(appArgs.writePath))
    }

  }

}
