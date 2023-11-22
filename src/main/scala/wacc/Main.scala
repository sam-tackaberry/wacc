package wacc

import scala.io.Source
import frontEnd.semanticError._
import compiler._

object main {
    
    var fileName : String = null

    def main(args: Array[String]): Unit = {
        val file : String = args.head
        fileName = file
        setFile(file)
        compile(readFile(file))
    }

    def readFile(fileName: String) : String = {
        val source = Source.fromFile(fileName)
        try source.getLines().mkString("\n")
        finally source.close
    }

}