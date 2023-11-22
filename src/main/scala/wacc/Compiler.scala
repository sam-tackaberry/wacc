package wacc

import frontEnd.lexer._
import main._

import parsley.{Parsley, Success, Failure, Result}
import frontEnd.ast._
import frontEnd.semanticAnalyser._
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors._
import parsley.errors.tokenextractors.LexToken.constantSymbols
import frontEnd.errors._
import frontEnd.parser
import backEnd.codeGenerator._
import backEnd.constants._
import java.io._
import java.nio.file.Paths
import parsley.errors.Token

object compiler {
    implicit val eb: ErrorBuilder[WaccError] = new WaccErrorBuilder with LexToken{
        private val identifiers = lexer.nonlexeme.names.identifier.map(x => s"${identStr} $x")
        private val integers = lexer.nonlexeme.numeric.integer.decimal32.map(x => s"{$intStr} $x")
        private val _keywords = constantSymbols((keywords.map(x => (lexer.nonlexeme.symbol(x), s"${keywordStr} $x")).toList):_*)

        def tokens: Seq[Parsley[String]] = identifiers +: integers +: _keywords

        override def extractItem(cs: Iterable[Char], amountOfInputParserWanted: Int): Token 
        = TillNextWhitespace.unexpectedToken(cs, amountOfInputParserWanted)
   }

    def compile(arg: String) : Unit = {
        val waccProg: Result[Any, WACCProgram] = parse(arg)
        waccProg match {
            case Success(x) => {
                println(s"${arg.head} = $x") 
                val semanticSafe = semanticCheck(waccProg.get)
                //println(instr)
                if (!semanticSafe){
                    sys.exit(SEMANTIC_ERROR)
                }
                val instr = generateCode(waccProg.get)
                convertToFile(instr)
                sys.exit(SUCCESS)
            }
            case Failure(msg) => {
                println(msg)
                sys.exit(SYNTAX_ERROR)
            }
        }
    }

    def parse(program : String) = {
        parser.`<parse-program>`.parse(program)
    }

    def semanticCheck(program : WACCProgram): Boolean = {
        val (prog, errList) = programCheck(program)
        // go thru list of semantic errors and call print error on all of them
        var semanticSafe = true
        for (e <- errList){
            semanticSafe = false
            // if one semantic error is found, the program is not semantically correct
            e.printError()
        }
        semanticSafe
    }

    def convertToFile(instr: List[String]) = {
        val name = Paths.get(fileName).getFileName.toString.dropRight(5)
        val file = new File(name + assembFile)
        val bw = new BufferedWriter(new FileWriter(file))
        instr.foreach(instr => {
            bw.write(instr)
            bw.newLine()
        })
        bw.close()
    }
}
