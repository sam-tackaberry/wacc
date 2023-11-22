package frontEnd

import semanticAst._
import ast._
import scala.io.Source
import scala.io.BufferedSource
import scala.collection.mutable.ListBuffer

object semanticError{
    var file: BufferedSource = null
    var fileLines: List[String] = null

    val errorList: ListBuffer[ErrorMsg] = ListBuffer[ErrorMsg]()

    def semToString(semType: SemType): String = {
        semType match{
            case SemInt => "int"
            case SemString => "string"
            case SemChar => "char"
            case SemBool => "bool"
            //case SemPairLit => "null"
            case SemPairType(p1, p2) => s"pair(${semToString(p1)}, ${semToString(p2)})"
            case SemArray(elementType) => s"${semToString(elementType)}[]"
            case SemPairUnknown() => "unknown pair type"
            case SemAny => "any"
            // Should never reach this case, added to deal with warnings
           case _ => "unknown type"
        }
    }

    def identToVar(id: Ident): String = {
        id match {
            case Ident(i) => i
        }
    }

    def setFile(fileName: String): Unit = {
        file = Source.fromFile(fileName)
        fileLines = file.getLines().toList
    }

    def printFileLines(pos: (Int, Int)): Unit = {
        val lineNum = pos._1
        val colNum = pos._2
        var s = ""
        val before = lineNum - 2
        val curr = lineNum - 1
        val after = lineNum
        if (before >= 0){
            s += formatLine(before)
        }
        s += formatLine(curr)
        s += caretLine(colNum)
        if (after < fileLines.length){
            s += formatLine(after)
        }
        println(s)
    }

    def formatLine(lineNum: Int) = {
        ">" + fileLines(lineNum) + "\n"
    }
    def caretLine(colNum: Int) = {
        ">" + s"${" " * (colNum - 1)}${"^"}" + "\n"
    }

    def addToErrorList(e: ErrorMsg): Unit = {
        errorList += e
    }


    sealed trait ErrorMsg{
        def printError(): Unit
    }

    case class TypeError(unexpected: SemType, expected: SemType, pos: (Int, Int)) extends ErrorMsg {
        def printError(): Unit = {
            print(s"Type error at line ${pos._1}, column ${pos._2}\nUnexpected ${semToString(unexpected)}\nExpected ${semToString(expected)}\n")
            printFileLines(pos)
        }
    }

    case class TypeErrorMessage(unexpected: SemType, expected: String, pos: (Int, Int)) extends ErrorMsg {
        def printError(): Unit = {
            print(s"Type error at line ${pos._1}, column ${pos._2} \nUnexpected ${semToString(unexpected)}\nExpected ${expected}\n")
            printFileLines(pos)
        }
    }

    case class PairTypeError(pos: (Int, Int)) extends ErrorMsg {
        def printError(): Unit = {
            print(s"Type error at line ${pos._1}, column ${pos._2} \nAttempting to exchange values between pairs of unknown types \n" +
              s"Pair exchange is only legal when the type of at least one of the sides is known or specified\n")
            printFileLines(pos)
        }
    }

    case class ScopeError(undeclared: Ident, pos: (Int, Int))extends ErrorMsg {
        def printError(): Unit = {
            print(s"Scope error at line ${pos._1}, column ${pos._2} \nVariable ${identToVar(undeclared)} has not been declared in this scope\n")
            printFileLines(pos)
        }
    }

    case class ReturnError(pos: (Int, Int)) extends ErrorMsg {
        def printError(): Unit = {
            print(s"Return placement error at line ${pos._1}, column ${pos._2} \nreturn outside of function is not allowed\n")
            printFileLines(pos)
        }
    }

    case class FunctionError(undeclared: Ident, pos: (Int, Int)) extends ErrorMsg {
        def printError(): Unit = {
            print(s"Undefined function error at line ${pos._1}, column ${pos._2} \nFunction ${identToVar(undeclared)} has not been defined\n")
            printFileLines(pos)
        }
    }

    case class RedeclarationError(declared: Ident, pos: (Int, Int)) extends ErrorMsg  {
        def printError(): Unit = {
            print(s"Illegal declaration of variable ${identToVar(declared)} \npreviously declared in this scope")
            printFileLines(pos)
        }
    }

    case class FunctionRedefinitionError(declared: Ident, pos: (Int, Int)) extends ErrorMsg {
        def printError(): Unit = {
            print(s"Function redefinition error at line ${pos._1}, column ${pos._2} \nIllegal redefinition of ${identToVar(declared)} \npreviously declared already")
            printFileLines(pos)
        }
    }

    case class FunctionCallError(declared: Ident, unexpected: Integer, expected: Integer, pos: (Int, Int)) extends ErrorMsg  {
        def printError(): Unit = {
            print(s"Wrong number of arguments provided to function ${identToVar(declared)} \nunexpected $unexpected arguments \nexpected $expected arguments\n")
            printFileLines(pos)
        }
    }

    case class CustomError(message: String) extends ErrorMsg  {
        def printError(): Unit = {
            println(message)
        }
    }

}
