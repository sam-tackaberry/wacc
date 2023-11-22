package wacc.backEnd

import frontEnd.ast._
import frontEnd.symbolTable._
import frontEnd.semanticAst._
import assemblyIR._
import constants._
import io._
import frontEnd.getSemType.exprGetType
import scala.collection.mutable.ListBuffer

object utils {
    
    def boolToInt(b:Boolean) = if (b) TRUE else FALSE

    // How much space we need to store each type
    def getExprSize(expr: Expr, st: SymbolTable): Int = {
        val expType = exprGetType(expr, st)
        expType match {
                    case SemBool => ONE_BYTE
                    case SemChar => ONE_BYTE
                    case _ => WORD_SIZE
                }
    }

    def sizeToInt(size: Size): Int = {
        size match {
            case OneByte => ONE_BYTE
            case FourBytes => WORD_SIZE
        }
    }

    /* 
        Adds in visible '\' characters which are needed for the ascii code 
        (reversing the lexers removal of escape characters) 
    */
    def specialChars(s: String): String = {
        s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")
    }
   
    /* 
        Flags for labels (whether certain labels such as prints have been 
        created yet) and counters to maintain distinct labels counts. 
    */
    var labelCounter = -1
    def getLabelCounter(): Int = {
        labelCounter += 1
        labelCounter
    }

    var scopeLabel = -1
    def getUniqueScopeLabel(): String = {
        scopeLabel += 1
        ".FL" + scopeLabel
    }

    // Flags for run time errors
    var divByZeroFlag = false
    var overflowFlag = false
    var arrayBoundFlag = false
    var nullDereferenceFlag = false

    var stringCounter = -1
    def getStringCounter() = {
        stringCounter += 1
        stringCounter
    }

    def getNextLabel(): Label = {
        return Label(genericLabel + getLabelCounter())
    }


    def getStringLabel(): Label = {
        return Label(strString + getStringCounter())
    }

    // used in branch operations when we need two labels
    def getBranchLabels(): (Label, Label) = {
        return (getNextLabel(), getNextLabel())
    }

    def branchBack(offset: Int, instrs: ListBuffer[Instruction]) = {
        instrs += Mov(Unconditional, SP, FP)
        instrs += Add_IR(SP, SP, ImmNum(-offset+WORD_SIZE))
        instrs
    }

    def matchGenReadOrChar(t: SemType, readGenerated: ListBuffer[Instruction]) = {
        t match {
            case SemInt => {
                generateReadInt()
                readGenerated += Mov(Unconditional, Reg0, Reg1)
                readGenerated += Bl(Unconditional, readILabel)
            }
            case SemChar => {
                generateReadChar()
                readGenerated += Mov(Unconditional, Reg0, Reg1)
                readGenerated += Bl(Unconditional, readCLabel)
            }
            case _ => sys.exit(SEMANTIC_ERROR)
            }
        readGenerated
        }

    // Flags if helper functions are generated already
    var printsGen = false
    var printiGen = false
    var printcGen = false
    var printbGen = false
    var printpGen = false
    var printlnGen = false
    var readIntGen = false
    var readCharGen = false
    var arrLoadGen = false
    var arrLoadStackGen = false
    var arrStoreGen = false
    var freePairGen = false
    var arrLoadBGen = false
    var arrLoadBStackGen = false
    var arrStoreBGen = false
}