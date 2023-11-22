package wacc.backEnd
import frontEnd.predefFuncs._
import frontEnd.ast._
import frontEnd.symbolTable._
import frontEnd.semanticAnalyserUtils.{typeToIdObj,paramListToListOfSemTypes}
import scala.collection.mutable.ListBuffer
import labelledCode._
import assemblyIR._
import constants._
import statementGenerate._
import frontEnd.semanticAst._

object irGenerator {

    //Generates code for the program, recurisvely generating code for the body of the program as well as the functions of the program
    def generateProgram (program: WACCProgram): List[Instruction] = {
    
        var programOffset = OFFSET_INIT

        var programGenerated = new ListBuffer[Instruction]()

        beforeData += dataLabel
        beforeText += textLabel
        programGenerated += globalMainLabel

        // Entering the main program
        programGenerated = generateEnterNewScope(main, programGenerated)
        
        
        program.body.foreach{stat => {
            val (newProgramGenerated,newOffset) = generateStat(stat,programOffset,programGenerated)
            programOffset = newOffset
            programGenerated = newProgramGenerated
        }}

        

        // Exiting the main program
        programGenerated = generateExitScope(programGenerated)
   
        // also generate predefined functions that have been called by the user
        val predefinedFuncs = getpredefForProg()
        
        
        predefinedFuncs.foreach {func => programGenerated = generateFunction(func, programGenerated)}

        program.functions.foreach{func => {
            programGenerated = generateFunction (func,programGenerated)
            }
        }
        
        programGenerated.toList
        
    }


    //Generates code for functions of the program, passing the buffer in and returning
    def generateFunction (function: Function, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {

        var functionOffset = WORD_SIZE
        val funcSt : SymbolTable = function.getScope()

        //Declare the parameters of the function to be set to whatever has been passed in, which will be on the stack above the FP.
        function.paramList match {
            case Some(ParamList(pl)) => {
                var paramsOffset = 2 * WORD_SIZE
                for (Param(_, argI) <- pl) {
                    val someSemP = funcSt.lookup(argI, EnumVariable)
                    someSemP match {
                        case Some(semP@SemVariable(_)) => {
                            semP.setOffset(-paramsOffset)
                            paramsOffset += WORD_SIZE
                        }
                        case Some(_) =>
                        case None =>
                    }
                }
            }
            case None => ListBuffer[Instruction]()
        }

        val (ty, i@Ident(id)) = function.typeAndIdent
        var functionGenerated = programGenerated

        /*Function must be looked up in the symbol table to get the label name of the function for label generation.*/
        val Some(semFunc : SemFunction) = funcSt.lookupAll(i, EnumFunction(typeToIdObj(ty),paramListToListOfSemTypes(function.paramList)))
        functionGenerated = generateEnterNewScope(semFunc.labelName, functionGenerated)
           
        val b = function.body
        b.foreach { stat => {
            val (newFunctionGenerated,newOffset) = generateStat(stat,functionOffset, functionGenerated)
            functionOffset = newOffset
            functionGenerated = newFunctionGenerated
        }}
        functionGenerated
    }

    // Generates Instructions used whenever entering a new scope
    def generateEnterNewScope(name: String, code: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        val scopeGenerated: ListBuffer[Instruction] = code
        scopeGenerated += Define(Label(name))
        scopeGenerated += Push(ListBuffer(FP, LR))
        scopeGenerated += Mov(Unconditional, FP, SP) 
        scopeGenerated  
    }

    // Generates Instructions used whenever we want to exit a scope
    def generateExitScope(code: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        val scopeGenerated: ListBuffer[Instruction] = code
        scopeGenerated += Mov(Unconditional, SP, FP)
        scopeGenerated += Mov(Unconditional, Reg0, IMM_ZERO)
        scopeGenerated += Pop(ListBuffer(FP, PC))
        scopeGenerated
    }   
}