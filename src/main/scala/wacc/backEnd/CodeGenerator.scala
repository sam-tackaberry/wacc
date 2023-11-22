package wacc.backEnd


import frontEnd.ast._
import assemblyIR._
import irGenerator._
import translateInstr._
import constants._
import labelledCode._

import scala.collection.mutable.ListBuffer

object codeGenerator {

    // Translate from Instructions to Strings the data and text labels
    def dataTextLabel(data: ListBuffer[Instruction], text: ListBuffer[Instruction], instr: ListBuffer[String]): ListBuffer[String] = {
        var assembly = instr
        if (data.length > 0) {
            assembly += translateInstr(dataLabel)
        }
        assembly = generateStringsFromInstrs(data, assembly)
        if (text.length > 0) {
            assembly += translateInstr(textLabel)
        }
        assembly = generateStringsFromInstrs(text, assembly)
        assembly
    }

    // Translate from Instructions to Strings a helper function
    def generateStringsFromInstrs(function: ListBuffer[Instruction], assembly: ListBuffer[String]) : ListBuffer[String] = {
        function.foreach(instr => {
        assembly += translateInstr(instr)
        })
        assembly
    }
    
   // Translate the entire program into code 
   def generateCode(program: WACCProgram): List[String] = {
    val instructions: List[Instruction] = generateProgram(program)
    var assembly = new ListBuffer[String]()
    assembly = generateStringsFromInstrs(beforeData, assembly)
    assembly = generateStringsFromInstrs(beforeText, assembly)

    instructions.foreach(instr => {
        assembly += translateInstr(instr)
    })

    assembly = generateStringsFromInstrs(freePairFunction, assembly)

    assembly = dataTextLabel(afterDataReadChar, afterTextReadChar, assembly)
    assembly = dataTextLabel(afterDataReadInt, afterTextReadInt, assembly)
    assembly = dataTextLabel(afterDataPrintString, afterTextPrintString, assembly)
    assembly = dataTextLabel(afterDataPrintChar, afterTextPrintChar, assembly)
    assembly = dataTextLabel(afterDataPrintInt, afterTextPrintInt, assembly)
    assembly = dataTextLabel(afterDataPrintBool, afterTextPrintBool, assembly)
    assembly = dataTextLabel(afterDataPrintPair, afterTextPrintPair, assembly)
    assembly = dataTextLabel(afterDataPrintln, afterTextPrintln, assembly)
    assembly = dataTextLabel(afterDataDivbyZero, afterTextDivbyZero, assembly)
    assembly = dataTextLabel(afterDataOverflow, afterTextOverflow, assembly)
    assembly = dataTextLabel(afterDataBoundsCheck, afterTextBoundsCheck, assembly)
    assembly = dataTextLabel(afterDataNullDereference, afterTextNullDereference, assembly)
    assembly.toList
   }

}
