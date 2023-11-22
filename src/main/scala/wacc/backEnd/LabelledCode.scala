package wacc.backEnd

import assemblyIR._
import scala.collection.mutable.ListBuffer

/*
    Blocks of instructions that can be generated during the translation and put
    at the end or the beginning of the program.
    Blocks of Instructions containing text and data information, also helper functions.
*/
object labelledCode {

    var beforeText = new ListBuffer[Instruction]()
    var beforeData = new ListBuffer[Instruction]()

    var afterTextPrintString = new ListBuffer[Instruction]()
    var afterDataPrintString = new ListBuffer[Instruction]()

    var afterTextPrintChar = new ListBuffer[Instruction]()
    var afterDataPrintChar = new ListBuffer[Instruction]()

    var afterTextPrintInt = new ListBuffer[Instruction]()
    var afterDataPrintInt = new ListBuffer[Instruction]()

    var afterTextPrintBool = new ListBuffer[Instruction]()
    var afterDataPrintBool = new ListBuffer[Instruction]()

    var afterTextPrintPair = new ListBuffer[Instruction]()
    var afterDataPrintPair = new ListBuffer[Instruction]()

    var afterTextPrintln = new ListBuffer[Instruction]()
    var afterDataPrintln = new ListBuffer[Instruction]()

    var afterTextReadChar = new ListBuffer[Instruction]()
    var afterDataReadChar = new ListBuffer[Instruction]()

    var afterTextReadInt = new ListBuffer[Instruction]()
    var afterDataReadInt = new ListBuffer[Instruction]()

    var afterTextDivbyZero = new ListBuffer[Instruction]()
    var afterDataDivbyZero = new ListBuffer[Instruction]()

    var afterTextOverflow = new ListBuffer[Instruction]()
    var afterDataOverflow = new ListBuffer[Instruction]()

    var afterTextBoundsCheck = new ListBuffer[Instruction]()
    var afterDataBoundsCheck = new ListBuffer[Instruction]()

    var afterTextNullDereference = new ListBuffer[Instruction]()
    var afterDataNullDereference = new ListBuffer[Instruction]()

    // Helper Functions without data and text labels
    var freePairFunction = new ListBuffer[Instruction]()
}
