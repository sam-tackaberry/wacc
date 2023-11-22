package wacc.backEnd

import labelledCode._
import assemblyIR._
import constants._
import utils._
import io._
import scala.collection.mutable.ListBuffer

object rteChecking {

    // Generate the Instructions for setting up a generic error message
     def errorMessageSetUp(errorLabel: Label, errorFormatLabel: Label, errorMsg: String, errorMsgLen: Integer, 
                           data: ListBuffer[Instruction], text: ListBuffer[Instruction]): Unit = {
        data += Word(errorMsgLen)
        data += Define(errorFormatLabel)
        data += Asciz(errorMsg)
        text += Define(errorLabel)
        text += Ldr(Reg0, ImmLoadLabel(errorFormatLabel), FourBytes)
    }

    // Generic Error behaviour
    def genericError(errorLabel: Label, errorFormatLabel: Label, errorMsg: String, errorMsgLen: Integer, flag: Boolean, 
                     data: ListBuffer[Instruction], text: ListBuffer[Instruction]): Unit = {
        if (!flag) {
            errorMessageSetUp(errorLabel, errorFormatLabel, errorMsg, errorMsgLen, data, text)
            text += Bl(Unconditional, printsLabel)
            text += Mov(Unconditional, Reg0, ImmNum(RUN_TIME_ERROR))
            text += Bl(Unconditional, exitLabel)
            generatePrints()
        }
    }

    // Generate division by zero run time error
    def divByZeroError(): Unit = {
        genericError(divByZeroLabel, divByZeroFormatLabel, divByZeroMsg, divByZeroMsgLen, divByZeroFlag, afterDataDivbyZero, afterTextDivbyZero)
        divByZeroFlag = true
    }

    // Generate overflow run time error
    def overflowError(): Unit = {
        genericError(overflowLabel, overflowFormatLabel, overflowMsg, overflowMsgLen, overflowFlag, afterDataOverflow, afterTextOverflow)
        overflowFlag = true
    }

    // Generate null derefrencing run time error
    def nullDereferenceError(): Unit = {
        genericError(nullDereferenceLabel, nullDereferenceFormatLabel, nullDereferenceMsg, nullDereferenceMsgLen, 
                     nullDereferenceFlag, afterDataNullDereference, afterTextNullDereference)
        nullDereferenceFlag = true
    }

    // Generate accessing array out of bound run time error
    def arrayBoundsError(): Unit = {
        if (!arrayBoundFlag) {
            errorMessageSetUp(boundsCheckLabel, boundsCheckFormatLabel, boundsCheckMsg, boundsCheckMsgLen, afterDataBoundsCheck, afterTextBoundsCheck)
            afterTextBoundsCheck += Bl(Unconditional, printFLabel)
            afterTextBoundsCheck += Mov(Unconditional, Reg0, IMM_ZERO)
            afterTextBoundsCheck += Bl(Unconditional, fflushLabel)
            afterTextBoundsCheck += Mov(Unconditional, Reg0, ImmNum(RUN_TIME_ERROR))
            afterTextBoundsCheck += Bl(Unconditional, exitLabel)
            arrayBoundFlag = true
            generatePrints()
        }
    }

   
}