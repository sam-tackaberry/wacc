package wacc.backEnd

import constants._
import utils._
import assemblyIR._
import labelledCode._
import frontEnd.semanticAst._
import scala.collection.mutable.ListBuffer

object io {
    
    // Generate a label for a string declared
    def generateStrDeclare(s: String): Label = {
        beforeData += Word(s.length())
        val label = Label(strDeclrString + getStringCounter())
        beforeData += Define(label)
        beforeData += Asciz(s)
        label
    }

    // READING FUNCTIONS
    
    // General read function for int and char
    def generateRead(flag: Boolean, formatSize: Int, formatLabel: Label, format: String, 
                    codeLabel: Label, data: ListBuffer[Instruction], text: ListBuffer[Instruction]): Unit = {
        if (!flag){
            data += Word(formatSize)
            data += Define(formatLabel)
            data += Asciz(format)
            text += Define(codeLabel)
            text += Push(ListBuffer(LR))
            text += StrPost(Reg0, StackAccess2(SP, IMM_M_FOUR), FourBytes)
            text += Mov(Unconditional, Reg1, SP)
            text += Ldr(Reg0, ImmLoadLabel(formatLabel), FourBytes)
            text += Bl(Unconditional, scanfLabel)
            text += Ldr(Reg0, StackAccess2(SP, IMM_ZERO), FourBytes)
            text += Add_IR(SP, SP, ImmNum(4))
            text += Pop(ListBuffer(PC))
        }
    }

    def generateReadInt(): Unit = {
        generateRead(readIntGen, decimalFormat.length(), readIFormatLabel, decimalFormat, readILabel, afterDataReadInt, afterTextReadInt)
        readIntGen = true
    }

    def generateReadChar(): Unit = {
        generateRead(readCharGen, readCharFormat.length() , readCFormatLabel, readCharFormat, readCLabel, afterDataReadChar, afterTextReadChar)
        readCharGen = true
    }

    // PRINTING FUNCTIONS

    // Generic code that generates the labels and associated code for the passed in print instruction
    def generatePrintCode(flag: Boolean, formatSize: Int, formatLabel: String, format: String, codeLabel: Label, data: ListBuffer[Instruction], text: ListBuffer[Instruction]): Unit = {
        if (!flag){
            data += Word(formatSize)
            data += Define(Label(formatLabel))
            data += Asciz(format)
            text += Define(codeLabel)
            text += Push(ListBuffer(LR))
            text += Mov(Unconditional, Reg1, Reg0)
            text += Ldr(Reg0, ImmLoadLabel(Label(formatLabel)), FourBytes)
            text += Bl(Unconditional, printFLabel)
            text += Mov(Unconditional, Reg0, ImmNum(0))
            text += Bl(Unconditional, fflushLabel)
            text += Pop(ListBuffer(PC))
        }
    }

    // Generate the code for the print int functions
    def generatePrinti(): Unit = {
        generatePrintCode(printiGen, TWO_CHARS, intFormatLabel, decimalFormat, printiLabel, afterDataPrintInt, afterTextPrintInt)
        printiGen = true
    }

    def generatePrintc(): Unit = {
        generatePrintCode(printcGen, TWO_CHARS, charFormatLabel, charFormat, printcLabel, afterDataPrintChar, afterTextPrintChar)
        printcGen = true
    }
    
    def generatePrintp(): Unit = {
        generatePrintCode(printpGen, TWO_CHARS, pairFormatLabel, pairFormat, printpLabel, afterDataPrintPair, afterTextPrintPair)
        printpGen = true
    }

    // Println, string and bool cannot be generified
    def generatePrintlnFormat() : Unit = {
        if (!printlnGen){
            afterDataPrintln += Word(ZERO)
            afterDataPrintln += Define(stringlnFormatLabel)
            afterDataPrintln += Asciz(emptyStr)
            afterTextPrintln += Define(printlnLabel)
            afterTextPrintln += Push(ListBuffer(LR))
            afterTextPrintln += Ldr(Reg0, ImmLoadLabel(stringlnFormatLabel), FourBytes)
            afterTextPrintln += Bl(Unconditional, putsLabel)
            afterTextPrintln += Mov(Unconditional, Reg0, ImmNum(0))
            afterTextPrintln += Bl(Unconditional, fflushLabel)
            afterTextPrintln += Pop(ListBuffer(PC))
            printlnGen = true
        }
    }


    def generatePrints(): Unit = {
            if (!printsGen){
                afterDataPrintString += Word(WORD_SIZE)
                afterDataPrintString += Define(stringFormatLabel)
                afterDataPrintString += Asciz(stringFormat)
                afterTextPrintString += Define(printsLabel)
                afterTextPrintString += Push(ListBuffer(LR))
                afterTextPrintString += Mov(Unconditional, Reg2, Reg0)
                afterTextPrintString += Ldr(Reg1, StackAccess2(Reg0, ImmNum(-WORD_SIZE)), FourBytes)
                afterTextPrintString += Ldr(Reg0, ImmLoadLabel(stringFormatLabel), FourBytes)
                afterTextPrintString += Bl(Unconditional, printFLabel)
                afterTextPrintString += Mov(Unconditional, Reg0, ImmNum(0))
                afterTextPrintString += Bl(Unconditional, fflushLabel)
                afterTextPrintString += Pop(ListBuffer(PC))
                printsGen = true
            }
        }


    
    def generatePrintb(): Unit = {
        if (!printbGen) {
            afterDataPrintBool += Word(5)
            afterDataPrintBool += Define(falseFormatLabel)
            afterDataPrintBool += Asciz(falseStr)
            afterDataPrintBool += Word(WORD_SIZE)
            afterDataPrintBool += Define(trueFormatLabel)
            afterDataPrintBool += Asciz(trueStr)
            afterDataPrintBool += Word(WORD_SIZE)
            afterDataPrintBool += Define(boolFormatLabel)
            afterDataPrintBool += Asciz(stringFormat)
            afterTextPrintBool += Define(printBLabel)
            afterTextPrintBool += Push(ListBuffer(LR))
            afterTextPrintBool += Cmp(Reg0, IMM_ZERO)
            afterTextPrintBool += B(IR_NE, printTrueLabel)
            afterTextPrintBool += Ldr(Reg2, ImmLoadLabel(falseFormatLabel), FourBytes)
            afterTextPrintBool += B(Unconditional,  printBoolLabel)
            afterTextPrintBool += Define(printTrueLabel)
            afterTextPrintBool += Ldr(Reg2, ImmLoadLabel(trueFormatLabel), FourBytes)
            afterTextPrintBool += Define(printBoolLabel)
            afterTextPrintBool += Ldr(Reg1, StackAccess2(Reg2, ImmNum(WORD_SIZE)), FourBytes)
            afterTextPrintBool += Ldr(Reg0, ImmLoadLabel(boolFormatLabel), FourBytes)
            afterTextPrintBool += Bl(Unconditional, printFLabel)
            afterTextPrintBool += Mov(Unconditional, Reg0, IMM_ZERO)
            afterTextPrintBool += Bl(Unconditional,fflushLabel)
            afterTextPrintBool += Pop(ListBuffer(PC))
            printbGen = true
        }
    }
    
    // adds the right label for printing the inputted type, should only be called once for each type
    def semTypePrint(t: SemType, instrs: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        t match {
            case SemInt => {
                generatePrinti()
                instrs += Mov(Unconditional, Reg0, Reg1)
                instrs += Bl(Unconditional, printiLabel)
            }
            case SemBool => {
                generatePrintb()
                instrs += Mov(Unconditional, Reg0, Reg1)
                instrs += Bl(Unconditional, printBLabel)
            }
            case SemChar => {
                generatePrintc()
                instrs += Mov(Unconditional, Reg0, Reg1)
                instrs += Bl(Unconditional, printcLabel)
            }
            case SemString => {
                generatePrints()
                instrs += Mov(Unconditional, Reg0, Reg1)
                instrs += Bl(Unconditional, printsLabel)
            }
            case SemUnknown() => {
                generatePrintp()
                instrs += Mov(Unconditional, Reg0, Reg1)
                instrs += Bl(Unconditional, printpLabel)
            }
            case SemArray(SemChar) => {
                generatePrints()
                instrs += Mov(Unconditional, Reg0, Reg1)
                instrs += Bl(Unconditional, printsLabel)
            }
            case _ => {
                generatePrintp()
                instrs += Mov(Unconditional, Reg0, Reg1)
                instrs += Bl(Unconditional, printpLabel)
            }
        }
        instrs
    }
}
