package wacc.backEnd

import assemblyIR._ 
import labelledCode._
import constants._
import utils._
import rteChecking._
import exprGenerate._
import frontEnd.ast._
import frontEnd.symbolTable._
import frontEnd.getSemType._
import frontEnd.semanticAst._
import scala.collection.mutable.ListBuffer

object heapFunctions {

    /* 
        Generates instructions to set up an array, mallocing the space needed for the 
        array and adding the address in the right place in stack.
        Reg2 will be used to store the address of the array.
        Reg1 will be used for any values
    */
    def setUpArrayHeap(programGenerated: ListBuffer[Instruction], space: Int, size: Int, 
                   pushOrStoreStat: Instruction): ListBuffer[Instruction] = {
        val arraySetGenerated: ListBuffer[Instruction] = programGenerated
        val index:Int = ARRAY_INDEX_INIT
        arraySetGenerated += Mov(Unconditional, Reg0, ImmNum(space))
        arraySetGenerated += Bl(Unconditional, mallocLabel)
        // will leave the address of the malloc in Reg 0
        
        arraySetGenerated += Mov(Unconditional, Reg2, Reg0)
        arraySetGenerated += Add_IR(Reg2, Reg2, ImmNum(WORD_SIZE))
        arraySetGenerated += Mov(Unconditional, Reg1, Reg2)
        //we push in the stack the address of the array
        arraySetGenerated += pushOrStoreStat
        arraySetGenerated += Mov(Unconditional, Reg1, ImmNum(size))
        arraySetGenerated += Str(Reg1, StackAccess2(Reg2, ImmNum(index)), FourBytes)
        arraySetGenerated
    }
    
    def setUpArrayStack(programGenerated: ListBuffer[Instruction], exprs: List[Expr], elemSize: Size): ListBuffer[Instruction] = {
        var arraySetGenerated = programGenerated
        val arraySize = exprs.length
        var index = 0
        arraySetGenerated += Mov(Unconditional, Reg1, ImmNum(exprs.length))
        arraySetGenerated += Add_IR(SP, SP, (ImmNum(-arraySize*sizeToInt(elemSize) - 8)))
        arraySetGenerated += Add_IR(Reg2, SP, ImmNum(8))
        arraySetGenerated += Str(Reg2, StackAccess2(SP, ImmNum(index)), FourBytes)
        index += 4
        arraySetGenerated += Str(Reg1, StackAccess2(SP, ImmNum(index)), FourBytes)
        index += 4
        //arraySetGenerated += Add_IR(SP, SP, (IMM_FOUR))
        //arraySetGenerated += Push(ListBuffer(Reg1))
        exprs.foreach(expr => {
            arraySetGenerated = exprGenerate.generateExpr(expr, arraySetGenerated)
            arraySetGenerated += Str(Reg1, StackAccess2(SP, ImmNum(index)), elemSize)
            index += sizeToInt(elemSize)
            //arraySetGenerated += Add_IR(SP, SP, ImmNum(sizeToInt(elemSize)))
            
        })

        //arraySetGenerated += Add_IR(SP, SP, ImmNum(-arraySize*sizeToInt(elemSize) - 4))
        arraySetGenerated
    }

    // Generate the instructions for the helper function to load an array
    def generateArrLoad() = {
        generateArrStrLdr(arrLoadGen, arrayLoadLabel, Ldr(Reg2, StackAccess2(Reg2,  Lsl(Reg10, IMM_TWO)), FourBytes))
        arrLoadGen = true
    }

    // def generateArrLoadStack(stackFlag: Boolean) = {
    //     generateArrStrLdr(arrLoadStackGen, arrayLoadStackLabel, Ldr(Reg2, StackAccess2()))
    // }

    // Generates the instructions for the helper function to store an array
    def generateArrStore() = {
        generateArrStrLdr(arrStoreGen, arrayStoreLabel, Str(Reg3, StackAccess2(Reg2,  Lsl(Reg10, IMM_TWO)), FourBytes))
        arrStoreGen = true
    }

    // Generates the instructions for the helper function to load an array where the elements are stored in one byte
    def generateArrLoadB() = {
        generateArrStrLdr(arrLoadBGen, arrayLoadBLabel, Ldr(Reg2, StackAccess2(Reg2, Reg10), OneByte))
        arrLoadBGen = true
    }

    // Generates the instructions for the helper function to store an array where the elements are stored in one byte
    def generateArrStoreB() = {
        generateArrStrLdr(arrStoreBGen, arrayStoreBLabel, Str(Reg3, StackAccess2(Reg2, Reg10), OneByte))
        arrStoreBGen = true
    }

    // Common instructions when we want to load or store an array, where b is the flag if the function is already generated
    def generateArrStrLdr(b: Boolean, l: Label, strOrLdr: Instruction) = {
        if (!b) {
            generateArrStrLdrSetUp(l, IMM_M_FOUR)
            afterTextPrintString += strOrLdr
            afterTextPrintString += Pop(ListBuffer(PC))
        }
    }

    // Generates the instructions to setup an array load or store
    def generateArrStrLdrSetUp(l: Label, stackOffset: ImmNum) = {
        afterTextPrintString += Define(l)
        afterTextPrintString += Push(ListBuffer(LR))
        afterTextPrintString += Cmp(Reg10, IMM_ZERO)
        afterTextPrintString += Mov(IR_LT, Reg1, Reg10)
        afterTextPrintString += Bl(IR_LT, boundsCheckLabel)
        afterTextPrintString += Ldr(LR, StackAccess2(Reg2, stackOffset), FourBytes)
        afterTextPrintString += Cmp(Reg10, LR)
        afterTextPrintString += Mov(IR_GE, Reg1, Reg10)
        afterTextPrintString += Bl(IR_GE, boundsCheckLabel)
    }

    /* 
        Mallocs a certain elemnt in memory.
        Use Reg2 to store the address.
        Use Reg1 to store the values
    */
    def mallocPairElem(e1: Expr, programGenerated: ListBuffer[Instruction], st: SymbolTable, pushOrStoreStat: Instruction): ListBuffer[Instruction] = {
        val size = getExprSize(e1, st)
        var elemGenerated = programGenerated
        elemGenerated += Mov(Unconditional, Reg0, ImmNum(size))
        elemGenerated += Bl(Unconditional, mallocLabel)
        // will leave the address of the malloc in Reg 0

        elemGenerated += Mov(Unconditional, Reg2, Reg0)
        elemGenerated = generateExpr(e1,elemGenerated)
        elemGenerated += Str(Reg1, StackAccess2(Reg2, IMM_ZERO), FourBytes)
        elemGenerated += pushOrStoreStat
    }

    /*
        Puts the pair elem in the stack
    */
    def putPairElemStack(e1: Expr, programGenerated: ListBuffer[Instruction], st: SymbolTable, pushOrStoreStat: Instruction) : ListBuffer[Instruction] = {
        var elemGenerated = programGenerated
        elemGenerated = generateExpr(e1,elemGenerated)
        elemGenerated += Mov(Unconditional, Reg2, Reg1)
        elemGenerated += pushOrStoreStat
    }

    def pairElemAddressCases(n: LValue, p: PairElem, programGenerated: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var pairGenerated = programGenerated
        val semP = getLValType(n, p.getScope())
        semP match {
            case semPair: SemPairType => {
                pairGenerated = getPairAddress(semPair.getStackFlag(), p, pairGenerated)
            }
            case a:SemPairUnknown => {
                pairGenerated = getPairAddress(a.getStackFlag(), p, pairGenerated)
            }
            case un:SemUnknown => {
                pairGenerated = getPairAddress(un.getStackFlag(), p, pairGenerated)
            }

            case _ => {pairGenerated = getPairElemAddress(pairGenerated, p)}
        }
        pairGenerated
    }

    def getPairElemCases(n: LValue, p: PairElem, programGenerated: ListBuffer[Instruction], pushOrStoreStat: Instruction): ListBuffer[Instruction] = {
        var rValGenerated = programGenerated
        val semP = getLValType(n, p.getScope())
        semP match {
            case semPair: SemPairType => {
                rValGenerated = getPairElem(semPair.getStackFlag(), p, rValGenerated)
            }
            case semUn: SemPairUnknown => {
                rValGenerated = getPairElem(semUn.getStackFlag(), p, rValGenerated)
            }
            case un: SemUnknown => {
                rValGenerated = getPairElem(un.getStackFlag(), p, rValGenerated)
            }
            case _ => {
                rValGenerated = getPairElemHeap(rValGenerated, p)
            }
        }
        rValGenerated += Mov(Unconditional, Reg2, Reg1)
        rValGenerated += pushOrStoreStat
    }

    def getPairAddress(flag: Boolean, p: PairElem, programGenerated: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var pairGenerated = programGenerated
        if (flag) {
            pairGenerated = getPairElemAddressStack(pairGenerated, p)
        } else {
            pairGenerated = getPairElemAddress(pairGenerated, p)
        }
        pairGenerated
    }

    def getPairElem(flag: Boolean, p: PairElem, programGenerated: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var rValGenerated = programGenerated
        if(flag){
            rValGenerated = getPairElemStack(rValGenerated, p)
        } else {
            rValGenerated = getPairElemHeap(rValGenerated, p)
        }
        rValGenerated
    }

    // Gets the actual pair elem from memory and loads it in Reg1
    def getPairElemHeap(programGenerated: ListBuffer[Instruction], p: PairElem): ListBuffer[Instruction] = {
        var pairGenerated = programGenerated
        pairGenerated = getPairElemAddress(pairGenerated, p)
        pairGenerated += Ldr(Reg1, StackAccess2(Reg1, IMM_ZERO), FourBytes)
        pairGenerated
    }

    // Gets the address in memory where the pair elem is stored, and leaves the address in 
    def getPairElemAddress(programGenerated: ListBuffer[Instruction], p: PairElem): ListBuffer[Instruction] = {
        var pairGenerated = programGenerated
        p match {
            case Fst(lVal) => {
                pairGenerated = getLvalAddressHelper(lVal, pairGenerated)
                pairGenerated += Ldr(Reg1, StackAccess2(Reg1, IMM_ZERO), FourBytes)
            }
            case Snd(lVal) => {
                pairGenerated = getLvalAddressHelper(lVal, pairGenerated)
                //It is stored as the second element in the pair
                pairGenerated += Ldr(Reg1, StackAccess2(Reg1, IMM_FOUR), FourBytes)
            }
            case _ =>        
        }
        
        nullDereferenceError()
        pairGenerated
    }

    def getLvalAddressHelper(lVal: LValue, code: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var pairGenerated = code
        lVal match {
                case snPair: PairElem => getPairElemHeap(pairGenerated, snPair)
                case id: Ident => pairGenerated = generateExpr(id, pairGenerated)
                case arr: ArrayElem => {
                    pairGenerated = generateExpr(arr, pairGenerated)
                    // will give the address of pair in Reg1
                }
                case _ =>
            }
        pairGenerated += Cmp(Reg1, IMM_ZERO)
        pairGenerated += Bl(IR_EQ, nullDereferenceLabel)
    }  

    // Gets the actual pair elem from memory and loads it in Reg1
    def getPairElemStack(programGenerated: ListBuffer[Instruction], p: PairElem): ListBuffer[Instruction] = {
        var pairGenerated = programGenerated
        pairGenerated = getPairElemAddressStack(pairGenerated, p)
        pairGenerated += Ldr(Reg1, StackAccess2(Reg1, IMM_ZERO), FourBytes)
        pairGenerated
    }

    // Gets the address in memory where the pair elem is stored, and leaves the address in 
    def getPairElemAddressStack(programGenerated: ListBuffer[Instruction], p: PairElem): ListBuffer[Instruction] = {
        var pairGenerated = programGenerated
        p match {
            case Fst(lVal) => {
                pairGenerated = getLvalStackHelper(lVal, pairGenerated)
            }
            case Snd(lVal) => {
                pairGenerated = getLvalStackHelper(lVal, pairGenerated)
                //It is stored as the second element in the pair
                pairGenerated += Add_IR(Reg1, Reg1, ImmNum(-4))
                
            }
            case _ =>        
        }
        
        nullDereferenceError()
        pairGenerated
    }

    def getLvalStackHelper(lVal: LValue, code: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var pairGenerated = code
        lVal match {
                case snPair: PairElem => {
                    getPairElemStack(pairGenerated, snPair)}
                case id: Ident => pairGenerated = generateExpr(id, pairGenerated)
                case arr: ArrayElem => {
                    pairGenerated = generateExpr(arr, pairGenerated)
                    // will give the address of pair in Reg1
                }
                case _ =>
            }
        pairGenerated += Cmp(Reg1, IMM_ZERO)
        pairGenerated += Bl(IR_EQ, nullDereferenceLabel)
    }       

    //Generates label and block of code for freeing pairs
    def generateFreePair() = {
        if(!freePairGen){
            freePairFunction += Define(freePairLabel)
            freePairFunction += Push(ListBuffer(LR))
            freePairFunction += Mov(Unconditional, Reg4, Reg0)
            freePairFunction += Cmp(Reg4, IMM_ZERO)
            freePairFunction += Bl(IR_EQ, nullDereferenceLabel)
            freePairFunction += Ldr(Reg0, StackAccess2(Reg4, IMM_ZERO), FourBytes)
            freePairFunction += Bl(Unconditional, freeLabel)
            freePairFunction += Ldr(Reg0, StackAccess2(Reg4, ImmNum(WORD_SIZE)), FourBytes)
            freePairFunction += Bl(Unconditional, freeLabel)
            freePairFunction += Mov(Unconditional, Reg0, Reg4)
            freePairFunction += Bl(Unconditional, freeLabel)
            freePairFunction += Pop(ListBuffer(PC))
            freePairGen = true
        }
        nullDereferenceError()
    }
}
