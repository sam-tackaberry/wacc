package wacc.backEnd

import frontEnd.ast._
import frontEnd.symbolTable._
import frontEnd.semanticAst._
import frontEnd.semanticAnalyserUtils.{argListToSemTypeList, typeToIdObj}
import frontEnd.getSemType.{getLValType}
import scala.collection.mutable.ListBuffer
import io._
import utils._
import assemblyIR._
import constants._
import rteChecking._
import labelledCode._
import frontEnd.getSemType._
import exprGenerate._
import heapFunctions._

object statementGenerate {

    // Generate instructions for each statement (except imports), the offset only changes in a declare.
    def generateStat(stat: Stat, offset : Int, programGenerated : ListBuffer[Instruction]) 
                    : (ListBuffer[Instruction], Int) = {
        stat match{
            case Skip => (programGenerated,offset)
            case d: Declare => generateDeclare (d,offset,programGenerated)
            case a: Assign => (generateAssign (a,programGenerated), offset)
            case r: ReadStat => (generateRead (r,programGenerated), offset)
            case f: FreeStat => (generateFree (f,programGenerated), offset)
            case r: ReturnStat => (generateReturn (r,offset,programGenerated), offset)
            case e: ExitStat => (generateExit (e,programGenerated), offset)
            case p: PrintStat => (generatePrint (p,programGenerated), offset)
            case p: PrintlnStat => (generatePrintln (p,programGenerated), offset)
            case i: IfStat => (generateIf (i,offset,programGenerated), offset)
            case w: WhileStat => (generateWhile (w,offset,programGenerated), offset)
            case b: BeginStat => (generateBegin(b,offset,programGenerated),offset)
        }    
    }

    // Generate instructions for a declare statement.
    def generateDeclare(stat: Declare, offset: Int, programGenerated : ListBuffer[Instruction])
                        : (ListBuffer[Instruction], Int) = {

        var declareGenerated = programGenerated
        val st: SymbolTable = stat.getScope()
        var newOffset = offset
        val semNode = st.lookup(stat.i, EnumVariable)
        semNode match {
            case Some(e) => {
                e match {
                    // We add in the symbol table the offset of where we are declaring the variable in the stack
                    case evar@SemVariable(ty) => {
                        evar.setOffset(offset)
                        ty match {
                            case a@ SemArray(el) => {
                                //In order to reuse the previous functions as they point the array to the first elem and not the size
                                if(a.getStackFlag()){    
                                    el match {
                                        case SemChar => {
                                            evar.setOffset(offset+(a.getSize())+ 4 + (4 - (a.getSize() % 4)))
                                            newOffset += (a.getSize() + DOUBLE_W_SIZE + (4 - (a.getSize() % 4)))
                                        }
                                        case _ => {
                                            evar.setOffset(offset+4*(a.getSize() + 1))
                                            newOffset += (a.getSize() + 2) * 4
                                        }
                                    }
                                } else {
                                    newOffset += 4
                                }
                            }
                            case p: SemPairType => {
                                val rVal = stat.r 
                                rVal match {
                                    case NewPair(Stack,_,_) => {
                                            evar.setOffset(offset + 8)
                                            newOffset += 12
                                    }
                                    case _=> newOffset += 4
                                }
                            }
                            case _ => newOffset += 4
                        }
                    }
                    case _ =>
                }
            }
            case None =>
        }
        val r = stat.r
        declareGenerated = generateRval(r, false, ZERO, declareGenerated, typeToIdObj(stat.t))
        (declareGenerated, newOffset)
    }


    // Generate instructions for an Assign statement.
    def generateAssign(stat: Assign, programGenerated : ListBuffer[Instruction]) : ListBuffer[Instruction] = {
        val st : SymbolTable = stat.getScope()
        var assignGenerated = programGenerated
        val lval: LValue = stat.l
        val rval: RValue = stat.r
        lval match {
            case id: Ident => {       
                val node = st.lookupAllDefined(id, EnumVariable)
                node match {
                    case Some(sVar: SemVariable) => {
                        // Get the offset to find where the variable is on the stack
                        val offset = sVar.getOffset()
                        assignGenerated = generateRval(rval, true, offset, assignGenerated, getLValType(lval,st))
                    }
                    case _ => sys.exit(SEMANTIC_ERROR)
                }
            }
            case a@ArrayElem(i, exprs) => {
                var arrayGeneration = assignGenerated
                val t = exprGetType(a, i.getScope())
                arrayGeneration = generateRval(rval, false, ZERO, arrayGeneration, getLValType(lval,st))
                //We store the value generated in Reg3
                arrayGeneration += Pop(ListBuffer(Reg3))
                arrayGeneration = generateExpr(i, arrayGeneration)
                //We will store the address of the ident in Reg2
                arrayGeneration += Mov(Unconditional, Reg2, Reg1)
                generatePrints()
                t match {
                    case SemChar => {
                        generateArrStoreB()
                        arrayBoundsError()
                        getArrayElem(false, arrayGeneration, Reg2, exprs, true)
                    }
                    case _ => {
                        generateArrStore()
                        arrayBoundsError()
                        getArrayElem(false, arrayGeneration, Reg2, exprs, false) 
                    }
                }
            }
            case p: PairElem =>{
                var pairGenerated = assignGenerated
                pairGenerated = generateRval(rval, false, ZERO, pairGenerated, getLValType(lval,st))
                pairGenerated += Pop(ListBuffer(Reg3))
                p match {
                    
                    case Fst(n) => {
                        pairGenerated = pairElemAddressCases(n, p, pairGenerated) 
                    }
                    case Snd(n) => {
                        pairGenerated = pairElemAddressCases(n, p, pairGenerated)
                    }

                }
                pairGenerated += Mov(Unconditional, Reg2, Reg1)
                pairGenerated += Str(Reg3, StackAccess2(Reg2, IMM_ZERO), FourBytes)
                //the address is in Reg2
            }
            case _ =>
        }
        assignGenerated
    }




    /*
        Generates the instructions and evaluate the rvalue.
        We use this method for assign and for declare. If we assign we str the value generated in FP - offset.
        If we use it for declare we push the value evaluated in the stack.
    */
    def generateRval(rVal: RValue, isAssign: Boolean, offset: Int, programGenerated : ListBuffer[Instruction], callReturnType : SemType) : ListBuffer[Instruction] = {
        var rValGenerated = programGenerated
        var pushOrStoreStat: Instruction = null 
        if(isAssign){
            pushOrStoreStat = Str(Reg1, StackAccess2(FP, ImmNum(-offset)), FourBytes)
        } else {
            pushOrStoreStat = Push(ListBuffer(Reg2))
        }
        rVal match {
            case e: Expr => {
                rValGenerated = generateExpr(e, rValGenerated)
                //We push into the stack from reg2
                rValGenerated += Mov(Unconditional, Reg2, Reg1)
                rValGenerated += pushOrStoreStat
            }
            case a@ArrayLiter(heapOrStack, exprs) => {
                val stackFlag = heapOrStack == Stack
                rValGenerated = generateRvalArrayLit(rValGenerated, isAssign, exprs, stackFlag, offset)
                rValGenerated
            }
            // We malloc for each one of the pair elemes and then we store the address of the pair in the stack
            case NewPair(heapOrStack, e1, e2) => {
                val st: SymbolTable = rVal.getScope()
                
                if(heapOrStack == Heap){
                val pairSpace: Int = 2 * WORD_SIZE
                var index: Int = ZERO
                rValGenerated = mallocPairElem(e1, rValGenerated, st, pushOrStoreStat)
                rValGenerated = mallocPairElem(e2, rValGenerated, st, pushOrStoreStat)

                rValGenerated += Mov(Unconditional, Reg0, ImmNum(pairSpace))
                rValGenerated += Bl(Unconditional, mallocLabel)
                rValGenerated += Mov(Unconditional, Reg2, Reg0)
                rValGenerated += Pop(ListBuffer(Reg1))
                index += WORD_SIZE
                rValGenerated += Str(Reg1, StackAccess2(Reg2, ImmNum(index)), FourBytes)
                rValGenerated += Pop(ListBuffer(Reg1))
                index -= WORD_SIZE
                rValGenerated += Str(Reg1, StackAccess2(Reg2, ImmNum(index)), FourBytes)
                rValGenerated += pushOrStoreStat
                } else {
                    rValGenerated = putPairElemStack(e1, rValGenerated, st, pushOrStoreStat)
                    rValGenerated = putPairElemStack(e2, rValGenerated, st, pushOrStoreStat)
                    rValGenerated += Add_IR(Reg2, SP, ImmNum(4))
                    rValGenerated += Push(ListBuffer(Reg2))
                    rValGenerated
                }

            }
            case p: PairElem => {
                p match {
                    case Fst(n) => {
                        getPairElemCases(n, p, rValGenerated, pushOrStoreStat)
                    }
                    case Snd(n) => {
                        getPairElemCases(n, p, rValGenerated, pushOrStoreStat)
                    }
                }
                
            }


            // Function calls with at least one parameter
            case Call(id@Ident(name), as@Some(ArgList(list))) => {
                // evaluate each parameter and push it to stack.
                // every time you push to stack, store offset (sp?) for the parameter
                val revd = list.reverse
                val numOfParams = revd.size
                for (param <- revd) {
                    rValGenerated = generateExpr(param, rValGenerated)
                    rValGenerated += Push(ListBuffer(Reg1))
                }
                val Some(semFunc : SemFunction) = rVal.getScope().lookupAll(id, EnumFunction(callReturnType, argListToSemTypeList(as,rVal.getScope())))
                rValGenerated += Bl(Unconditional, Label(semFunc.labelName))
                rValGenerated += Mov(Unconditional, Reg2, Reg0)
                rValGenerated += Add_IR(SP, SP, ImmNum(numOfParams*WORD_SIZE))
                rValGenerated += pushOrStoreStat
            }

            // Function calls without parameters
            case Call(id@Ident(name), None) => {
                val Some(semFunc : SemFunction) = rVal.getScope().lookupAll(id, EnumFunction(callReturnType, None))
                rValGenerated += Bl(Unconditional, Label(semFunc.labelName))
                rValGenerated += Mov(Unconditional, Reg2, Reg0)
                rValGenerated += pushOrStoreStat
            }            
        }
    }

    /*
        Generates the instructions to store the value of the array address in the stack.
        The elems are stored one after the other in memory. We also store the size of the array.
    */
    def generateRvalArrayLit(code: ListBuffer[Instruction], isAssign: Boolean, exprs: Option[List[Expr]], stackFlag: Boolean, offset: Int): ListBuffer[Instruction] = {
        var rValGenerated = code

        var pushOrStoreStat: Instruction = null 
        if(isAssign){
            pushOrStoreStat = Str(Reg1, StackAccess2(FP, ImmNum(-offset)), FourBytes)
        } else {
            pushOrStoreStat = Push(ListBuffer(Reg2))
        }

        exprs match {
                    case None => {
                        //If stack flag set just push size of array onto stack
                        if (stackFlag) {
                            rValGenerated = setUpArrayStack(rValGenerated, List(), OneByte)
                        } else {
                            val arraySize: Int = ZERO
                            val arraySpace: Int = (arraySize + ONE) * WORD_SIZE
                            rValGenerated = setUpArrayHeap(rValGenerated, arraySpace, arraySize, pushOrStoreStat)
                            rValGenerated
                        }
                    }
                    case Some(elements: List[Expr]) => {
                        elements.head match {
                            case CharLiter(_) => {
                                if (stackFlag) {
                                    if(!isAssign){
                                    rValGenerated = setUpArrayStack(rValGenerated, elements, OneByte)
                                } else {
                                    var index: Int = -(2*WORD_SIZE)
                                        elements.foreach (el => {
                                        //See if you should do generate R Val or generate Expr
                                        
                                        rValGenerated = generateExpr(el, rValGenerated)
                                        rValGenerated += Str(Reg1, StackAccess2(FP, ImmNum(-(offset+index))), FourBytes)
                                        index -= 1
                                    })
                                }
                                } else {
                                    val arraySize: Int = elements.size
                                    val arraySpace: Int = arraySize + WORD_SIZE
                                    var index = ZERO;
                                    
                                    rValGenerated = setUpArrayHeap(rValGenerated, arraySpace, arraySize, pushOrStoreStat)
                                
                                    elements.foreach (el => {
                                        rValGenerated = generateExpr(el, rValGenerated)
                                        rValGenerated += Str(Reg1, StackAccess2(Reg2, ImmNum(index)), FourBytes)
                                        index += 1
                                    })
                                }
                            }
                            case _ => {
                                if (stackFlag) {
                                    if(!isAssign){
                                        rValGenerated = setUpArrayStack(rValGenerated, elements, FourBytes)
                                    } else {
                                        var index: Int = -WORD_SIZE
                                        elements.foreach (el => {
                                        //See if you should do generate R Val or generate Expr
                                        index -= WORD_SIZE
                                        rValGenerated = generateExpr(el, rValGenerated)
                                        rValGenerated += Str(Reg1, StackAccess2(FP, ImmNum(-(offset+index))), FourBytes)
                                        })
                                    }
                                } else {
                                    val arraySize: Int = elements.size
                                    val arraySpace: Int = (arraySize + ONE) * WORD_SIZE
                                    var index: Int = -WORD_SIZE
                                    rValGenerated = setUpArrayHeap(rValGenerated, arraySpace, arraySize, pushOrStoreStat)
                                
                                    elements.foreach (el => {
                                        index += WORD_SIZE
                                        //See if you should do generate R Val or generate Expr
                                        rValGenerated = generateExpr(el, rValGenerated)
                                        rValGenerated += Str(Reg1, StackAccess2(Reg2, ImmNum(index)), FourBytes)
                                    })
                                }
                            }
                        }
                        rValGenerated
                    }
                }
        rValGenerated
    }


    //Generates code for read statements
    def generateRead (stat: ReadStat, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction]  = {
        val st = stat.getScope()
        var readGenerated = programGenerated
        stat.l match {
            case id@Ident(i) => {
                val t = exprGetType(id, id.getScope())
                readGenerated = generateExpr(id, readGenerated)
                readGenerated += Mov(Unconditional, Reg0, Reg1)
                t match {
                    case SemInt => {
                        generateReadInt()
                        readGenerated += Bl(Unconditional, readILabel)
                    }
                    case SemChar => {
                        generateReadChar()
                        readGenerated += Bl(Unconditional, readCLabel)
                        
                    }
                    case _ => sys.exit(SEMANTIC_ERROR)
                }
                val node = st.lookupAllDefined(id, EnumVariable)
                node match {
                    case Some(sVar: SemVariable) => {
                        val offset = sVar.getOffset()
                        readGenerated += Str(Reg0, StackAccess2(FP,ImmNum(-offset)), FourBytes)
                    }
                    case _ => sys.exit(SEMANTIC_ERROR)
                }
            }
            case a@ArrayElem(id, e) => {
                var isStack = false
                val arrtype = exprGetType(id, id.getScope())
                val t = exprGetType(a, id.getScope())
                arrtype match {
                    case sArr: SemArray => {
                        if(sArr.getStackFlag()){isStack = true}
                    }
                    case _ =>
                }
                var offsetL = IMM_M_FOUR
                if(isStack) {
                    offsetL = IMM_FOUR
                }
                readGenerated = generateExpr(a, readGenerated)
                matchGenReadOrChar(t, readGenerated)
                val node = st.lookupAll(id, EnumVariable)
                node match {
                    case Some(sVar: SemVariable) => {
                        readGenerated += Mov(Unconditional, Reg3, Reg0)
                        readGenerated = generateExpr(id, readGenerated)
                        readGenerated += Mov(Unconditional, Reg2, Reg1)
                        generateArrStore()
                        arrayBoundsError()
                        t match {
                            case SemChar => getArrayElem(false, readGenerated, Reg2, e, true)
                            case _ => getArrayElem(false, readGenerated, Reg2, e, false)
                        }
                    }
                    case _ => sys.exit(SEMANTIC_ERROR)
                }
            }

            case p: PairElem => {
                val t = getLValPairElem(p, p.getScope())
                matchGenReadOrChar(t, readGenerated)
                readGenerated += Mov(Unconditional, Reg3, Reg0)
                p match {
                    case Fst(n) => {
                        readGenerated = pairElemAddressCases(n, p, readGenerated)
                    }
                    case Snd(n) => {
                        readGenerated = pairElemAddressCases(n, p, readGenerated)
                    }
                }
                
                readGenerated += Mov(Unconditional, Reg2, Reg1)
                readGenerated += Str(Reg3, StackAccess2(Reg2, IMM_ZERO), FourBytes)
                //the address is in Reg2
            }
            case _ => sys.exit(SEMANTIC_ERROR)
        }
    }

    // Generates instructions to free pairs and arrays
    def generateFree (stat: FreeStat, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var freeGenerated = programGenerated
        val expr = stat.e
        freeGenerated = generateExpr(expr, freeGenerated)
        val ty = exprGetType(expr, stat.getScope())
        freeGenerated += Mov(Unconditional, Reg0, Reg1)
        ty match {
            case SemArray(_) => {
                freeGenerated += Sub_IR(Reg0, Reg0, ImmNum(WORD_SIZE))
                Bl(Unconditional, freeLabel)
            }
            case SemPairType(_,_) => {
                // We use our helper fuction to free a pair as we free each of the elements in the pair
                freeGenerated += Bl(Unconditional, freePairLabel)
                generateFreePair()}
            case _ => sys.exit(SEMANTIC_ERROR)// should not be possible to free any other types
        }
        freeGenerated
    }

    //Generates code for return statements
    def generateReturn (stat: ReturnStat, offset: Int, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var retGenerated = programGenerated
        retGenerated = generateExpr(stat.e, retGenerated)
        retGenerated += Mov(Unconditional, Reg0, Reg1)
        retGenerated += Mov(Unconditional, SP, FP)
        retGenerated += Pop(ListBuffer(FP, PC))
        retGenerated += Ltorg()
        retGenerated
    }

    //Generates code for exit statements
    def generateExit (stat: ExitStat, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var exitGenerated = programGenerated
        // evaluate the expression with generateExpr which leaves it in R1. Then branch and link with exit label   
        exitGenerated = generateExpr(stat.e, exitGenerated)
        exitGenerated += Mov(Unconditional, Reg0, Reg1)
        exitGenerated += Bl(Unconditional, exitLabel)        
        exitGenerated
    }

    // Generates code for print statements
    def generatePrintExpr(expr: Expr, scope: SymbolTable, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var printExprGenerated = programGenerated
        expr match {
            case StrLiter(s) => {
                generatePrints()
                beforeData += Word(s.length())
                val label = getStringLabel()
                beforeData += Define(label)
                beforeData += Asciz(s)
                printExprGenerated += Ldr(Reg0, ImmLoadLabel(label), FourBytes)
                printExprGenerated += Bl(Unconditional, printsLabel)
            }
            case IntLiter(i) => {
                generatePrinti()
                printExprGenerated += Mov(Unconditional, Reg0, ImmNum(i))
                printExprGenerated += Bl(Unconditional, printiLabel)
            }
            case CharLiter(c) => {
                generatePrintc()
                printExprGenerated += Mov(Unconditional, Reg0, ImmNum(c.toInt))
                printExprGenerated += Bl(Unconditional, printcLabel)
            }
            case BoolLiter(b) => {
                generatePrintb()
                printExprGenerated += Mov(Unconditional, Reg0, ImmNum(boolToInt(b)))
                printExprGenerated += Bl(Unconditional, printBLabel)
            }
            case PairLiter => {
                generatePrintp()
                printExprGenerated += Mov(Unconditional, Reg0, IMM_ZERO)
                printExprGenerated += Bl(Unconditional, printpLabel)
            }
            case i@Ident(id) => {
                val t = exprGetType(i, scope)
                printExprGenerated = generateExpr(i, printExprGenerated)
                printExprGenerated = semTypePrint(t, printExprGenerated)
            }
            case a@ArrayElem(i, e) => {
                val t = exprGetType(a, scope)
                printExprGenerated = generateExpr(a, printExprGenerated)
                printExprGenerated = semTypePrint(t, printExprGenerated)
            }
            case u:UnOpExpr => {
                val t = exprGetType(u, scope)
                printExprGenerated = generateUnOpExpr(u, printExprGenerated)
                printExprGenerated = semTypePrint(t, printExprGenerated)
            }
            case b:BinOpExpr => {
                val t = exprGetType(b, scope)
                printExprGenerated = generateBinOpExpr(b, printExprGenerated)
                printExprGenerated = semTypePrint(t, printExprGenerated)
            }
            case BracketedExpr(b) => {
                generatePrintExpr(b, scope, printExprGenerated)
            }
            case _ => sys.exit(SEMANTIC_ERROR)
        }
        printExprGenerated
    }

    //Generates code for print statements
    def generatePrint (stat: PrintStat, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        val expr = stat.e
        generatePrintExpr(expr, stat.getScope(),programGenerated)
    }

    //Generates code for println statements
    def generatePrintln (stat: PrintlnStat, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var printLnGenerated = programGenerated
        printLnGenerated = generatePrintExpr(stat.e, stat.getScope(), printLnGenerated)
        printLnGenerated += Bl(Unconditional, printlnLabel)
        generatePrintlnFormat()
        printLnGenerated
    }

    //Generates code for if statements
    def generateIf (stat: IfStat, offset : Int, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var ifOffset = offset
        var ifGenerated = programGenerated
        val (ifLabel, endLabel) = getBranchLabels()
        ifGenerated = generateExpr(stat.e, ifGenerated)
        // compare reg1 to 1 and branch to IF
        ifGenerated += Cmp(Reg1, IMM_ONE)
        ifGenerated += B(IR_EQ, ifLabel)   
        stat.el.foreach { s => {
            val (newIfGenerated, newOffset) = generateStat(s,ifOffset, ifGenerated)
            ifOffset = newOffset
            ifGenerated = newIfGenerated
            }
        }
        ifGenerated += B(Unconditional, endLabel)
        ifGenerated += Define(ifLabel)
        ifGenerated += Mov(Unconditional, SP, FP)
        ifGenerated += Add_IR(SP, SP, ImmNum(-ifOffset+WORD_SIZE))
        stat.t.foreach { s => {
            val (newIfGenerated, newOffset) = generateStat(s,ifOffset, ifGenerated)
            ifOffset = newOffset
            ifGenerated = newIfGenerated
            }
        }
        ifGenerated += Define(endLabel)
        ifGenerated = branchBack(offset, ifGenerated)
        ifGenerated
    }

    //Generates code for while statements
    def generateWhile(stat: WhileStat, offset : Int, programGenerated : ListBuffer[Instruction]): ListBuffer[Instruction] = {
        var whileOffset = offset
        var whileGenerated = programGenerated

        val (condLabel, whileLabel) = getBranchLabels()

        whileGenerated += B(Unconditional, condLabel)
        whileGenerated += Define(whileLabel)
        stat.s.foreach { s => { 
            val (newWhileGenerated, newOffset) = generateStat(s,whileOffset, whileGenerated)
            whileOffset = newOffset
            whileGenerated = newWhileGenerated
            }
        }
        //If the condition is satisfied we brnach back
        whileGenerated = branchBack(offset, whileGenerated)
        whileGenerated += Define(condLabel)
        whileGenerated = generateExpr(stat.e, whileGenerated)
        whileGenerated += Cmp(Reg1, IMM_ONE)
        whileGenerated += B(IR_EQ, whileLabel)
        whileGenerated
    }

    // Generates instructions for begin statements
    def generateBegin(stat : BeginStat, offset : Int, programGenerated : ListBuffer[Instruction]) : ListBuffer[Instruction] = {
        var beginGenerated = programGenerated
        var beginOffset = offset
        for (ss <- stat.s) {
            val (newBeginGenerated, newOffset) = generateStat(ss,beginOffset, beginGenerated)
            beginOffset = newOffset
            beginGenerated = newBeginGenerated
        }
        beginGenerated = branchBack(offset, beginGenerated)
        beginGenerated
    }
}