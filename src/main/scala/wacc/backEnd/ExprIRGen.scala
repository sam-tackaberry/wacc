package wacc.backEnd

import frontEnd.ast._
import frontEnd.symbolTable._
import frontEnd.semanticAst._
import scala.collection.mutable.ListBuffer
import io._
import utils._
import assemblyIR._
import constants._
import rteChecking._
import heapFunctions._
import frontEnd.ast.Add
import frontEnd.ast.Mul
import frontEnd.getSemType._

object exprGenerate {

    // Generates instructions for expressions.
    def generateExpr(e: Expr, programGenerated: ListBuffer[Instruction])
                     : ListBuffer[Instruction] = {
        var exprGenerated = programGenerated
        exprGenerated += Push(ListBuffer(Reg2, Reg3, Reg4))
        e match {
            case IntLiter(i) => {
                if (i >= ZERO && i <= maxMovInt) {
                    exprGenerated += Mov(Unconditional, Reg1, ImmNum(i))
                } else {
                    exprGenerated += Ldr(Reg1, ImmLoadNum(i), FourBytes)
                }

            }
            case BoolLiter(b) => exprGenerated += Mov(Unconditional, Reg1, ImmNum(boolToInt(b)))
            case CharLiter(c) => exprGenerated += Mov(Unconditional, Reg1, ImmNum(c.toInt))
            case StrLiter(s) => {
                val label = generateStrDeclare(s)
                exprGenerated += Ldr(Reg1, ImmLoadLabel(label), FourBytes)
            }
            case PairLiter => exprGenerated += Mov(Unconditional, Reg1, IMM_ZERO)
            case b:BinOpExpr => generateBinOpExpr(b, exprGenerated)
            case u: UnOpExpr => generateUnOpExpr(u, exprGenerated)
            case BracketedExpr(exp) => generateExpr(exp, exprGenerated)
            case id@Ident(n) => {
                val st: SymbolTable = id.getScope()
                val node = st.lookupAll(id, EnumVariable)
                
                node match {
                    case Some(x: SemVariable) => {
                        val offset = x.getOffset()
                        exprGenerated += Ldr(Reg1, StackAccess2(FP, ImmNum(-offset)), FourBytes)
                        
                    }
                    case _ => sys.exit(SEMANTIC_ERROR)
                }
            }   
            case a@ArrayElem(id, exprs) => {
                var isStack = false
                val arrtype = exprGetType(id, id.getScope())
                val t = exprGetType(a, id.getScope())
                arrtype match {
                    case sArr: SemArray => {
                        if(sArr.getStackFlag()){
                            isStack = true
                            val st: SymbolTable = id.getScope()
                            val node = st.lookupAll(id, EnumVariable)
                            node match {
                                case Some(x: SemVariable) => {
                                    val offset = x.getOffset()
                                    exprGenerated += Ldr(Reg1, StackAccess2(FP, ImmNum(-offset)), FourBytes)
                                }
                                 case _ => sys.exit(SEMANTIC_ERROR)
                            }
                        } else {
                            exprGenerated = generateExpr(id, exprGenerated)
                        }
                    }
                    case _ => sys.exit(SEMANTIC_ERROR)
                }
                
                //we will store the offset of the ident in Reg2
                exprGenerated += Mov(Unconditional, Reg2, Reg1)
                generatePrints()
                
                t match {
                    case SemChar => {
                        generateArrLoadB()
                        arrayBoundsError()
                        exprGenerated = getArrayElem(true, exprGenerated, Reg2, exprs, true)
                            
                    }
                    case _ => {
                        generateArrLoad()
                        arrayBoundsError()
                        exprGenerated = getArrayElem(true, exprGenerated, Reg2, exprs, false)
                    }
                }
            }  
            case _ => sys.exit(SEMANTIC_ERROR)
        }
        exprGenerated += Pop(ListBuffer(Reg2, Reg3, Reg4))
    } 

    /* 
        Generates the label if it is a store or load instruction.
        If it is a char array we use the storeB and loadB functions.
     */
    def genLoadOrStore(isLoad: Boolean, isCharArray: Boolean) : Label = {
        var newLabel = arrayLoadBLabel
        if (isCharArray){
            if(isLoad){ 
                newLabel = arrayLoadBLabel
            } else {
                generateArrStoreB()
                arrayBoundsError()
                newLabel = arrayStoreBLabel
            }       
        } else {
            if(isLoad){ 
                arrayBoundsError()
                generateArrLoad()
                newLabel = arrayLoadLabel
            } else {
                generateArrStore()
                arrayBoundsError()
                newLabel = arrayStoreLabel
            }
        }
        newLabel
    }

    /* 
        Generates instructions for getting an array elem given a base address,
        and a list of expressions.

        The index of the elem will be stored in Reg10
        The address of the array will be stored in Reg2
    */
    def getArrayElem(isLoad: Boolean, assemblyGenerated: ListBuffer[Instruction], 
                address: Register, exs: List[Expr], charArray: Boolean): ListBuffer[Instruction] = {
                    val head::tail = exs
                    var newLabel = arrayLoadLabel
                    if(exs.length > 1){                      
                            generateArrLoad()
                            arrayBoundsError()
                            newLabel = arrayLoadLabel               
                        } else {
                            newLabel = genLoadOrStore(isLoad, charArray)
                        }
                    var helperGenerate = ListBuffer[Instruction]()
                    helperGenerate = generateExpr(head, assemblyGenerated)
                    // Move the index from Reg1 to Reg10
                    helperGenerate += Mov(Unconditional, Reg10, Reg1)
                
                    helperGenerate += Mov(Unconditional, Reg2, address)
                    helperGenerate += Bl(Unconditional, newLabel)
                  
                    if(tail.isEmpty){
                        helperGenerate += Mov(Unconditional, Reg1, Reg2)
                    } else {
                        println(exs.length)
                        // We call the method recursevly on the remaining exprs
                        helperGenerate = getArrayElem(isLoad, helperGenerate, Reg2, tail, charArray) 
                    }
                    helperGenerate
                }

    // Generates instructions for unary operation expressions.
    def generateUnOpExpr(e: UnOpExpr, programGenerated: ListBuffer[Instruction])
                        : ListBuffer[Instruction] = {
        var unop = new ListBuffer[(Instruction)]()
        unop = generateExpr(e.expr, programGenerated)
        unop += Push(ListBuffer(Reg1))

        unop += Pop(ListBuffer(Reg1))

        e.unop match{
            case Not() => {
                unop += Cmp(Reg1, IMM_ONE)
                unop += Mov(IR_NE, Reg1, IMM_ONE)
                unop += Mov(IR_EQ, Reg1, IMM_ZERO)
            }
            case Neg() => {
                unop += Mov(Unconditional, Reg2, IMM_ZERO)
                unop += Sub_IR(Reg1, Reg2, Reg1)
                unop += Bl(IR_VS, overflowLabel)
                overflowError()
            }
            case Len() => unop += Ldr(Reg1, StackAccess2(Reg1, IMM_M_FOUR), FourBytes)

            //Ord and Chr cases are already implicity handled in assembly
            case _ => {}
        }
        unop
    }

    // Generates instructions for binary operation expressions. 
    def generateBinOpExpr(e: BinOpExpr, programGenerated: ListBuffer[Instruction])
                        : ListBuffer[Instruction] = {
        var binop = new ListBuffer[Instruction]()
        binop = generateExpr(e.e2, programGenerated)
        binop += Push(ListBuffer(Reg1))
        binop = generateExpr(e.e1, binop)
        binop += Push(ListBuffer(Reg1))

        e.binop match {
            //Arithmetic Operations

            // Check for overflow errors
            case Minus() => {
                binop = popR1R2(binop)
                binop += Sub_IR(Reg1, Reg1, Reg2)
                binop  += Bl(IR_VS, overflowLabel)
                overflowError()
            }
            case Add() => {
                binop = popR1R2(binop)
                binop += Add_IR(Reg1, Reg1, Reg2)
                binop += Bl(IR_VS, overflowLabel) 
                overflowError()
            }
            case Mul() => {
                binop += Pop(ListBuffer(Reg3))
                binop += Pop(ListBuffer(Reg4))
                binop += Mul_IR(Reg1, Reg2, Reg3, Reg4)
                binop += Cmp(Reg2, Asr(Reg1, ImmNum(31)))
                binop += Bl(IR_NE, overflowLabel)
                overflowError()
            }
            // Check that we are not dividing by zero otherwise throw error
            case Div() => {
                binop = applyDivMod(binop)
                binop += Mov(Unconditional, Reg1, Reg0)
            }
            case Mod() => binop = applyDivMod(binop)

            // Comparison operations
            case GT() => binop = comparisonOper(binop, IR_GT, IR_LE)
            case LT() => binop = comparisonOper(binop, IR_LT, IR_GE)
            case GTEq() => binop = comparisonOper(binop, IR_GE, IR_LT)
            case LTEq() => binop = comparisonOper(binop, IR_LE, IR_GT)
            case Eq() => binop = comparisonOper(binop, IR_EQ, IR_NE)
            case Neq() => binop = comparisonOper(binop, IR_NE, IR_EQ)

            // Logical operations
            case And() => binop = logicalOper(binop, IR_NE)         
            case Or() => binop = logicalOper(binop, IR_EQ)
        }
        binop
    }
    
    /*
        Used when you want to compare two expressions, using "greater than", 
        "less than", etc..
        The first condition corresponds to the condition we are comparing the 
        expressions with. The oppCond is the opposite condition meaning that
        our comparison was incorrect.
    */
    def comparisonOper(program: ListBuffer[Instruction], cond: Condition, oppCond: Condition)
                        : ListBuffer[Instruction] = {
        var binop = program
        binop = popR1R2(binop)
        binop += Cmp(Reg1, Reg2)
        binop += Mov(cond, Reg1, IMM_ONE)
        binop += Mov(oppCond, Reg1, IMM_ZERO)
        binop
    }

    /*
        Used when we want to use the logical operations such as "And" and "Or"
        We Branch with a condition to shortcut the operation, lazy evaluate.
        For example false And true after evaluating the first exp(false) we 
        evaluate the whole expr to false without evaluating exp2
    */
    def logicalOper(program: ListBuffer[Instruction], cond: Condition): ListBuffer[Instruction] = {
        var binop = program
        val label = getNextLabel()
        binop = popR1R2(binop)
        binop += Cmp(Reg1, IMM_ONE)
        binop += B(cond, label)
        binop += Cmp(Reg2, IMM_ONE)
        binop += Define(label)
        binop += Mov(IR_EQ, Reg1, IMM_ONE) 
        binop += Mov(IR_NE, Reg1, IMM_ZERO)
    }

    /* 
        Generate instructions for the div and mod instructions. 
        Check for runtime errors (division by zero)
    */
    def applyDivMod(binop: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        binop += Pop(ListBuffer(Reg0))
        binop += Pop(ListBuffer(Reg1))
        binop += Cmp(Reg1, IMM_ZERO)
        binop += Bl(IR_EQ, divByZeroLabel)
        binop += Bl(Unconditional, divModLabel)
        divByZeroError()
        binop
    }

    // Common instruction when evaluating expr.
    def popR1R2(binop: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        binop += Pop(ListBuffer(Reg1))
        binop += Pop(ListBuffer(Reg2))
        binop
    }

}
