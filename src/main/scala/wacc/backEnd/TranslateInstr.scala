package wacc.backEnd

import assemblyIR._
import constants._
import utils._

/*
    Translate the instructions into arm32 assembly.
    As the Instructions are general, we check the types of the operands 
    using the arm convention.
    We exit with code 1 if the operands types do not match the arm operands types.
*/
object translateInstr {

    // Operands type checking error
    def incompatibleWithARMErr () = {
        println("The operands do not follow the conventions of arm instr")
        sys.exit(1)
    }

    def translateInstr(instr: Instruction) : String = {
        instr match {
            case Define(name) => tab + translateLabel(name) + ":"
            case l:Label => tab + translateLabel(l)
            
            //Arithemtic
            case a:BinOp => twoTabs + translateArith(a)
            // Compare
            case Cmp(op1, op2) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                twoTabs + "cmp "  + translateOperands(List(op1, op2))
            }

            // Logical
            case Mov(cond, op1, op2) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                twoTabs + "mov" + translateCond(cond) + translateOperands(List(op1, op2))
            }
    
            // Branch
            case B(cond, l) => {twoTabs + "b" + translateCond(cond) + translateLabel(l)}
            case Bl(cond, l) => {twoTabs + "bl" + translateCond(cond) + translateLabel(l)}

            // Store    
            case Str(op1, op2, FourBytes) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                twoTabs + "str " + translateOperands(List(op1, op2))
            }
            case StrPost(op1, op2, FourBytes) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                twoTabs + "str " + translateOperands(List(op1, op2)) + "!"
            }

            case Str(op1, op2, OneByte) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()

                }
                twoTabs + "strb " + translateOperands(List(op1, op2))
            }
            case StrPost(op1, op2, OneByte) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                twoTabs + "strb " + translateOperands(List(op1, op2)) + "!"
            }

            // Load
            case Ldr(op1, op2, FourBytes) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                twoTabs + "ldr " + translateOperands(List(op1, op2))
            }

            case Ldr(op1, op2, OneByte) => {
                if(!op1.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                twoTabs + "ldrsb " + translateOperands(List(op1, op2))
            }


            // Push/Pop
            case Push(ops) => {
                ops.foreach(op => {
                    if(!op.isInstanceOf[Register] ){
                        incompatibleWithARMErr()
                }
                })
                twoTabs + "push " + "{" + translateOperands(ops.toList) + "}"
            }
            case Pop(ops) => {
                ops.foreach(op => {
                    if(!op.isInstanceOf[Register] ){
                    incompatibleWithARMErr()
                }
                })
                twoTabs + "pop "+ "{" + translateOperands(ops.toList) + "}"
            }

            case Asciz(str) => {twoTabs + ".asciz " + "\"" + specialChars(str) + "\""}
            case Word(len) => {twoTabs + ".word " + len}

            case Ltorg() => {twoTabs + ".ltorg"}

        }
    }

    // Give a representation for each condition
    def translateCond(cond: Condition): String = {
        cond match {
            case IR_EQ => EQ_STR
            case IR_GT => GT_STR
            case IR_LT => LT_STR 
            case IR_GE => GE_STR
            case IR_LE => LE_STR
            case IR_NE => NE_STR 
            case IR_VS => VS_STR
            case Unconditional => space          
        }
    }

    // Translate a register shift
    def translateShift(shift: ShiftRegister): String = {
        shift match {
            case Lsl(reg, op) => {
                if(!op.isInstanceOf[ImmNum]){
                    incompatibleWithARMErr()
                }
                translateRegister(reg) + ", lsl " + translateOperand(op)
            }
            case Asr(reg, op) => {
                if(!op.isInstanceOf[ImmNum]){
                    incompatibleWithARMErr()
                }
                translateRegister(reg) + ", asr " + translateOperand(op)
            }
        }
    }

    // Translate the arithmetic instructions such as add, sub and mul
    def translateArith(binop: BinOp): String = {
        binop match {
            case Add_IR(dest, op1, op2) => {
                if(!dest.isInstanceOf[Register] || !op1.isInstanceOf[Register]){
                    incompatibleWithARMErr()
                }
                "adds " + translateOperands(List(dest, op1, op2))
        }
            case Sub_IR(dest, op1, op2) => {
                if(!dest.isInstanceOf[Register] || !op1.isInstanceOf[Register]){
                    incompatibleWithARMErr()
                }
                "subs " + translateOperands(List(dest, op1, op2))
        }
            case Mul_IR(dest1, dest2, op1, op2) => {
                if(!dest1.isInstanceOf[Register] || !dest2.isInstanceOf[Register]
                 || !op1.isInstanceOf[Register] || !op2.isInstanceOf[Register]){
                                        incompatibleWithARMErr()

                }
                "smull " + translateOperands(List(dest1, dest2, op1, op2))
        }
        }
    }
    
    def translateOperands(ops: List[Operand]) : String = {
        var operandsTranslated = ""
        if (ops.length == 1){
            translateOperand(ops.head)
        }else{
            for (op <- ops.dropRight(1)) {
            operandsTranslated += translateOperand(op) + argSep
            }
        operandsTranslated += translateOperand(ops.last)
        operandsTranslated
        }
    }

    // Translate the label
    def translateLabel(l: Label) : String = l.name
    
    // Match and translate each operand case
    def translateOperand(op: Operand) : String = {
        op match {
            case ImmNum (num) => "#" + num
            case ImmLoadNum (num) => "=" + num
            case ImmLoadLabel(Label(s)) => "=" + translateLabel(Label(s))

            case r:Register => translateRegister(r)
            case StackAccess2 (op1, op2) => translateStackAccess(List(op1, op2))
                
            case s:ShiftRegister => translateShift(s)
        }
    }

    // Translate the memory/ stack access into a string
    def translateStackAccess(l: List[Operand]): String = {
        return "[" + translateOperands(l) + "]"
    }

    // Translate each register into its string representation
    def translateRegister(reg: Register) : String = {
        reg match {
            case Reg0 => {"r0"}
            case Reg1 => {"r1"}
            case Reg2 => {"r2"}
            case Reg3 => {"r3"}
            case Reg4 => {"r4"}
            case Reg5 => {"r5"}
            case Reg6 => {"r6"}
            case Reg7 => {"r7"}
            case Reg8 => {"r8"}
            case Reg9 => {"r9"}
            case Reg10 => {"r10"}
            case FP => {"fp"}
            case Reg12 => {"r12"}
            case SP => {"sp"}
            case LR => {"lr"}
            case PC => {"pc"}
        }
    }
}