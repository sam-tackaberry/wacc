package wacc.backEnd

import scala.collection.mutable.ListBuffer

object assemblyIR {

    sealed trait Instruction 
    
    // Define/Label
    case class Define(name: Label) extends Instruction
    case class Label(name: String) extends Instruction

    // Arithmetic
    sealed trait BinOp extends Instruction
    case class Add_IR(dest: Operand, op1: Operand, op2: Operand) extends BinOp
    case class Sub_IR(dest: Operand, op1: Operand, op2: Operand) extends BinOp
    case class Mul_IR(dest1: Operand, dest2: Operand, op1: Operand, op2: Operand) extends BinOp
    
    // Compare
    sealed trait Compare extends Instruction
    case class Cmp(op1: Operand, op2: Operand) extends Compare

    // Logical
    sealed trait Logical extends Instruction
    case class Mov(cond: Condition, dest: Operand, source: Operand) extends Logical
   
    // Branch
    sealed trait Branch extends Instruction
    case class B(cond: Condition, l: Label) extends Branch
    case class Bl(cond: Condition, l: Label) extends Branch

    //Conditions 
    sealed trait Condition
    case object Unconditional extends Condition
    case object IR_EQ extends Condition
    case object IR_GT extends Condition
    case object IR_LT extends Condition
    case object IR_GE extends Condition
    case object IR_LE extends Condition
    case object IR_NE extends Condition
    case object IR_VS extends Condition
    
    // Store/Load
    sealed trait Mem extends Instruction
    case class Str(dest: Operand, source: Operand, size: Size) extends Mem
    case class StrPost(dest: Operand, source: Operand, size: Size) extends Mem
    case class Ldr(source: Operand, dest: Operand, size: Size) extends Mem

    sealed trait Size
    case object FourBytes extends Size
    case object OneByte extends Size

    // Push/Pop
    sealed trait Stack extends Instruction 
    case class Push(ops: ListBuffer[Operand]) extends Stack
    case class Pop(ops: ListBuffer[Operand]) extends Stack

    // Operands
    sealed trait Operand
    case class ImmNum (num: Int) extends Operand
    case class ImmLoadNum (num: Int) extends Operand
    case class ImmLoadLabel(l: Label) extends Operand

    sealed trait Print extends Instruction
    case class Asciz(str: String) extends Print
    case class Word(length: Int) extends Print

    // Registers
    sealed trait Register extends Operand
    case object Reg0 extends Register
    case object Reg1 extends Register
    case object Reg2 extends Register
    case object Reg3 extends Register
    case object Reg4 extends Register
    case object Reg5 extends Register
    case object Reg6 extends Register
    case object Reg7 extends Register
    case object Reg8 extends Register
    case object Reg9 extends Register
    case object Reg10 extends Register
    case object FP extends Register
    case object Reg12 extends Register
    case object SP extends Register
    case object LR extends  Register
    case object PC extends Register

    // Accessing memory or stack
    sealed trait StackAccess extends Operand
    case class StackAccess2(op1: Operand, op2: Operand) extends StackAccess

    // Shifting Registers 
    sealed trait ShiftRegister extends Operand
    case class Lsl(reg: Register, num: ImmNum) extends ShiftRegister
    case class Asr(reg: Register, num: ImmNum) extends ShiftRegister

    sealed trait SpecialOps extends Instruction
    case class Ltorg() extends SpecialOps

}