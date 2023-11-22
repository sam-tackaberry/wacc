package wacc.backEndTests

import wacc.backEnd.translateInstr._
import wacc.backEnd.assemblyIR._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ListBuffer




class TranslateInstrSpec extends AnyFlatSpec with Matchers {

    "translateRegister method" should "return a valid string representation of standard (numbered) registers" in {
        val res = ListBuffer[String]()
        res += translateRegister(Reg0)
        res += translateRegister(Reg1)
        res += translateRegister(Reg10)
        res should equal (ListBuffer("r0", "r1", "r10"))
    }

    "translateRegister method" should "return a valid string representation of special (named) registers" in {
        val res = ListBuffer[String]()
        res += translateRegister(FP)
        res += translateRegister(SP)
        res += translateRegister(LR)
        res += translateRegister(PC)
        res should equal (ListBuffer("fp", "sp", "lr", "pc"))
    }

    "translateLabel method" should "return a valid translation of a label0" in {
        val res = translateLabel(Label(".L0"))
        res should equal (".L0")
    }
    
    "translateLabel method" should "return a valid translation of a label1" in {
        val res = translateLabel(Label(".L1"))
        res should equal (".L1")
    }
    
    "translateStackAccess method" should "return a valid ARM11 stack access with correct operands "in {
        val res = translateStackAccess(List(FP, ImmNum(-4)))
        res should equal("[fp, #-4]")
    }

    "translateOperand method" should "translate immediate numbers for move (#) and numbers for load (=) correctly" in {
        val res = ListBuffer[String]()
        res += translateOperand(ImmNum(-4))
        res += translateOperand(ImmLoadNum(-8))
        res should equal (ListBuffer("#-4", "=-8"))
    }

    "translateInstr method" should "produce valid ldr and str instructions" in {
        val res = ListBuffer[String]()
        res += translateInstr(Mov(Unconditional, Reg1, Reg0))
        res += translateInstr(Ldr(Reg1, StackAccess2(FP, ImmNum(-4)), FourBytes))
        res += translateInstr(Str(Reg2, StackAccess2(FP, ImmNum(-8)), FourBytes))
        res should equal (ListBuffer("\t\tmov r1, r0", "\t\tldr r1, [fp, #-4]", "\t\tstr r2, [fp, #-8]"))
    }

}