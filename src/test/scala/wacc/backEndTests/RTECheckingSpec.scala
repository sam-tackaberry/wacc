package wacc.backEndTests

import wacc.backEnd.utils._
import wacc.backEnd.rteChecking._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ListBuffer

class RTECheckingSpec extends AnyFlatSpec with Matchers {
    "divByZeroError method" should "set the divByZeroFlag" in {
        val res = ListBuffer[Boolean]()
        res += divByZeroFlag
        divByZeroError()
        res += divByZeroFlag
        res should equal (ListBuffer(false, true))
    }

    "overflow method" should "set the overflowFlag" in {
        val res = ListBuffer[Boolean]()
        res += overflowFlag
        overflowError()
        res += overflowFlag
        res should equal (ListBuffer(false, true))
    }

    "nullDereferenceError method" should "set the nullDereferenceFlag" in {
        val res = ListBuffer[Boolean]()
        res += nullDereferenceFlag
        nullDereferenceError()
        res += nullDereferenceFlag
        res should equal (ListBuffer(false, true))
    }

    "arrayBoundsError method" should "set the arrayBoundFlag" in {
        val res = ListBuffer[Boolean]()
        res += arrayBoundFlag
        arrayBoundsError()
        res += arrayBoundFlag
        res should equal (ListBuffer(false, true))
    }




}