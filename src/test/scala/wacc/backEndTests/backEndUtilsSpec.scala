package wacc.backEndTests

import wacc.backEnd.utils._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ListBuffer



class BackEndUtilsSpec extends AnyFlatSpec with Matchers {
    "boolToInt method" should "return 1 when true inputted" in {
      val res = boolToInt(true)
      res should equal (1)
    }

   "boolToInt method" should "return 0 when false inputted" in {
      val res = boolToInt(false)
      res should equal (0)
    }

    "getLabelCounter method" should "return distinct (increasing) labels every time" in {
        val res = ListBuffer[Int]()
        res += getLabelCounter()
        res += getLabelCounter()
        res += getLabelCounter()
        res += getLabelCounter()
        res += getLabelCounter()
        res += getLabelCounter()
        res += getLabelCounter()
        res += getLabelCounter()
        res += getLabelCounter()
        res should equal(ListBuffer(0,1,2,3,4,5,6,7,8))
    }


    "getStringCounter method" should "return distinct (increasing) labels for strings every time" in {
        val res = ListBuffer[Int]()
        res += getStringCounter()
        res += getStringCounter()
        res += getStringCounter()
        res += getStringCounter()
        res += getStringCounter()
        res += getStringCounter()
        res += getStringCounter()
        res += getStringCounter()
        res += getStringCounter()
        res should equal(ListBuffer(0,1,2,3,4,5,6,7,8))
    }


}
