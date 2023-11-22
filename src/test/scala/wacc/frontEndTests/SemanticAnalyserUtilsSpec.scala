// package frontEndTests

// import semanticAnalyserUtils._
// import semanticAst._
// import ast._
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.should.Matchers._

// class SemanticAnalyserUtilsSpec extends AnyFlatSpec {
//     "The baseTypeToObj function" should "Return the correct SemNodes for BaseTypes" in {
//         info("It should provide a SemInt if given an IntType BaseType")
//         baseTypeToIdObj(IntType) shouldBe SemInt
//         info("It should provide a SemBool if given an BoolType BaseType")
//         baseTypeToIdObj(BoolType) shouldBe SemBool
//         info("It should provide a SemChar if given an CharType BaseType")
//         baseTypeToIdObj(CharType) shouldBe SemChar
//         info("It should provide a SemString if given an StringType BaseType")
//         baseTypeToIdObj(StringType) shouldBe SemString
//     }
//     "The pairElemTypeToObj function" should "Return the correct SemNodes for PairElemTypes" in {
//         info("It should provide a SemPairUnknown if given an PaiElem type Pair")
//         pairElemTypeToIdObj(Pair) shouldBe SemPairUnknown
//         info("It should provide a SemInt if given an IntType BaseType")
//         // pairElemTypeToIdObj(ArrayType(t)) shouldBe SemArray()
//         info("It should provide a SemInt if given an IntType BaseType")
//         pairElemTypeToIdObj(IntType) shouldBe SemInt

//     }

//     "The typeToIdObj function" should "Give the correct SemNodes for any inputted types" in {
//         info("BoolType should provide SemBool")
//         typeToIdObj(BoolType) shouldBe SemBool
//         info("ArrayType of Integer type should provide SemArray(SemInt)")
//         typeToIdObj(ArrayType(IntType)) shouldBe SemArray(SemInt)
//         info("BoolType should provide SemBool")
//         typeToIdObj(PairType(IntType, StringType)) shouldBe SemPairType(SemInt,SemString)
//     }
    
//     "The areEqualTypes function" should "Return true if there are two equal types" in {
//         val t1 = SemString
//         val t2 = SemString
//         areEqualTypes(t1,t2) shouldBe true
//     }

//     it should "Return false if there are two non-equal types" in {
//         val t1 = SemString
//         val t2 = SemChar
//         areEqualTypes(t1,t2) shouldBe false
//     }
    
//     it should "Return true if either of the inputted types are a SemAny" in {
//         info("Should return true if the first type is a SemAny")
//         val t1 = SemAny
//         val t2 = SemInt
//         areEqualTypes(t1,t2) shouldBe true
//         info("Should return true if the second type is a SemAny")
//         val t3 = SemBool
//         val t4 = SemAny
//         areEqualTypes(t3,t4) shouldBe true
//     }
// }