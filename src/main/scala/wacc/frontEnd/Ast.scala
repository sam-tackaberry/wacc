package frontEnd

import parsley.genericbridges._
import parserBridgePos._
import symbolTable._
object ast {

  sealed trait ScopeNode {
    var scope: SymbolTable = null
    def addScope(newScope: SymbolTable): Unit = {
      scope = newScope;
    }
    def getScope() : SymbolTable = {
      scope
    }
  }


  case class WACCProgram(imports: List[Import], functions: List[Function], body : List[Stat])(val pos: (Int, Int)) extends ScopeNode
  object WACCProgram extends ParserBridgePos3[List[Import], List[Function], List[Stat], WACCProgram]

  case class Function(typeAndIdent: (Type, Ident) , paramList : Option[ParamList], body : List[Stat])(val pos: (Int, Int)) extends ScopeNode
  object Function extends ParserBridgePos3[(Type, Ident), Option[ParamList], List[Stat], Function]

  case class ParamList(paramList : List[Param]) 
  object ParamList extends ParserBridge1[List[Param], ParamList]

  case class Param(argType : Type, argIdent : Ident)(val pos: (Int, Int)) extends ScopeNode
  object Param extends ParserBridgePos2[Type, Ident, Param]
 

  case class Import(lib: Library)
  object Import extends ParserBridge1[Library, Import]

  case class Library(name: String)
  object Library extends ParserBridge1[String, Library]
  

  sealed trait Stat extends ScopeNode

  case object Skip extends Stat with ParserBridge0[Stat]
  case class Declare(t: Type, i: Ident, r: RValue)(val pos: (Int, Int)) extends Stat 
  case class Assign(l: LValue, r : RValue)(val pos: (Int, Int)) extends Stat
  case class ReadStat(l:LValue)(val pos: (Int, Int)) extends Stat
  case class FreeStat(e: Expr)(val pos: (Int, Int)) extends Stat 
  case class ReturnStat(e: Expr)(val pos: (Int, Int)) extends Stat
  case class ExitStat(e: Expr)(val pos: (Int, Int)) extends Stat
  case class PrintStat(e: Expr)(val pos: (Int, Int)) extends Stat
  case class PrintlnStat(e: Expr)(val pos: (Int, Int)) extends Stat 
  case class IfStat(e: Expr, t: List[Stat], el: List[Stat])(val pos: (Int, Int)) extends Stat 
  case class WhileStat(e: Expr, s: List[Stat])(val pos: (Int, Int)) extends Stat
  case class BeginStat(s: List[Stat])(val pos: (Int, Int)) extends Stat

  object Declare extends ParserBridgePos3[Type, Ident, RValue, Declare]
  object Assign extends ParserBridgePos2[LValue, RValue, Assign]
  object ReadStat extends ParserBridgePos1[LValue, ReadStat]
  object FreeStat extends ParserBridgePos1[Expr, FreeStat]
  object ReturnStat extends ParserBridgePos1[Expr, ReturnStat]
  object ExitStat extends ParserBridgePos1[Expr, ExitStat]
  object PrintStat extends ParserBridgePos1[Expr, PrintStat]
  object PrintlnStat extends ParserBridgePos1[Expr, PrintlnStat]
  object IfStat extends ParserBridgePos3[Expr, List[Stat], List[Stat], IfStat]
  object WhileStat extends ParserBridgePos2[Expr, List[Stat], WhileStat]
  object BeginStat extends ParserBridgePos1[List[Stat], BeginStat]

  sealed trait LValue extends ScopeNode

  sealed trait PairElem extends LValue with RValue 
  case class Fst(lValue: LValue)(val pos: (Int, Int)) extends PairElem
  case class Snd(lValue: LValue)(val pos: (Int, Int)) extends PairElem
 
  object Fst extends ParserBridgePos1[LValue, Fst]
  object Snd extends ParserBridgePos1[LValue, Snd]
 
  sealed trait RValue extends ScopeNode
 
  case class NewPair(heapOrStack: HeapOrStack, e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
  object NewPair extends ParserBridgePos3[HeapOrStack, Expr, Expr, NewPair]
 
  case class Call(i: Ident, as: Option[ArgList])(val pos: (Int, Int)) extends RValue
  object Call extends ParserBridgePos2[Ident, Option[ArgList], Call]
 
 
  case class ArgList(es: List[Expr])(val pos: (Int, Int)) extends ScopeNode
  object ArgList extends ParserBridgePos1[List[Expr], ArgList]
 
  sealed trait Type
  sealed trait BaseType extends Type with PairElemType
 
  case object IntType extends BaseType with ParserBridge0[BaseType]
  case object BoolType extends BaseType with ParserBridge0[BaseType]
  case object CharType extends BaseType with ParserBridge0[BaseType]
  case object StringType extends BaseType with ParserBridge0[BaseType]
  case class StaticArrayType(t:Type, size: Int) extends TypeAndPairElemType
  object StaticArrayType extends ParserBridge2[Type, Int, StaticArrayType]
  case class ArrayType(t:Type) extends TypeAndPairElemType
  object ArrayType extends Type with ParserBridge1[Type, ArrayType]

  sealed trait TypeAndPairElemType extends Type with PairElemType

  case class StaticDynamicArrayType(t: Type, size: Int) extends TypeAndPairElemType

  object StaticDynamicArrayType extends ParserBridge2[Type, Option[Int], TypeAndPairElemType] {
    def apply (t: Type, size: Option[Int]) : TypeAndPairElemType = 
      size match {
        case Some(s) => StaticArrayType(t, s)
        case None => ArrayType(t)
      }
  }
 
  case class PairType(pairType1 : PairElemType, pairType2 : PairElemType) extends Type with PairElemType
  object PairType extends ParserBridge2[PairElemType, PairElemType, PairType]
 
  sealed trait PairElemType
  case object Pair extends PairElemType with ParserBridge0[PairElemType] 

  sealed trait Expr extends RValue 
  sealed trait UnaryOp
  sealed trait BinaryOp

  case class UnOpExpr(unop: UnaryOp, expr: Expr) extends Expr 
  case class BinOpExpr(binop: BinaryOp, e1: Expr, e2: Expr)(val pos: (Int, Int)) extends Expr 
  case class BracketedExpr(e: Expr)(val pos: (Int, Int)) extends Expr 

  object UnOpExpr extends ParserBridge2[UnaryOp, Expr, UnOpExpr]
  object BinOpExpr extends ParserBridgePos3[BinaryOp, Expr, Expr, BinOpExpr]
  object BracketedExpr extends ParserBridgePos1[Expr, BracketedExpr]

  sealed trait ExprAndLValue extends Expr with LValue

  case class Ident(name: String)(val pos: (Int, Int)) extends ExprAndLValue with ScopeNode
  object Ident extends ParserBridgePos1[String, Ident]

  case class ArrayElem(val i: Ident, val e : List[Expr])(val pos: (Int, Int)) extends ExprAndLValue with ScopeNode
  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]

  case class IdentArrayElem(id: String, l: List[Expr])(val pos: (Int, Int)) extends ExprAndLValue 

  object IdentArrayElem extends ParserBridgePos2[String, Option[List[Expr]], ExprAndLValue] {
    def apply(id: String, l: Option[List[Expr]])(pos: (Int, Int)): ExprAndLValue =
      l match {
        case Some(l) => ArrayElem(Ident(id)(pos), l)(pos)
        case None => Ident(id)(pos)
      }
  }

  case class IntLiter(i: Int)(val pos: (Int, Int)) extends Expr 
  object IntLiter extends ParserBridgePos1[Int,  IntLiter]
  case class Digit(d: Char) 
  object Digit extends ParserBridge1[Char, Digit]
 
  case class BoolLiter(b: Boolean)(val pos: (Int, Int)) extends Expr
  object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]
 
  case class CharLiter(c: Char)(val pos: (Int, Int)) extends Expr 
  object CharLiter extends ParserBridgePos1[Char, CharLiter]

  case class StrLiter(s: String)(val pos: (Int, Int)) extends Expr
  object StrLiter extends ParserBridgePos1[String, StrLiter]

  case class ArrayLiter(heapOrStack: HeapOrStack, es: Option[List[Expr]])(val pos: (Int, Int)) extends RValue 
  object ArrayLiter extends ParserBridgePos2[HeapOrStack, Option[List[Expr]], ArrayLiter]

  sealed trait HeapOrStack
  case object Heap extends HeapOrStack with ParserBridge0[HeapOrStack]
  case object Stack extends HeapOrStack with ParserBridge0[HeapOrStack]

  case object PairLiter extends Expr

  case class Not()(val pos: (Int, Int)) extends UnaryOp
  case class Neg()(val pos: (Int, Int)) extends UnaryOp 
  case class Len()(val pos: (Int, Int)) extends UnaryOp
  case class Ord()(val pos: (Int, Int)) extends UnaryOp
  case class Chr()(val pos: (Int, Int)) extends UnaryOp


  object Not extends ParserBridgePos1[Expr, UnOpExpr]{
    def apply(x: Expr)(pos: (Int, Int)): UnOpExpr = UnOpExpr(Not()(pos), x)
  }

  object Neg extends ParserBridgePos1[Expr, UnOpExpr]{
    def apply(x: Expr)(pos: (Int, Int)): UnOpExpr = UnOpExpr(Neg()(pos), x)
  }

  object Len extends ParserBridgePos1[Expr, UnOpExpr]{
    def apply(x: Expr)(pos: (Int, Int)): UnOpExpr = UnOpExpr(Len()(pos), x)
  }

  object Ord extends ParserBridgePos1[Expr, UnOpExpr]{
    def apply(x: Expr)(pos: (Int, Int)): UnOpExpr = UnOpExpr(Ord()(pos), x)
  }

  object Chr extends ParserBridgePos1[Expr, UnOpExpr]{
    def apply(x: Expr)(pos: (Int, Int)): UnOpExpr = UnOpExpr(Chr()(pos), x)
  }

  case class Mul()(val pos: (Int, Int)) extends BinaryOp 
  case class Div()(val pos: (Int, Int)) extends BinaryOp 
  case class Mod()(val pos: (Int, Int)) extends BinaryOp 
  case class Add()(val pos: (Int, Int)) extends BinaryOp
  case class Minus()(val pos: (Int, Int)) extends BinaryOp 
  case class GT()(val pos: (Int, Int)) extends BinaryOp 
  case class GTEq()(val pos: (Int, Int)) extends BinaryOp 
  case class LT()(val pos: (Int, Int)) extends BinaryOp 
  case class LTEq()(val pos: (Int, Int)) extends BinaryOp 
  case class Eq()(val pos: (Int, Int)) extends BinaryOp 
  case class Neq()(val pos: (Int, Int)) extends BinaryOp 
  case class And()(val pos: (Int, Int)) extends BinaryOp 
  case class Or()(val pos: (Int, Int)) extends BinaryOp 

  object Mul extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Mul()(pos), x, y)(pos)
  }

  object Div extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Div()(pos), x, y)(pos)
  }

  object Mod extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Mod()(pos), x, y)(pos)
  }
 
  object Add extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Add()(pos), x, y)(pos)
  }

  object Minus extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Minus()(pos), x, y)(pos)
  }
 
  object GT extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(GT()(pos), x, y)(pos)
  }
 
  object GTEq extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(GTEq()(pos), x, y)(pos)
  }

  object LT extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(LT()(pos), x, y)(pos)
  }

  object LTEq extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(LTEq()(pos), x, y)(pos)
  }

  object Eq extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Eq()(pos), x, y)(pos)
  }

  object Neq extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Neq()(pos), x, y)(pos)
  }

  object And extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(And()(pos), x, y)(pos)
  }

  object Or extends ParserBridgePos2[Expr, Expr, BinOpExpr] {
    def apply(x: Expr, y: Expr)(pos: (Int, Int)): BinOpExpr = BinOpExpr(Or()(pos), x, y)(pos)
  }
     
}

