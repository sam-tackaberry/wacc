package frontEnd

import ast._
import semanticAst._
import scala.collection.mutable.LinkedHashMap

object symbolTable {
  sealed trait EnumType
  case object EnumVariable extends EnumType
  case class EnumFunction(funcReturnType : SemType, params : Option[List[SemType]]) extends EnumType

  case class SymbolTable(encSt: Option[SymbolTable]) {
  
    //Key as an Ident and an enumtype as you can have 2 different types with the same name
    val dict = LinkedHashMap[(Ident, EnumType), SemNode]() 

    def add(name: Ident, t : EnumType, obj: SemNode): Unit = dict((name,t)) = obj

    def size() : Int = dict.size
    
    def remove(name : Ident, t : EnumType) : Unit = dict.remove(name,t)
    
    def lookup(name: Ident, t : EnumType): Option[SemNode] = dict.get((name,t))

    def lookupAll(name : Ident, t : EnumType) : Option[SemNode] = {
      var optS : Option[SymbolTable] = Option(this)

      while (optS.isDefined) {
        optS match {
          case Some(s) => {
            val obj = s.lookup(name, t)
            obj match {
              case Some(x) => return obj
              case None =>
            }
            // If not found you will want to look at the parent symbol table
            optS = s.encSt
          }
          case None => 
        }
      }
      return None
    }

    //This function is only used to look up variables. As such, the cases of EnumFunction called would be an error.
    def lookupAllDefined(name : Ident, t : EnumType) : Option[SemNode] = {
      var optS : Option[SymbolTable] = Option(this)

      t match {
        case _ : EnumFunction => {
          println("lookupAllDefined is called on a SemFunction. It can only be called on a SemVariable")
          sys.exit(200)
        }
        case EnumVariable => 
      }

      while (optS.isDefined) {
        optS match {
          case Some(s) => {
            val obj = s.lookup(name, t)
            obj match {
              case Some(sv : SemVariable) => {
                if (sv.getOffset() != 0) {
                  return obj
                }
              }
              case Some(_) =>
              case None =>
            }
            // If not found you will want to look at the parent symbol table
            optS = s.encSt
          }
          case None => 
        }
      }
      return None
    }
  }
}

