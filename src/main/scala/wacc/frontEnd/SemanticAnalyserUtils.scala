package frontEnd

import semanticAst._
import ast._
import symbolTable._
import getSemType.{exprGetType}

object semanticAnalyserUtils {
    // Converts the types form the syntax ast to semantic types
    def typeToIdObj(t : Type) : SemType = {
      t match {
        case x : BaseType => baseTypeToIdObj(x)
        case ArrayType(t) => SemArray(typeToIdObj(t))
        case PairType(p1, p2) => SemPairType(pairElemTypeToIdObj(p1), pairElemTypeToIdObj(p2))
        case _ => sys.exit(200)
      }
    }
    
    def baseTypeToIdObj(t : BaseType) : SemBaseType = {
        t match {
            case IntType => SemInt
            case BoolType => SemBool
            case CharType => SemChar
            case StringType => SemString
        }
    }

    def pairElemTypeToIdObj(t: PairElemType) : SemType = {
      t match {
        case PairType(t1, t2) => new SemPairType(pairElemTypeToIdObj(t1), pairElemTypeToIdObj(t2)) 
        case Pair => SemPairUnknown()
        case ArrayType(t) => new SemArray(typeToIdObj(t))
        case x : BaseType => baseTypeToIdObj(x)
        case _ => sys.exit(200)
      }
    }

    def paramListToListOfSemTypes(ps : Option[ParamList]) : Option[List[SemType]] = {
        ps match {
            case None => None
            case Some(paramList) => Some(paramList.paramList.map(p => typeToIdObj(p.argType)))
        }
    }

    def argListToSemTypeList(argList : Option[ArgList], st : SymbolTable) : Option[List[SemType]] = {
        argList match {
            case None => None
            case Some(ArgList(es)) => Some(es.map(x => exprGetType(x,st)))
        }
    }
}
