package frontEnd

import symbolTable._

object semanticAst {

    // SemNode can be a Semantic Variable, Semantic Function or a Semantic Program
    abstract class SemNode() {
        def getType() : SemType
    }
    
    sealed trait SemType

    trait SemBaseType extends SemType {}

    case object SemInt extends SemBaseType {}
    case object SemString extends SemBaseType {}
    case object SemChar extends SemBaseType {}
    case object SemBool extends SemBaseType {}

    // SemUnkown represents a null reference
    case class SemUnknown() extends SemType {
        var stackFlag: Boolean = false
        def setStackFlag() = {stackFlag = true}

        def getStackFlag() = stackFlag
    }
    case class SemPairUnknown() extends SemType {
        var stackFlag: Boolean = false
        def setStackFlag() = {stackFlag = true}

        def getStackFlag() = stackFlag
    }

    // SemAny is equal to all the types
    case class SemArray(elementType : SemType) extends SemType {
        var size: Int = 0
        var stackFlag: Boolean = false
        def setSize(newSize: Int) = {size = newSize}
        
        def getSize() = size

        def setStackFlag() = {stackFlag = true}

        def getStackFlag() = stackFlag
    }

    case class SemPairType(p1 : SemType, p2 : SemType) extends SemType{
        var stackFlag: Boolean = false
        def setStackFlag() = {stackFlag = true}

        def getStackFlag() = stackFlag
    }

    case object SemAny extends SemType {}

    case class SemVariable(t : SemType) extends SemNode {
        var stackOffset: Int = 0
        def setOffset(address: Int) = {
            stackOffset = address
        }
        def getOffset(): Int = stackOffset
        def getType() : SemType = t
    }

    case class SemFunction(funcReturnType : SemType, symtable : SymbolTable, 
                           formals : Option[List[SemVariable]], labelName : String) extends SemNode {
        def getType() : SemType = funcReturnType
    }

    class SemProgram(symbolTable : SymbolTable) extends SemNode {
        def getType() : SemType = SemUnknown()
    }

    // Used to compare the types
    def areEqualTypes(type1 : SemType, type2 : SemType) : Boolean = {
        type1 match {
            case SemAny => true
            case SemPairType(p1, p2) => {
                type2 match {
                    case SemPairType(p3,p4) => { areEqualTypes(p1,p3) && areEqualTypes(p2,p4)} 
                    case SemAny => true
                    case SemUnknown() => true
                    case SemPairUnknown() => true
                    case _ => false
                }
            }
            case SemArray(t1) => {
                type2 match {
                    case SemArray(t2) => { areEqualTypes(t1,t2) }
                    case SemAny => true
                    // case _ => areEqualTypes(t1, type2)
                    case _ => false
                }
            }
            case SemPairUnknown() => {
                type2 match {
                    case SemPairUnknown() => true
                    case SemUnknown() => true
                    case SemPairType(p1, p2) => true //!!
                    case SemAny => true 
                    case _ => false
                }
            }
            case SemUnknown() => {
                type2 match {
                    
                    case _ => true
                }
            }
            case _ => {
                type2 match {
                    case SemAny => true
                    //case SemPairType(_,_) => false
                    // case SemArray(t2) => areEqualTypes(type1,t2)
                    case SemUnknown() => true
                    case _ => type1 == type2
                }
            }   
        }
    }
}