package frontEnd

import symbolTable._
import semanticAst._
import ast._
import semanticError._

import semanticAnalyser._

object getSemType {

    // Returns the type of the return statement (only if it is a valid return)
    def statGetReturnType(stat : Stat, st : SymbolTable) : SemType = {
        stat.addScope(st)
        stat match {
            case ReturnStat(expr) => exprGetType(expr, st)
            case i@IfStat(expr, ifStats, elseStats) => {
                val ifStType : SemType = statGetReturnType(ifStats.last, st)
                val elseStType : SemType = statGetReturnType(elseStats.last, st)
                //Check that the if and else return the same type
                if(!areEqualTypes(ifStType, elseStType)){
                    val err = TypeError(elseStType, ifStType, i.pos)
                    addToErrorList(err)
                }
                ifStType
            }
            case BeginStat(body) => statGetReturnType(body.last, st)
            //Not a valid return
            case _ => {
                val err = CustomError("Not a valid return statement")
                addToErrorList(err)
                return null
            }
        }
    }

    //Returns the type of the expresion
    def exprGetType(expr : Expr, st : SymbolTable) : SemType = {
        expr match {
            case IntLiter(_) => SemInt
            case BoolLiter(_) => SemBool
            case CharLiter(_) => SemChar
            case StrLiter(_) => SemString
            case PairLiter => SemUnknown()
            case i:Ident => {
                i match {
                    case Ident(name) => {
                        val identObj : Option[SemNode] = st.lookupAll(Ident(name)(0,0), EnumVariable) 
                       identObj match {
                            case None => {
                                val err = ScopeError(i, i.pos)
                                addToErrorList(err)
                                return null
                            }
                            case Some(ob) => {
                                i.addScope(st)
                                ob.getType()
                            }
                        }
                    }

                }
            }
            case a@ArrayElem(i, exprs) => {
                // Gets the Semantic Type of an Array Element
                def getArrayType(array: ArrayElem, st : SymbolTable) : SemType = {
                    val arrIdent : Option[SemNode] = st.lookupAll(i, EnumVariable)
                    arrIdent match {
                    case Some(x) => {
                        i.addScope(st)
                        x match {
                            
                            case SemVariable(a: SemArray) => a
                            //Error is not stored as an array
                            case e => {
                                val err = TypeErrorMessage(e.getType(), "Array",  a.pos)
                                addToErrorList(err)
                                return null
                            }
                        }
                    }
                    //Error array doesnt exist in this scope
                    case None => {

                        val err = ScopeError(i, i.pos)
                        addToErrorList(err)
                        return null
                    } 

                 }
                }
               
                // Gets the recursive type of the array in a multidimensional array
                def getElem(exprs: List[Expr], arrT: SemType, st: SymbolTable) : SemType = {
                    val expTy : SemType = exprGetType(exprs.head, st)
                    val head::tail = exprs
                    
                    if(expTy != SemInt){
                        //Cannot access array with a non int index
                        val err = TypeError(expTy, SemInt, a.pos)
                        addToErrorList(err)
                    }
                    arrT match {
                        case SemArray(sType) => {if (tail.isEmpty) {
                            sType
                        } else {
                            getElem(tail, sType, st)
                        }}
                        case ty => {
                            if (exprs.isEmpty) {
                                ty
                            } else {
                                val err = CustomError("Type Error \nAccessing array of wrong dimensions\n")
                                addToErrorList(err)
                                return null
                            }
                        }
                    }   
                }
                val arrType = getArrayType(a, st)
                getElem(exprs, arrType, st)
            }
                
            case UnOpExpr(op, e) => {
                op match {
                    case Not() => SemBool
                    case Neg() => SemInt
                    case Len() => SemInt
                    case Ord() => SemInt
                    case Chr() => SemChar
                }
            }

            case b:BinOpExpr => {
                b match {
                    case BinOpExpr(op, e1, e2) => {
                        val newE1 = exprGetType(e1, st)
                        val newE2 = exprGetType(e2,st)
                        if (!areEqualTypes(newE1, newE2)) {
                            val err = TypeError(newE2, newE1, b.pos)
                            addToErrorList(err)
                            return null
                        }
                        op match {
                            case Mul() => SemInt
                            case Div() => SemInt
                            case Mod() => SemInt
                            case Add() => SemInt
                            case Minus() => SemInt
                            case _ => SemBool
                        }
                    }
                }
            }
            case BracketedExpr(e: Expr) => exprGetType(e, st)
            case _ => sys.exit(200)
        }
    }

    // An LValue can only be an Ident, Array Elem or Pair Elem
    def getLValType(lval : LValue, st : SymbolTable) : SemType = {
        lval match {
            case i: Ident => {
                val lv = st.lookupAll(i, EnumVariable)
                lv match {
                    case Some(x) => {
                        i.addScope(st)
                        x.getType()
                    }
                    case None => {
                

                        val err = ScopeError(i, i.pos)
                        addToErrorList(err)
                        return null
                    }
                }
            }
            case arr: ArrayElem => exprGetType(arr, st)
            case x: PairElem => getLValPairElem(x, st) 
            case _ => sys.exit(200)
        }
    }

    // Get the type of a PairElem
    def getLValPairElem(p: PairElem, st : SymbolTable) : SemType = {
        p match {
            case f: Fst => {
                f.addScope(st)
                f match {
                case Fst(x) => getLValType(x, st) match {
                    case SemPairType(fst, _) => fst
                    case p: SemPairUnknown => {
                        val semUnk = SemUnknown()
                        if(p.getStackFlag()){
                            semUnk.setStackFlag()
                        }
                        semUnk
                    }
                    case x => {
                        val err = PairTypeError(f.pos)
                        addToErrorList(err)
                        return null
                    }
                }
            }
        }
            case s: Snd => {
                s.addScope(st)
                s match {  
                    case Snd(x) => getLValType(x, st) match {
                        case SemPairType(_, snd) =>{
                            snd
                        }
                        case p: SemPairUnknown => {
                        val semUnk = SemUnknown()
                        if(p.getStackFlag()){
                            semUnk.setStackFlag()
                        }
                        semUnk
                    }
                        case _ => {
                            sys.exit(200)
                        }
                    }
                }
            }
        }
    }

    // Get the type of the RValue
    def valRValue(returnType : Option[SemType], r : RValue, st : SymbolTable) : SemType = {
        r.addScope(st)
        r match {
            case x: Expr => {
                exprCheck(x, st)
                exprGetType(x, st)
            }
            case al:ArrayLiter => {
                al match {
                    case ArrayLiter(heapOrStack, es) => {
                        es match {
                            case None => SemArray(SemAny)
                                //SemArray(SemAny) used to be
                            case Some(xs) => {
                                val arrayElemType : SemType = valRValue(None, xs.head, st)
                                var sameType : Boolean = true
                                /* Check that all the elems of the array are the
                                   same as the type of the array declared */
                                xs.foreach(e => {
                                    exprCheck(e,st)
                                    sameType = areEqualTypes(arrayElemType, valRValue(None, e, st))
                                    if(!sameType){
                                        val err = TypeError(exprGetType(e, st), arrayElemType, al.pos)
                                        addToErrorList(err)
                                        sys.exit(200)
                                    }
                                }
                            )
                            
                           
                            val retArr: SemArray = SemArray(arrayElemType)
                            retArr.setSize(xs.size)
                            heapOrStack match {
                                case Stack => {
                                    retArr.setStackFlag()
                                }
                                case _ => 
                            }
                            retArr
                            }                    
                        }
                    }
                }
            }
            case  np@ NewPair(heapOrStack,e1,e2) => {
                //If Pair inside of pair change the type to SemPairUnkown
                def changePairType(ty : SemType) : SemType = {
                    ty match {
                        case s@SemPairType(_,_) => s
                        case x => x
                    }
                } 
                exprCheck(e1, st)
                exprCheck(e2, st)
                val e1Type = exprGetType(e1, st)
                val e2Type = exprGetType(e2, st)

                
                val retPair: SemPairType = SemPairType(changePairType(e1Type), changePairType(e2Type))

                heapOrStack match {
                    case Stack => retPair.setStackFlag()
                    case _ =>
                }
                retPair
            }

            case p : PairElem => getLValPairElem(p, st)
            
            case Call(i, as) => {
                val Some(rT) = returnType
                funcCallCheck(rT, i, as, st)
            }
        }
    }
}