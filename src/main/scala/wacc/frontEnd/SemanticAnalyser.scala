package frontEnd

import predefFuncs._
import symbolTable._
import semanticAst._
import ast._
import semanticAnalyserUtils._
import semanticError._
import scala.util.control.Breaks.{break, breakable}


import getSemType._
import scala.collection.mutable.ListBuffer
import wacc.backEnd.utils.{getUniqueScopeLabel}

object semanticAnalyser {
    val topSymTable : SymbolTable = SymbolTable(None)
    
    def programCheck(program : WACCProgram) : (SemNode, ListBuffer[ErrorMsg]) = {



        program.imports.foreach(i => importCheck(i))


        /* Add all the function definitions on the program symbol table
           As they can be accessed before they are reached in the code */
        program.functions.foreach(function => addFuncSymTable(function, topSymTable))
   
        // Call the check method in all the functions
        program.functions.foreach(function => functionCheck(function, topSymTable))

        // Call the check method in all the statements of the program
        program.body.foreach(stat => statementCheck(stat, topSymTable, None))

        // Return a Semantic Program Node that carries all the neccessary information 
        val semWaccProgram : SemProgram = new SemProgram(topSymTable)
        program.addScope(topSymTable)
        return (semWaccProgram, errorList)
    }

    def importCheck(i: Import): Unit = {
        // We check if the user has attempted to access a library they have installed (ie, is in waccpackages)
        if (!loadLibrary(i)) {
            // Library does not exist. Throw semantic error
            val err = CustomError(i.lib.name + "does not exist")
            addToErrorList(err)  
    }
}




    def functionCheck(func : Function, st : SymbolTable) : Unit = {

        val (ty, id) = func.typeAndIdent
        val funcO = st.lookupAll(id, EnumFunction(typeToIdObj(ty),paramListToListOfSemTypes(func.paramList)))
        
        funcO match {
            case Some(funcObj: SemFunction) => {
                val paramSt : SymbolTable = SymbolTable(Some(funcObj.symtable))
                func.paramList match {
                    case None =>
                    case Some(ParamList(params)) => {
                        for (p <- params) {
                            val V = funcObj.symtable.lookup(p.argIdent, EnumVariable)
                            V match {
                                case None => {
                                    funcObj.symtable.add(p.argIdent, EnumVariable, SemVariable(typeToIdObj(p.argType)))
                                }
                                case Some(_) => {
                                    val err = CustomError("Parameter is already defined")//Param already defined
                                    addToErrorList(err)

                            }
                            }
                        }
                    }
                }

                // Checks all the statements in the function
                func.body.foreach(stat => {statementCheck(stat, paramSt, Some(funcObj.funcReturnType))})
                
                val lastStatReturnType = statGetReturnType(func.body.last, paramSt)
                /* The function's return function in the declaration should equal
                   the return statement's type */
                if(!areEqualTypes(funcObj.funcReturnType, lastStatReturnType)){
                    {
                        val err = CustomError("Function's return statement does not match function's return type")
                        addToErrorList(err)
                    }
                } 
            }
            // The function has not been declared
            case _ => {                        
                val err = CustomError("Function has not been declared")
                addToErrorList(err)
            }
        }
    }
    
    // Adds the function definition to the symbol table
    def addFuncSymTable(func : Function, st : SymbolTable) : SemFunction = {

        var (funcType, funcIdent) = func.typeAndIdent
        /*stores the EnumFunction uniquely distinguishing the function with its return type and parameter return types for overloading.*/
        val enumFunc = EnumFunction(typeToIdObj(funcType), paramListToListOfSemTypes(func.paramList))
        val fSem : Option[SemNode] = st.lookupAll(funcIdent, enumFunc)

        val funcSt : SymbolTable = SymbolTable(Some(st))
        val formals : Option[List[SemVariable]] = 
            func.paramList match {
            // Call check method on all the params
            case Some(ParamList(params)) => {
                Some(params.map(p => paramCheck(p, funcSt)))
                }
            case Some(_) => None
            
            // The function does not have parameters
            case None => None
        }
        fSem match {
            case Some(_) => {
                val err = FunctionRedefinitionError(funcIdent, func.pos)
                addToErrorList(err)
                return null
            }   
            case None => {
                val labelName = getUniqueScopeLabel()
                val funcObj : SemFunction = new SemFunction(typeToIdObj(funcType), funcSt, formals, labelName)
                st.add(funcIdent, enumFunc, funcObj)
                func.addScope(funcSt)
                return funcObj
            }
        }   
    }

    // Check the param and add them to the functions symbol table
    def paramCheck(param : Param, st : SymbolTable) : SemVariable = {
        val semParam : Option[SemNode] = st.lookup(param.argIdent, EnumVariable)
        semParam match {
            case Some(_) => {
                val err = ScopeError(param.argIdent, param.pos)
                addToErrorList(err)
                return null
            }
            case None => {
                val pObj = SemVariable(typeToIdObj(param.argType)) 
                param.addScope(st)
                return pObj
            }
        } 
    }

    // Check all the statement types
    def statementCheck(stat : Stat, st : SymbolTable, retType : Option[SemType]) : Unit = {
        stat.addScope(st)
        stat match {
            case Skip => 
            case Declare(ty, name, rval) => declareCheck(ty, name, rval, st)
            case a@Assign(lval, rval) => assignCheck(lval, rval, st, a.pos)
            case r:ReadStat => {
                r match{
                    case ReadStat(lVal) => {
                        val ty = getLValType(lVal, st)
                        if(!(areEqualTypes(ty, SemInt) || areEqualTypes(ty, SemChar))){
                            val err = TypeErrorMessage(ty, "char or int", r.pos)
                            addToErrorList(err)
                        }
                    }
                }
            }

            case f:FreeStat => {
                f match {
                    case FreeStat(expr) => {                
                        exprCheck(expr, st)
                        val exprType : SemType = exprGetType(expr, st)
                        // Can only free a reference to a pair or array
                        if(!(areEqualTypes(exprType, SemArray(SemAny)) || areEqualTypes(exprType, SemPairType(SemAny,SemAny)))){
                            val err = TypeErrorMessage(exprType,"at least 1-dimensional array or pair", f.pos)
                            addToErrorList(err)
                        }
                        exprType match {
                            case arr: SemArray => {
                                if(arr.getStackFlag()) {
                                    val err = CustomError("Cannot free the array, as it is stored in the stack.")
                                    addToErrorList(err)
                                }
                            }
                            case unPair: SemPairUnknown => {
                                if(unPair.getStackFlag()) {
                                    val err = CustomError("Cannot free the pair, as it is stored in the stack.")
                                    addToErrorList(err)
                                }
                            }
                            case p: SemPairType => {
                                if(p.getStackFlag()) {
                                    val err = CustomError("Cannot free the pair, as it is stored in the stack.")
                                    addToErrorList(err)
                                }
                            }
                            case _ =>
                        }
                    }
                }
            }
            case r: ReturnStat => {
                
                r match {
                    case ReturnStat(expr) => {
                        exprCheck(expr, st)
                        retType match {
                            case None => {
                                val err = ReturnError(r.pos)
                                addToErrorList(err)
                            }
                            case Some(t) => {
                                //Error - return type is not equal to expected
                                if (!areEqualTypes(exprGetType(expr, st), t)) {
                                    val err = TypeError(exprGetType(expr, st), t, r.pos)
                                    addToErrorList(err)
                                }
                            }
                        }
                    }
                }
            }
            // the expression you are using to exit should eval to an int
            case e:ExitStat => {
                e match {
                    case ExitStat(expr) => {
                        exprCheck(expr, st)
                        if(!areEqualTypes(exprGetType(expr, st), SemInt)){
                            val err = TypeError(exprGetType(expr, st), SemInt, e.pos)
                            addToErrorList(err)
                     }
                    }
                }

            }

            case PrintStat(expr) => exprCheck(expr, st)
            case PrintlnStat(expr) => exprCheck(expr, st)
            case f:IfStat => {
                f match {
                    case IfStat(expr, ifStats, elStats) => {
                        exprCheck(expr, st)
                        val exprType = exprGetType(expr,st)
                        if (!areEqualTypes(exprType, SemBool)) {
                            val err = TypeError(exprType, SemBool, f.pos)
                            addToErrorList(err)
                        }
                        //Create new scopes for if and else
                        val ifSymbolTable : SymbolTable = SymbolTable(Some(st))
                        val elSymbolTable : SymbolTable = SymbolTable(Some(st))
                        ifStats.foreach(ifStat => {
                            statementCheck(ifStat, ifSymbolTable, retType)
                            ifStat.addScope(ifSymbolTable)
                        })
                        elStats.foreach(elStat => {
                            statementCheck(elStat, elSymbolTable, retType)
                            elStat.addScope(elSymbolTable)
                        }) 
                    }
                } 
            }
            case w:WhileStat => {
                w match {
                    case WhileStat(expr, stats) => {
                        exprCheck(expr, st)
                        val exprType = exprGetType(expr,st)
                        if (!areEqualTypes(exprType, SemBool)) {
                            val err = TypeError(exprType, SemBool, w.pos)
                            addToErrorList(err)
                        }         
                        //Create new scope for while      
                        val whileSymbolTable : SymbolTable = SymbolTable(Some(st))
                        stats.foreach(stat => {
                            statementCheck(stat, whileSymbolTable, retType)
                            stat.addScope(whileSymbolTable)
                        })
                    }
                }
            }
            case BeginStat(stats) => {
                val beginSymbolTable : SymbolTable = SymbolTable(Some(st))
                //Create new scope for begin statements
                stats.foreach(stat => {
                    statementCheck(stat, beginSymbolTable, retType)
                    stat.addScope(beginSymbolTable)
                })
            }
            case _ =>
        }
    }

    /* When declaring we check that the variable is not defined within the scope 
       and that the types match*/
    def declareCheck(t : Type, i : Ident, r : RValue, st : SymbolTable) = {
        val V = st.lookup(i, EnumVariable)
        val rValType : SemType = valRValue(Some(typeToIdObj(t)),r, st)
        V match {
            case Some(_) => {
                val err = CustomError("Error - variable is already declared")
                addToErrorList(err)
            }
            case None => {
                if(!areEqualTypes(typeToIdObj(t), rValType)) {
                    val err = TypeError(rValType, typeToIdObj(t), i.pos)
                    addToErrorList(err)
                }
                var varObj: SemNode = new SemVariable(SemUnknown())

                rValType match {
                    case SemUnknown() => varObj = new SemVariable(typeToIdObj(t))
                    case SemAny => varObj = new SemVariable(typeToIdObj(t))
                    case semP@SemArray(SemAny) => varObj = {
                        val p = typeToIdObj(t)
                        p match {
                            case newSemP:SemArray => {
                                if (semP.getStackFlag()) {
                                    newSemP.setStackFlag()
                                }
                            }
                            case _ =>
                        }
                        new SemVariable(p)
                    }
                    case semP@SemPairType(SemUnknown(), _) => varObj = {
                        val p = typeToIdObj(t)
                        p match {
                            case newSemP:SemPairType => {
                                if (semP.getStackFlag()) {
                                    newSemP.setStackFlag()
                                }
                            }
                            case _ =>
                        }
                        new SemVariable(p)
                    }
                    case semP@SemPairType(_, SemUnknown()) => varObj = {
                        val p = typeToIdObj(t)
                        p match {
                            case newSemP:SemPairType => {
                                if (semP.getStackFlag()) {
                                    newSemP.setStackFlag()
                                }
                            }
                            case _ =>
                        }
                        new SemVariable(p)
                    }

                    case semP@SemPairType(semUn1: SemPairType, semUn2: SemPairType) => varObj = {
                        val p = typeToIdObj(t)
                        p match {
                            case newSemP@SemPairType(un1: SemPairUnknown, _) => {
                                if (semP.getStackFlag()) {
                                    newSemP.setStackFlag()
                                }
                                if (semUn1.getStackFlag()) {
                                    un1.setStackFlag()
                                }
                                if (semUn2.getStackFlag()) {
                                    un1.setStackFlag()
                                }
                            }
                            case _ =>
                        }
                        new SemVariable(p)
                    }

                    case semP@SemPairType(semUn: SemPairType, _) => varObj = {
                        val p = typeToIdObj(t)
                        p match {
                            case newSemP@SemPairType(un1: SemPairUnknown, _) => {
                                if (semP.getStackFlag()) {
                                    newSemP.setStackFlag()
                                }
                                if (semUn.getStackFlag()) {
                                    un1.setStackFlag()
                                }
                            }
                            case _ =>
                        }
                        new SemVariable(p)
                    }
                    
                    case semP@SemPairType(_, semUn: SemPairType) => varObj = {
                        val p = typeToIdObj(t)
                        p match {
                            case newSemP@SemPairType(_, un2: SemPairUnknown) => {
                                if (semP.getStackFlag()) {
                                    newSemP.setStackFlag()
                                }
                                if (semUn.getStackFlag()) {
                                    un2.setStackFlag()
                                }
                            }
                            case _ =>
                        }
                        new SemVariable(p)
                    }
                    
                    
                    case _=> varObj = new SemVariable(rValType)
                }
      
                
                st.add(i, EnumVariable, varObj)
                i.addScope(st)
            }
        }
    }

    //Get the ident of an lvalue
    def getlValId(l : LValue, st : SymbolTable) : Ident = {
        l.addScope(st)
        l match {
            case id: Ident => {
                id match {
                    case Ident(name) => {
                        val V = st.lookupAll(id, EnumVariable)
                        V match {
                            case None => {
                                val err = ScopeError(id, id.pos)
                                addToErrorList(err)
                                return null
                            } 
                            case Some(_) => {
                                id.addScope(st)
                                id
                            } 
                        }                        
                    }
                }
            }

            case a: ArrayElem => {
                a match {
                    case ArrayElem(i, es) => {
                        es.foreach(e => exprCheck(e, st))
                        val V = st.lookupAll(i, EnumVariable)
                        V match {
                            case None => {
                                val err = ScopeError(i, a.pos)
                                addToErrorList(err)
                                return null
                            } 
                            case Some(_) => i
                        }      
                    }
                } 
            }

            case f:Fst => {
                f.addScope(st)
                f match {
                    case Fst(lVal) => {
                        val idName = getlValId(lVal, st)
                        val pairVal = st.lookupAll(idName, EnumVariable)
                        pairVal match {
                            case None => {
                                val err = CustomError("Cannot assign to a pair that does not exit")
                                addToErrorList(err)
                                return null
                            } 
                            case Some(SemVariable(SemPairType(_, _))) => idName
                            case Some(SemVariable(SemArray(SemPairType(_,_)))) => idName
                            case _ => {
                                val err = TypeErrorMessage(SemAny, "Can only call first on a pair", f.pos)
                                addToErrorList(err)
                                return null
                            } 
                        }   
                    }
                }
            }
            
            case s:Snd => {
                s.addScope(st)
                s match {
                    case Snd(lVal) => {
                        val idName = getlValId(lVal, st)
                        val pairVal = st.lookupAll(idName, EnumVariable)
                        pairVal match {
                            case None => {
                                val err = CustomError("Cannot assign to a pair that does not exit")
                                addToErrorList(err)
                                return null
                            } 
                            case Some(SemVariable(SemPairType(_, _))) => idName
                            case _ => {
                                val err = TypeErrorMessage(SemAny, "Can only call first on a pair", s.pos)
                                addToErrorList(err)
                                return null
                            } 
                        }
                    }
                }
            }
            case _ => sys.exit(200)
        }
    }

    // We check the types are the same
    def assignCheck(l : LValue, r : RValue, st : SymbolTable, pos: (Int, Int)) = {
        val idName : Ident = getlValId(l, st)
        val V = st.lookupAll(idName, EnumVariable)
        V match {
            case None => {
                val err = CustomError("Cannot assign to an undefined variable")
                addToErrorList(err)
            }
            case Some(_) => {
                println("LValType: " + getLValType(l, st))
                println("RValType: " + valRValue(Some(getLValType(l, st)), r, st))
                if(!areEqualTypes(getLValType(l, st), valRValue(Some(getLValType(l, st)), r, st)) || (getLValType(l, st) == SemUnknown() && valRValue(Some(getLValType(l, st)), r,st) == SemUnknown())){
                    
                    if (getLValType(l, st) != SemUnknown()){
                        val err = TypeError(valRValue(Some(getLValType(l, st)), r, st), getLValType(l, st), pos)
                        addToErrorList(err)
                    }else {
                        val err = CustomError("Left and right side of assignment are unknown")
                        addToErrorList(err)
                    }
                }
                val lValType: SemType = getLValType(l, st)
                val rValType: SemType = valRValue(Some(lValType), r, st)
                lValType match{
                    case a1: SemArray => {
                            rValType match {
                                case a2: SemArray => {
                                    if(a1.getStackFlag()){
                                        if(a1.getSize() != a2.getSize()){
                                        val err = CustomError("You can not change the size of the array if you declare it in the stack")
                                        addToErrorList(err)
                                    }
                                }

                                if(a1.getStackFlag() != a2.getStackFlag()){
                                    val err = CustomError("You can not change the position of the array, from heap to stack or viceversa")
                                    addToErrorList(err)
                                }
                                }
                                case _ => {
                                    val err = TypeError(rValType, lValType, pos)
                                    addToErrorList(err)
                                }   
                            }
                        
                    }
                    case p1: SemPairType => {
                        rValType match {
                            case p2: SemPairType => {
                                if(p1.getStackFlag() != p2.getStackFlag()){
                                    val err = CustomError("You can not change the position of the pairs, from heap to stack or viceversa")
                                    addToErrorList(err)
                                }
                            }
                            case _=> 
                        }
                    }
                    case _ =>
                }
            }      
        }
    }

    def funcCallCheck(lValReturnType : SemType, i : Ident, as : Option[ArgList], st : SymbolTable) : SemType = {
        val fSem : Option[SemNode] = st.lookupAll(i, EnumFunction(lValReturnType, argListToSemTypeList(as,st)))
        i.addScope(st)
        fSem match {
            case None => {
                val predefFunc = addPredefFunc(i, lValReturnType, argListToSemTypeList(as,st))
                predefFunc match {
                    case None => {
                        val err = FunctionError(i, i.pos)
                        addToErrorList(err)
                        return null
                    }
                    case Some(func) => {
                            addFuncSymTable(func, topSymTable)
                            functionCheck(func, st)
                            funcCallCheck(lValReturnType, i, as, st)
                    }
                }
            }
            case Some(x) => {
                x match {
                    case SemFunction(funcReturnType, symtable, formals, _) => {
                        formals match {
                        case None => {
                            as match {
                                case None => 
                                case Some(ArgList(xs)) => {
                                    if(xs.size != 0){
                                        //number of parameters does not match expected  
                                        val err = CustomError("Number of parameters does not match expected")
                                        addToErrorList(err)
                                    }
                                }
                                case _ => return null
                                    
                                 //should be caught by syntax
                            }
                        }
                        case Some(formalsX) => {
                            as match {
                                case None => {
                                    if(formalsX.size != 0){
                                        return null
                                    }
                                }  
                                case Some(a:ArgList) => {
                                    a match {
                                        case ArgList(asX) => {
                                            if (asX.size != formalsX.size) {
                                                val err = FunctionCallError(i, asX.size, formalsX.size, a.pos)
                                                addToErrorList(err)
                                                return null
                                            }
                                            val size = asX.size
                                            for (i <- 0 to (size - 1)) {
                                                exprCheck(asX(i),st)
                                                if (!areEqualTypes(exprGetType(asX(i), st), formalsX(i).getType())) {
                                                    val err = TypeError(exprGetType(asX(i), st), formalsX(i).getType(), a.pos)
                                                    addToErrorList(err)
                                                }
                                            }
                                        }
                                    }
                                }
                                case Some(_) => return null
                            }
                        }
                        }
                    }
                    case _ => return null
                
            }
            
            x.getType()
        }
        }
    }

    def exprCheck(expr : Expr, st: SymbolTable) : Unit  = {
        expr match {
            case id:Ident => { 
                id match {
                    case Ident(x) => {
                        val i = st.lookupAll(id, EnumVariable)
                        i match {
                            case None => {
                                val err = ScopeError(id, id.pos)
                                addToErrorList(err)
                            } 
                            case Some(x) => {
                                id.addScope(st)
                                x.getType()}
                        }
                    }
                }
            }

            case ArrayElem(i, es) => {
                val arr : Option[SemNode] = st.lookupAll(i, EnumVariable)
                arr match {
                    case None => {
                        val err = ScopeError(i, i.pos)
                        addToErrorList(err)
                    }
                    case Some(SemVariable(SemArray(x))) => {
                        i.addScope(st)
                        for (e <- es) {
                            exprCheck(e, st)
                        }
                    }
                    case _ => 
                }
            }
            case UnOpExpr(op, e) => {

                exprCheck(e,st)
                val eType = exprGetType(e, st)
                op match {
                    case n:Not => {
                        if (!areEqualTypes(eType, SemBool)) {
                            val err = TypeError(eType, SemBool, n.pos)
                            addToErrorList(err)
                        }
                    }

                    case n:Neg => {
                        if (!areEqualTypes(eType, SemInt)) {
                            val err = TypeError(eType, SemInt, n.pos)
                            addToErrorList(err)
                        }
                    }

                    case l:Len => {
                        if (!areEqualTypes(eType, SemArray(SemAny))) {
                            val err = TypeError(eType, SemArray(SemAny), l.pos)
                            addToErrorList(err)
                        }
                    }

                    case o:Ord => {
                        if (!areEqualTypes(eType, SemChar)) {
                            val err = TypeError(eType, SemChar, o.pos)
                            addToErrorList(err)
                        }
                    }

                    case c:Chr => {
                        if (!areEqualTypes(eType, SemInt)) {
                            val err = TypeError(eType, SemChar, c.pos)
                            addToErrorList(err)
                        }  
                    }

                    case _ =>
                }
            }

            case b:BinOpExpr => {
                b match {
                    case BinOpExpr(op, e1, e2) => {
                        exprCheck(e1,st)
                        exprCheck(e2,st)
                        val e1Type = exprGetType(e1, st)
                        val e2Type = exprGetType(e2, st)
                        val returnType = exprGetType(expr, st)

                        if(!areEqualTypes(e1Type, e2Type)) {
                            val err = TypeError(e2Type, e1Type, b.pos)
                            addToErrorList(err)
                        }

                    op match {
                        case m:Mul => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt), SemInt, m.pos) }
                        case d:Div => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt), SemInt, d.pos) }
                        case m:Mod => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt), SemInt, m.pos) }
                        case a:Add => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt), SemInt, a.pos) }
                        case m:Minus => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt), SemInt, m.pos) }
                        case g:GT => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt, SemChar), SemBool, g.pos) }
                        case g:GTEq => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt, SemChar), SemBool, g.pos) }
                        case l:LT => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt, SemChar), SemBool, l.pos) }
                        case l:LTEq => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemInt, SemChar), SemBool, l.pos) }
                        case e:Eq => {compareArgumentAndReturnTypes(e1Type, returnType, List(SemAny), SemBool, e.pos)}
                        case n:Neq => {compareArgumentAndReturnTypes(e1Type, returnType, List(SemAny), SemBool, n.pos)}
                        case a:And => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemBool), SemBool, a.pos) }
                        case o:Or => { compareArgumentAndReturnTypes(e1Type, returnType, List(SemBool), SemBool, o.pos) }
                        case _ =>
                    }
                    }
                }
            }
            case BracketedExpr(e: Expr) => exprCheck(e,st)
            case _ =>
        }
    }

    // Check that the arguments and the return types are the same
    def compareArgumentAndReturnTypes(argType : SemType, returnType : SemType, expectedArgTypes : List[SemType], expectedReturnType : SemType, pos:(Int, Int)) : Unit = {
        if (!areEqualTypes(returnType,expectedReturnType)) {
            val err = TypeError(returnType, expectedReturnType, pos)
            addToErrorList(err)
        }

        var safe = false
        breakable{ for (e <- expectedArgTypes) {
            if (areEqualTypes(e, argType)) {
                safe = true
                break()
            }
        }}
        if (!safe) {
            val err = TypeError(argType, expectedArgTypes(0), pos)
            addToErrorList(err)
        }

    }
    
}