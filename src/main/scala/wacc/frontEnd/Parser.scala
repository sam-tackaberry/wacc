package frontEnd

import parsley.Parsley
import parsley.Parsley.{attempt, pure}
import parsley.expr.{chain, precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.combinator.{sepBy, sepBy1,  many, some, option,  choice}
import parsley.errors.combinator._
import ast._
import lexer._
import implicits.implicitSymbol

object parser {
    lazy val `<parse-program>` = fully(`<program>`)

    def hasValidReturn(stats: List[Stat]) : Boolean = {
        for(i <- 0 to (stats.size-2)){
            stats(i) match {
                case ReturnStat(_) => false // return statement before the end of the function
                case _ =>
            }
        }
        stats.last match{
            case ReturnStat(_) | ExitStat(_) => true
            case IfStat(_, ifStat, elseStat) => (hasValidReturn(ifStat) && hasValidReturn(elseStat))
            case WhileStat(BoolLiter(true), stats) => (hasValidReturn(stats))
            case BeginStat(body) => hasValidReturn(body)
            case _ => false
        }
    }

    lazy val `<import>` = Import("import" *> Library(IDENT_ID))

    lazy val `<program>` = WACCProgram(many(`<import>`), "begin" *> many(`<func>`), `<stats>` <* "end").label("begin").explain("Programs must start with begin")

    lazy val `<func>` = amend(_functionNoType <|> Function(attempt(`<type>` <~> `<ident>` <* "(".label("open parenthesis")),
        entrench(option(`<param-list>`))  <* ")".label("close parenthesis"), "is" *> (entrench(`<stats>`).guardAgainst {
        case body if !hasValidReturn(body) => Seq("Function must have a return statement on all exit paths")
    })<* "end")).label("function declaration")    
   
    lazy val `<param-list>` = ParamList(sepBy1(`<param>`,",")).label("parameter list")  
    lazy val `<param>` = _paramNoType <|>Param(`<type>`, `<ident>`).label("parameter").explain("a comma must be followed by a parameter in a function declaration")

    lazy val `<stats>`= sepBy1(`<stat>`, ";")

    lazy val `<stat>` : Parsley[Stat] = (_functionWrongScope
                            <|> "skip".label("Skip") #> Skip 
                            <|> Declare(`<type>`, `<ident>` <* "=".label("declaration"), `<rvalue>`)
                            <|> Assign(`<lvalue>` <* "=".label("assignment"), `<rvalue>`)
                            <|> ReadStat("read" *> `<lvalue>`).label("read")
                            <|> FreeStat("free" *> `<expr>`).label("free")
                            <|> ReturnStat("return" *> `<expr>`).label("return")
                            <|> ExitStat("exit" *> `<expr>`).label("exit")
                            <|> PrintStat("print" *> `<expr>`).label("print")
                            <|> PrintlnStat("println" *> `<expr>`).label("println")
                            <|> IfStat("if" *> `<expr>`,
                                            "then".label("then").explain("if statements must have a then")
                                            *> `<stats>`,
                                            "else".label("else").explain("if statements must have a else clause")
                                            *> `<stats>` <*
                                            "fi".label("fi").explain("Unclosed if statement")).label("if")
                            <|> WhileStat("while" *> `<expr>`,
                                            "do".label("do").explain("while loops must have a do")
                                            *> `<stats>` <*
                                            "done".label("done").explain("Unclosed while loop")).label("while")
                            <|> BeginStat("begin" *>`<stats>`<*
                                            "end".label("end")).label("begin"))

    lazy val `<ident-array-elem>` = IdentArrayElem(IDENT_ID.label("identifier"), option("[".label("index (like `xs[idx]`)") *> sepBy(`<expr>`, "][") <* "]"))

    lazy val `<lvalue>` : Parsley[LValue] = (`<ident-array-elem>` <|> `<pair-elem>`)
   
    lazy val `<pair-elem>` = (Fst("fst".label("fst") *> `<lvalue>`) <|> Snd("snd".label("snd") *> `<lvalue>`))
   

    lazy val `<rvalue>` : Parsley[RValue] = (attempt(NewPair(`<heap-stack>`, "newpair" *> "(".label("open parenthesis") *> `<expr>` <* ",", `<expr>` <* ")".label("close parenthesis"))).label("new pair")
                                        <|> _noFuncWithoutCallCheck
                                        <|> `<array-liter>`
                                        <|> `<expr>`
                                        <|> `<pair-elem>`
                                        <|> Call("call" *> `<ident>` <* "(".label("open parenthesis"),  option(`<arg-list>`) <* ")".label("close parenthesis")).label("call"))

    lazy val `<arg-list>` = ArgList(sepBy1(`<expr>`, ",")).label("argument list")

    lazy val `<type>` : Parsley[Type] = chain.postfix(( `<base-type>` <|> `<pair-type>`), ArrayType <# "[]".label("square brackets")).label("type")

    lazy val `<base-type>` : Parsley[BaseType] = ("int" #> IntType 
                                        <|> "bool" #> BoolType
                                        <|> "char" #> CharType
                                        <|> "string" #> StringType).label("base type")

    lazy val `<array-type>` = StaticDynamicArrayType(`<type>`,"[" *> option(INTEGER) <* "]")

    lazy val `<pair-type>` : Parsley[PairType] = PairType("pair" *> "(".label("open parenthesis") *> `<pair-elem-type>`, "," *> `<pair-elem-type>` <* ")".label("closing parenthesis")).label("pair type")

    lazy val `<pair-elem-type>` =  chain.postfix(`<base-type>` , ArrayType <# "[]".label("square brackets")) <|> attempt(`<pair-type>`) <|> "pair" #> Pair

    lazy val `<expr>` : Parsley[Expr] = (`<oper>` <|> `<atom>`)


    lazy val `<atom>` = ("(".label("open parenthesis") *> `<expr>` <* ")".label("close parenthesis") <|> `<ident-array-elem>` <|> `<bool-liter>` <|> `<char-liter>` <|>  `<pair-liter>` <|> `<str-liter>`  <|> `<int-liter>`)

    lazy val `<oper>` =
        precedence[Expr](`<atom>`)(
                    Ops(Prefix)(choice(Not <# "!", Neg <# UNIOPMIN, Len <# "len", Ord <# "ord",Chr <# "chr")
                                .label("unary operator")),
                    Ops(InfixL)(choice(Mul <# "*", Div <# "/", Mod <# "%")
                                .label("binary operator")),
                    Ops(InfixL)(choice(Add <# "+", Minus <# "-")
                                .label("binary operator")),
                    Ops(InfixN)(choice(LT <# "<", LTEq <# "<=", GT <# ">",  GTEq <# ">=", Eq <# "==", Neq <# "!=")
                                .label("binary operator")),        
                    Ops(InfixR)(And <# "&&".label("binary operator")),      

                    Ops(InfixR)(Or <# "||".label("binary operator")))

    lazy val `<ident>` = _cStylePointer <|> Ident(IDENT_ID)
                        .label("identifier")

    lazy val `<array-elem>` = ArrayElem(`<ident>`.label("identifier"), some("[".label("index (like `xs[idx]`)") *> `<expr>` <* "]".label("close square bracket")))

    lazy val `<int-liter>` = IntLiter(INTEGER).label("integer")

    lazy val `<bool-liter>` = BoolLiter(BOOL_LIT <* many("\t")).label("boolean literal")
    lazy val `<char-liter>` = CharLiter(CHAR_LIT).label("character literal")
    lazy val `<str-liter>` = StrLiter(STRING_LIT).label("string literal")
    lazy val `<array-liter>` = ArrayLiter(`<heap-stack>`, "[".label("open square bracket") *> option(sepBy1(`<expr>`, ","))<* "]".label("close square bracket")).label("Array")
    lazy val `<heap-stack>` = "stack" #> Stack <|> pure("") #> Heap
    lazy val `<pair-liter>` = ("null".label("null") #> PairLiter)

    private val _functionNoType
        = attempt(`<ident>` *> "()").hide *> fail("function declarations must have a type")

    private val _paramNoType 
        = amend(`<ident>`.hide *> fail("all parameters must have a given type"))

    private lazy val _cStylePointer
        = amend(("*" *> Ident(IDENT_ID)).hide *> fail("Unexpected *\n", "parameters are declared with a type and identifier, do not use *"))

    private val _functionWrongScope
        = amend(attempt(`<type>` *> `<ident>` *> "(").hide *> unexpected("function declaration").explain("functions must be declared at the top of the main block"))
   
    private val _noFuncWithoutCallCheck =
        amend(attempt(`<ident>` *> "(").hide *> unexpected("(").explain("functions must use call"))

    // private val _noFuncInOperation =
    //     amend(attempt(`<ident>` *> "(").hide *> fail("Unexpected (\n", "function calls may not appear in expressions and must use call"))
}
