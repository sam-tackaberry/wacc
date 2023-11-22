package frontEnd

import parsley.Parsley
object lexer {

    import parsley.token.Lexer
    import parsley.character.{string, digit}
    import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc, SpaceDesc, numeric, text}
    import parsley.token.predicate.{Unicode, Basic}
    import parsley.Parsley.{attempt, notFollowedBy}


    val keywords = Set( "null", "begin", "end", "is", "skip", "read", "free", "return", "exit", 
                                "print", "println", "if", "then", "else", "fi", "while", "do", 
                                "done", "fst", "snd", "call", "int", "bool", "char", 
                                "string", "pair", "true", "false", "len", "ord", "chr", "new")

    private val waccDesc = LexicalDesc(
        NameDesc.plain.copy(
            identifierStart = Unicode(c => Character.isLetter(c) || c == '_'),
            identifierLetter = Unicode(c => Character.isLetterOrDigit(c) || c == '_'),
        ),
        SymbolDesc.plain.copy(
            hardKeywords = keywords, //Null and newpair removed from Keywords
          
            hardOperators = Set("*", "/", "%", "+", "-", ">", ">=", "<", "<=", "==", "!=", "&&", 
                                "||", "!"),
            caseSensitive = true,
        ),
        numeric.NumericDesc.plain.copy(
            leadingZerosAllowed = true,
        ),
        text.TextDesc.plain.copy(
            escapeSequences = text.EscapeDesc.plain.copy(
                literals = Set('\\', '\"', '\''),
                singleMap = Map('0' -> 0x0000,
                                'b' -> 0x0008,
                                'f' -> 0x000c,
                                'n' -> 0x000a,
                                'r' -> 0x000d,
                                't' -> 0x0009),
            ),
            graphicCharacter = Unicode(x => x >= ' '.toInt && x != 34 && x!= 39 && x!= 92),
        ),
        SpaceDesc.plain.copy(
            commentLine = "#",
            nestedComments = false,
            commentLineAllowsEOF = true,
            space = Basic(c => c == ' ' || c == '\t' || c == '\n'),
        )
    )

    val lexer = new Lexer(waccDesc)

    // Do we need a CON_ID and VAR_ID ??

    val IDENT_ID = lexer.lexeme.names.identifier

    // unary minus use notFollowed by a digit
    
    //use decimal instead of number because wacc does not support octal and binary and hex
    val INTEGER = lexer.lexeme.numeric.integer.decimal32

    //val INT_SIGN = string("+") #> + 
    //val INT_LIT = option(string("-") <|> string("+")) *> INTEGER
    val BOOL_LIT = lexer.lexeme.symbol("true") #> true <|> lexer.lexeme.symbol("false") #> false
    val CHAR_LIT = lexer.lexeme.text.character.ascii //(Lexer.waccDesc.escapeSequences)
    val STRING_LIT = lexer.lexeme.text.string.ascii
    
    val UNIOPMIN = lexer.lexeme(attempt(string("-") *> notFollowedBy(digit)))
    //= string("-") *> notFollowedBy(INTEGER) 
    
    val PAIR_LIT = string("null") #> null

    //val NEWLINE = lexer.lexeme(newline).void

    def fully[A](p: Parsley[A]) = lexer.fully(p)

    val implicits = lexer.lexeme.symbol.implicits
}

