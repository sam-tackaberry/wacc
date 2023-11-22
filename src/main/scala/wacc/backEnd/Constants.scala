package wacc.backEnd

import assemblyIR._

object constants {

    val maxMovInt = 2048

    // program top level labels
    val main = "main"
    val globalMainLabel = Label(".global main")


    // Size
    val WORD_SIZE = 4
    val ONE_BYTE = 1
    val TWO_CHARS = 2
    val DOUBLE_W_SIZE = 8


    // Boolean
    val TRUE = 1
    val FALSE = 0

    // ERROR CODES
    val SUCCESS = 0
    val SYNTAX_ERROR = 100
    val SEMANTIC_ERROR = 200
    val RUN_TIME_ERROR = 255


    // COMPILER file
    val identStr = "identifier "
    val intStr = "integer "
    val keywordStr = "keyword "
    val assembFile = ".s"

    
    // Common operands 
    val IMM_ZERO = ImmNum(0)
    val IMM_ONE = ImmNum(1)
    val IMM_TWO = ImmNum(2)
    val IMM_FOUR = ImmNum(4)
    val IMM_M_FOUR = ImmNum(-4)

    // String translations of operands/instructions

    val EQ_STR = "eq "
    val GT_STR = "gt "
    val LT_STR = "lt "
    val GE_STR = "ge "
    val LE_STR = "le "
    val NE_STR = "ne "
    val VS_STR = "vs "

    // PRINTING FORMAT
    val space = " "
    val emptyStr = ""
    val argSep = ", "
    val tab = "\t"
    val twoTabs = "\t\t"

    // LABELS FOR Input/Output
    val genericLabel = ".L"
    val dataLabel = Label(".data")
    val textLabel = Label(".text")

    val strDeclrString = ".str_d"
    val strString = ".str"


    val stringlnFormatLabel = Label(".stringln_format")
    val stringFormatLabel = Label(".string_format")

    val stringFormat = "%.*s"
    val decimalFormat = "%d"
    val charFormat = "%c"
    val pairFormat = "%p"
    val readCharFormat = " %c"

    val falseStr = "false"
    val falseFormatLabel = Label(".false_format")

    val intFormatLabel = ".int_format"
    val charFormatLabel = ".char_format"
    val pairFormatLabel = ".pair_format"

    val trueStr = "true"
    val trueFormatLabel = Label(".true_format")

    val boolFormatLabel = Label(".bool_format")

    val printTrueLabel = Label(".print_true")

    val printBoolLabel = Label(".print_bool")

    val printBLabel = Label("_printb")
    val printiLabel = Label("_printi")
    val printcLabel = Label("_printc")
    val printsLabel = Label("_prints")
    val printpLabel = Label("_printp")

    val readCLabel = Label("_readc")
    val readILabel = Label("_readi")
    val readCFormatLabel = Label(".readc_format")
    val readIFormatLabel = Label(".readi_format")


    // SYS CALLS
    val scanfLabel = Label("scanf")
    val printlnLabel = Label("_println")
    val printFLabel = Label("printf")
    val fflushLabel = Label("fflush")
    val putsLabel = Label("puts")
    val mallocLabel = Label("malloc")
    val exitLabel = Label("exit")
    val freeLabel = Label("free")



    val divModLabel = Label("__aeabi_idivmod")


    // Heap and Stack labels
    val arrayLoadLabel = Label("_arrLoad")
    val arrayLoadStackLabel = Label("_arrLoadStack")
    val arrayLoadBLabel = Label("_arrLoadB")
    val arrayLoadBStackLabel = Label("_arrLoadBStack")
    val arrayStoreLabel = Label("_arrStore")
    val arrayStoreBLabel = Label("_arrStoreB")
    val freePairLabel = Label("_freePair")


    // STACK 
    val OFFSET_INIT = 4
    val ZERO = 0
    val ONE = 1

    // Array
    val ARRAY_INDEX_INIT = -4

    // Run Time Error Label Names
    val divByZeroFormatLabel = Label(".L._errDivZero_str0")
    val divByZeroLabel = Label("_errDivZero")

    val overflowFormatLabel = Label(".L._prints_str0")
    val overflowLabel = Label("_errOverflow")

    val boundsCheckFormatLabel = Label(".L._boundsCheck_str0")
    val boundsCheckLabel = Label("_boundsCheck")

    val nullDereferenceFormatLabel = Label(".L._errNull_str0")
    val nullDereferenceLabel = Label("_errNull")

    // Run Time Error Messages
    val divByZeroMsg = "fatal error: division or modulo by zero\\n"
    val overflowMsg = "fatal error: integer overflow or underflow occurred\\n"
    val boundsCheckMsg = "fatal error: array index %d out of bounds\\n"
    val nullDereferenceMsg = "fatal error: null pointer dereferenced or freedz\\n"

    //Run Time Error Message Lengths
    val divByZeroMsgLen = divByZeroMsg.length()
    val overflowMsgLen = overflowMsg.length()
    val boundsCheckMsgLen = boundsCheckMsg.length()
    val nullDereferenceMsgLen = nullDereferenceMsg.length()
}