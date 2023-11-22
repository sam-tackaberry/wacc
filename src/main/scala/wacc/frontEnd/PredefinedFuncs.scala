package frontEnd


import java.io.File
import semanticAst._
import scala.collection.mutable.ListBuffer
import frontEnd.ast._
import wacc.main.readFile
import wacc.compiler.parse
import semanticAnalyserUtils.{typeToIdObj, paramListToListOfSemTypes}

object predefFuncs{

    val STD_LIB_DIR = "waccpackages/"
    val LIB_SUFFIX = ".wacc"

    // This holds the functions available from Standard libraries (not the ones we generate code for)
    var funcsInLib: ListBuffer[Function] = ListBuffer[Function]()

    // Flag to tell us whether we have loaded the functions into funcsInLib
    val stdLibLoaded = false

    // This holds the functions we have actually called in the program and will generate code for
    val predefForProg: ListBuffer[Function] = ListBuffer[Function]()

    // Called from semantic analyser, finds the right predefined function (if it exists) and adds it to predefForProg
    def addPredefFunc(i: Ident, lValReturnType : SemType, as: Option[List[SemType]]): Option[Function] = { 
        val function: Option[Function] = getPreDefFunc(i, lValReturnType, as)
        function
    }


    // Used in addPredefFunc to search for the right predefined function
    def getPreDefFunc(i: Ident, lValReturnType : SemType, as: Option[List[SemType]]): Option[Function] = {
        var func: Option[Function] = None
        for (f@Function(typeAndIdent, paramList, _) <- funcsInLib) {
            if (typeToIdObj(typeAndIdent._1) == lValReturnType && typeAndIdent._2 == (i) && as == paramListToListOfSemTypes(paramList)){
                // If we have found the right function (based on name, return type and parameter types), add it to our list of functions that we will generate code for
                    predefForProg += f     
                    func = Some(f)
            }
        }
        func
    }

    // Getter used in code generation
    def getpredefForProg(): ListBuffer[Function] = {
        predefForProg
    }


    // Loading in the functions from the standard library into funcsInLib
    // def loadStdLib(): Unit = {
    //     val prog: Result[Any, WACCProgram] = parse(readFile(STD_LIB))
    //     val WACCProgram(_, functions, _) = prog.get
    //     val funcs = ListBuffer(functions: _*)
    //     funcsInLib = funcs
    // }


    // loading in functions from a library imported by the user
    // these go into funcsInLib and are not code generated unless called by the user
    def loadLibrary(i: Import): Boolean = {
        val libName = i.lib.name + LIB_SUFFIX
        if (checkFileExists(STD_LIB_DIR, libName)) {
            // add it to the list of our functions available to user via the import
            val lib = parse(readFile(STD_LIB_DIR + libName))
            val WACCProgram(_, functions, _) = lib.get
            funcsInLib ++= ListBuffer(functions: _*)
            true
        } else{
            false
        }
    }


    def checkFileExists(directoryPath: String, fileName: String): Boolean = {
      val file = new File(directoryPath, fileName)
      file.exists() && file.isFile()
    }





}
