all:
	sbt test:compile assembly
	
testAll:
	sbt test

testBackEndUtils:
	sbt "testOnly wacc.backEndTests.BackEndUtilsSpec"

testTranslateInstr:
	sbt "testOnly wacc.backEndTests.TranslateInstrSpec"

testRTEChecking: 
	sbt "testOnly wacc.backEndTests.RTECheckingSpec"

clean:
	sbt clean && rm -rf wacc-09-compiler.jar

.PHONY: all clean
