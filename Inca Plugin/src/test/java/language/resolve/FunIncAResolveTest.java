package language.resolve;


public class FunIncAResolveTest extends FunIncAResolveTestCase {

    public void testConstructor1(){doTest();} // constructor ref in match case
    public void testConstructor2(){doTest();} // constructor call
    public void testConstructor3(){doTest();} // parameter shadows constructor
    public void testConstructor4(){doTest(false);} // parameter shadows constructor
    public void testDatatypes(){doTest();}
    public void testFold(){doTest();} // cannot be parsed?? AST only contains dummy blocks
    public void testHigherOrderFunction1(){doTest();}
    public void testHigherOrderFunction2(){doTest();}
    public void testLet(){doTest();}
    public void testLet2(){doTest(false);}
    public void testMatchCase(){doTest();}
    public void testMethod1(){doTest();} // call of function definition
    public void testMethod2(){doTest(false);} // unresolved call
    public void testMethod3(){doTest();} // recursion
    public void testMethod4(){doTest(false);} // parameter and function have the same name
    public void testMethod5(){doTest();} // comparing two methods
    public void testMethod6(){doTest();} // parameter def shadows function def
    public void testMethod7(){doTest(false);} // parameter def shadows function def
    public void testMethod8(){doTest();} // function as set member
    public void testMultLet1(){doTest();}
    public void testMultLet2(){doTest();}
    public void testParameter(){doTest();}
    public void testParamTypesArgument(){doTest();}
    public void testParamTypesReturn(){doTest();}
    public void testParamTypesDataDef1(){doTest();}
    public void testParamTypesDataDef2(){doTest();}
    public void testSetComprehension1(){doTest();}
    public void testSetComprehension2(){doTest();}
    public void testSetComprehension3(){doTest();} // function call within set comprehension
    public void testSetComprehension4(){doTest();} // higher order function within set comprehension
    public void testSetComprehension5(){doTest();} // set comprehension with 2 declarations in member expressions
    public void testSetComprehension6(){doTest(false);} // set comprehension with 2 declarations in member expressions
    public void testSetComprehension7(){doTest();} // set comprehension with 2 declarations in member expressions
    public void testSetComprehension8(){doTest(false);} // set comprehension within set comprehension
    public void testSetComprehension9(){doTest();} // set comprehension within set comprehension
    public void testSetComprehension10(){doTest(false);}
    public void testSetComprehensionShadowing1(){doTest();}
    public void testSetComprehensionShadowing2(){doTest();}
}
