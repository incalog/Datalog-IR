package inca.exampleEditScripts

import inca.treesitterAPI.treesitterMappings.TSDiffResult
import inca.treesitterLanguage.JavaTreeSitter.createDiffResult

object JavaEditScripts {

  val noChangesSrcCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val noChangesDestCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val noChangesEditScript: TSDiffResult = createDiffResult(noChangesSrcCode, noChangesDestCode)

  val updatedIntegerSrcCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val updatedIntegerDestCode: String =
    s"""
       |public class Main {
       |  int x = 6;
       |}
       |""".stripMargin
  val updatedIntegerEditScript: TSDiffResult = createDiffResult(updatedIntegerSrcCode, updatedIntegerDestCode)

  val updatedVariableNameSrcCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val updatedVariableNameDestCode: String =
    s"""
       |public class Main {
       |  int y = 5;
       |}
       |""".stripMargin
  val updatedVariableNameEditScript: TSDiffResult = createDiffResult(updatedVariableNameSrcCode, updatedVariableNameDestCode)

  val updatedIdentifierAndIntegerSrcCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val updatedIdentifierAndIntegerDestCode: String =
    s"""
       |public class Main {
       |  int y = 6;
       |}
       |""".stripMargin
  val updatedIdentifierAndIntegerEditScript: TSDiffResult = createDiffResult(updatedIdentifierAndIntegerSrcCode, updatedIdentifierAndIntegerDestCode)

  val updatedMultipleIdentifiersAndIntegerSrcCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val updatedMultipleIdentifiersAndIntegerDestCode: String =
    s"""
       |public class MainClass {
       |  int y = 6;
       |}
       |""".stripMargin
  val updatedMultipleIdentifiersAndIntegerEditScript: TSDiffResult = createDiffResult(updatedMultipleIdentifiersAndIntegerSrcCode, updatedMultipleIdentifiersAndIntegerDestCode)

  val addedNewFreeLineSrcCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val addedNewFreeLineDestCode: String =
    s"""
       |public class Main {
       |
       |  int x = 5;
       |}
       |""".stripMargin
  val addedNewFreeLineEditScript: TSDiffResult = createDiffResult(addedNewFreeLineSrcCode, addedNewFreeLineDestCode)

  val addedNewModifierSrcCode: String =
    s"""
       |class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val addedNewModifierDestCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val addedNewModifierEditScript: TSDiffResult = createDiffResult(addedNewModifierSrcCode, addedNewModifierDestCode)

  val addedModifierSrcCode: String =
    s"""
       |public class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val addedModifierDestCode: String =
    s"""
       |public final class Main {
       |  int x = 5;
       |}
       |""".stripMargin
  val addedModifierEditScript: TSDiffResult = createDiffResult(addedModifierSrcCode, addedModifierDestCode)

  val addedFunctionSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin
  val addedFunctionDestCode: String =
    s"""
       |public class Main {
       |  void myMethod() {
       |    System.out.println("I have been executed!");
       |  }
       |}
       |""".stripMargin
  val addedFunctionEditScript: TSDiffResult = createDiffResult(addedFunctionSrcCode, addedFunctionDestCode)

  val addedNewFunctionModifierSrcCode: String =
    s"""
       |public class Main {
       |  void myMethod() {
       |    System.out.println("I just got executed!");
       |  }
       |}
       |""".stripMargin
  val addedNewFunctionModifierDestCode: String =
    s"""
       |public class Main {
       |  public void myMethod() {
       |    System.out.println("I just got executed!");
       |  }
       |}
       |""".stripMargin
  val addedNewFunctionModifierEditScript: TSDiffResult = createDiffResult(addedNewFunctionModifierSrcCode, addedNewFunctionModifierDestCode)

  val swapFunctionOrder1SrcCode: String =
    s"""
       |public class Main {
       |  boolean myMethod() {
       |    return false;
       |  }
       |  boolean myOtherMethod() {
       |    return true;
       |  }
       |}
       |""".stripMargin
  val swapFunctionOrder1DestCode: String =
    s"""
       |public class Main {
       |  boolean myOtherMethod() {
       |    return true;
       |  }
       |  boolean myMethod() {
       |    return false;
       |  }
       |}
       |""".stripMargin
  val swapFunctionOrder1EditScript: TSDiffResult = createDiffResult(swapFunctionOrder1SrcCode, swapFunctionOrder1DestCode)

  val swapLoopsSrcCode: String =
    s"""
       |public class Main {
       |  void myMethod() {
       |    for (int i = 0; i < 5; i++) {
       |      System.out.println(i);
       |      }
       |  }
       |  void MyOtherMethod() {
       |    for (int i = 4; i >= 0; i--) {
       |      System.out.println(i);
       |    }
       |  }
       |}
       |""".stripMargin
  val swapLoopsDestCode: String =
    s"""
       |public class Main {
       |  void myMethod() {
       |    for (int i = 4; i >= 0; i--) {
       |      System.out.println(i);
       |    }
       |  }
       |  void MyOtherMethod() {
       |    for (int i = 0; i < 5; i++) {
       |      System.out.println(i);
       |    }
       |  }
       |}
       |""".stripMargin
  val swapLoopsEditScript: TSDiffResult = createDiffResult(swapLoopsSrcCode, swapLoopsDestCode)
}
