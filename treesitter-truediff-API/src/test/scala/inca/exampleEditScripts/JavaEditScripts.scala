package inca.exampleEditScripts

import inca.treesitterAPI.treesitterMappings.TSDiffResult
import inca.treesitterLanguage.JavaTreeSitter.createDiffResult

object JavaEditScripts {

  val noChangesSrcCode: String =
    """
                public class Main {
                   int x = 5;
                }
                """
  val noChangesDestCode: String =
    """
                public class Main {
                   int x = 5;
                }
                """
  val noChangesEditScript: TSDiffResult = createDiffResult(noChangesSrcCode, noChangesDestCode)

  val updatedIntegerSrcCode: String =
    """
                public class Main {
                   int x = 5;
                }
                """
  val updatedIntegerDestCode: String =
    """
                public class Main {
                   int x = 6;
                }
                """
  val updatedIntegerEditScript: TSDiffResult = createDiffResult(updatedIntegerSrcCode, updatedIntegerDestCode)

  val updatedVariableNameSrcCode: String =
    """
                public class Main {
                    int x = 5;
                }
                """
  val updatedVariableNameDestCode: String =
    """
                public class Main {
                    int y = 5;
                }
                """
  val updatedVariableNameEditScript: TSDiffResult = createDiffResult(updatedVariableNameSrcCode, updatedVariableNameDestCode)

  val updatedIdentifierAndIntegerSrcCode: String =
    """
                public class Main {
                    int x = 5;
                }
                """
  val updatedIdentifierAndIntegerDestCode: String =
    """
                public class Main {
                    int y = 6;
                }
                """
  val updatedIdentifierAndIntegerEditScript: TSDiffResult = createDiffResult(updatedIdentifierAndIntegerSrcCode, updatedIdentifierAndIntegerDestCode)

  val updatedMultipleIdentifiersAndIntegerSrcCode: String =
    """
                public class Main {
                    int x = 5;
                }
                """
  val updatedMultipleIdentifiersAndIntegerDestCode: String =
    """
                public class MainClass {
                    int y = 6;
                }
                """
  val updatedMultipleIdentifiersAndIntegerEditScript: TSDiffResult = createDiffResult(updatedMultipleIdentifiersAndIntegerSrcCode, updatedMultipleIdentifiersAndIntegerDestCode)

  val addedNewFreeLineSrcCode: String =
    """
                public class Main {
                    int x = 5;
                }
                """
  val addedNewFreeLineDestCode: String =
    """
                public class Main {

                    int x = 5;
                }
                """
  val addedNewFreeLineEditScript: TSDiffResult = createDiffResult(addedNewFreeLineSrcCode, addedNewFreeLineDestCode)

  val addedNewModifierSrcCode: String =
    """
                class Main {
                    int x = 5;
                }
                """
  val addedNewModifierDestCode: String =
    """
                public class Main {
                    int x = 5;
                }
                """
  val addedNewModifierEditScript: TSDiffResult = createDiffResult(addedNewModifierSrcCode, addedNewModifierDestCode)

  val addedModifierSrcCode: String =
    """
                public class Main {
                    int = 5;
                }
                """
  val addedModifierDestCode: String =
    """
                public final class Main {
                    int x = 5;
                }
                """
  val addedModifierEditScript: TSDiffResult = createDiffResult(addedModifierSrcCode, addedModifierDestCode)

  val addedFunctionSrcCode: String =
    """
                public class Main {
                }
                """
  val addedFunctionDestCode: String =
    """
                public class Main {
                   void myMethod() {
                       System.out.println("I have been executed!");
                   }
                }
                """
  val addedFunctionEditScript: TSDiffResult = createDiffResult(addedFunctionSrcCode, addedFunctionDestCode)

  val addedNewFunctionModifierSrcCode: String =
    """
                public class Main {
                    void myMethod() {
                        System.out.println("I just got executed!");
                    }
                }
                """
  val addedNewFunctionModifierDestCode: String =
    """
                public class Main {
                    public void myMethod() {
                        System.out.println("I just got executed!");
                    }
                }
                """
  val addedNewFunctionModifierEditScript: TSDiffResult = createDiffResult(addedNewFunctionModifierSrcCode, addedNewFunctionModifierDestCode)

  val swapFunctionOrder1SrcCode: String =
    """
                public class Main {
                    boolean myMethod() {
                        return false;
                    }
                    boolean myOtherMethod() {
                        return true;
                    }
                }
                """
  val swapFunctionOrder1DestCode: String =
    """
                public class Main {
                    boolean myOtherMethod() {
                        return true;
                    }
                    boolean myMethod() {
                        return false;
                    }
                }
                """
  val swapFunctionOrder1EditScript: TSDiffResult = createDiffResult(swapFunctionOrder1SrcCode, swapFunctionOrder1DestCode)

  val swapLoopsSrcCode: String =
    """
                public class Main {
                    void myMethod() {
                        for (int i = 0; i < 5; i++) {
                            System.out.println(i);
                        }
                    }
                    void MyOtherMethod() {
                        for (int i = 4; i >= 0; i--) {
                            System.out.println(i);
                        }
                    }
                }
                """
  val swapLoopsDestCode: String =
    """
                public class Main {
                    void myMethod() {
                        for (int i = 4; i >= 0; i--) {
                            System.out.println(i);
                        }
                    }
                    void MyOtherMethod() {
                        for (int i = 0; i < 5; i++) {
                            System.out.println(i);
                        }
                    }
                }
                """
  val swapLoopsEditScript: TSDiffResult = createDiffResult(swapLoopsSrcCode, swapLoopsDestCode)
}
