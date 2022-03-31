package inca.codeExamples

object JavaCodeExamples {

  val annotationTypeDeclarationSrcCode: String =
    s"""
       |@interface test {
       |  String name();
       |  int age();
       |  String[] relatives();
       |}
       |""".stripMargin

  val annotationTypeDeclarationDestCode: String =
    s"""
       |public class Main {
       |}
       |
       |@interface test {
       |  String name();
       |  int age();
       |}
       |""".stripMargin

  val annotationTypeElementDeclarationSrcCode: String =
    s"""
       |@interface test {
       |  String name();
       |  int age();
       |}
       |""".stripMargin

  val annotationTypeElementDeclarationDestCode: String =
    s"""
       |@interface test {
       |  String name();
       |  int age();
       |  String[] relatives() default {};
       |}
       |""".stripMargin

  val annotationSrcCode: String =
    s"""
       |public class Main {
       |  @Override(Argument)
       |  public int test() {
       |    return 42;
       |  }
       |}
       |""".stripMargin

  val annotationDestCode: String =
    s"""
       |public class Main {
       |  @Override(Argument)
       |  public int test() {
       |    return 42;
       |  }
       |
       |  @Retention(NewArgument)
       |  public int newTest() {
       |    return 12;
       |  }
       |}
       |""".stripMargin

  val arrayAccessSrcCode: String =
    s"""
       |int[] arr = { 1, 2, 3 };
       |arr[2];
       |""".stripMargin

  val arrayAccessDestCode: String =
    s"""
       |int[] arr = { 1, 2, 3 };
       |arr[0 + 1];
       |""".stripMargin

  val arrayCreationExpressionSrcCode: String =
    s"""
       |int[] arr = new int [] { 3, 4, 5 };
       |""".stripMargin

  val arrayCreationExpressionDestCode: String =
    s"""
       |int[] arr = new int[] { 3, 4, 5 };
       |new int[] { 1, 2, 3 };
       |""".stripMargin

  val arrayTypeSrcCode: String =
    s"""
       |int[] arr;
       |""".stripMargin

  val arrayTypeDestCode: String =
    s"""
       |double[] darr = new double[5];
       |""".stripMargin

  val assignmentExpressionSrcCode: String =
    s"""
       |x = 5;
       |""".stripMargin

  val assignmentExpressionDestCode: String =
    s"""
       |y += (7 * 5);
       |""".stripMargin

  val binaryExpressionSrcCode: String =
    s"""
       |1 + 2;
       |""".stripMargin

  val binaryExpressionDestCode: String =
    s"""
       |(3 * 5) / (4 - 2);
       |""".stripMargin

  val castExpressionSrcCode: String =
    s"""
       |int x = (int)(x) -> { x + 5; };
       |""".stripMargin

  val castExpressionDestCode: String =
    s"""
       |double x = (double) 4 + 2;
       |""".stripMargin

  val catchClauseSrcCode: String =
    s"""
       |public class Main {
       |  public void main() {
       |    try {
       |      return 1;
       |    } catch (IOException exp) {
       |      System.out.println(\"IO\");
       |    }
       |  }
       |}
       |""".stripMargin

  val catchClauseDestCode: String =
    s"""
       |public class Main {
       |  public void main() {
       |    try {
       |      return 1;
       |    } catch (Exception e) {
       |      System.out.println(\"Exception\");
       |    } catch (IOException exp) {
       |      System.out.println(\"IO\");
       |    }
       |  }
       |}
       |""".stripMargin

  val catchFormalParameterSrcCode: String =
    s"""
       |public void func() {
       |  try {
       |    return 5;
       |  } catch (IOException exp) {
       |    System.out.println(\"IO\");
       |  }
       |}
       |""".stripMargin

  val catchFormalParameterDestCode: String =
    s"""
       |public void func() {
       |  try {
       |    return 5;
       |  } catch (Exception | IOException e[]) {
       |    System.out.println(\"Exception\");
       |  }
       |}
       |""".stripMargin

  val classDeclarationSrcCode: String =
    s"""
       |class Main {
       |}
       |""".stripMargin

  val classDeclarationDestCode: String =
    s"""
       |public class NewMain extends Superclass implements Interface {
       |  public void func() {
       |  }
       |}
       |""".stripMargin

  val constantDeclarationSrcCode: String =
    s"""
       |final int[] x = new int[5];
       |""".stripMargin

  val constantDeclarationDestCode: String =
    s"""
       |final double x;
       |""".stripMargin

  val constructorDeclarationSrcCode: String =
    s"""
       |public class Main {
       |  private Main(int i, int d) {
       |  }
       |}
       |""".stripMargin

  val constructorDeclarationDestCode: String =
    s"""
       |public class Main {
       |  public Main(int i, double d) {
       |    System.out.println(\"Create new object\");
       |  }
       |}
       |""".stripMargin

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
}
