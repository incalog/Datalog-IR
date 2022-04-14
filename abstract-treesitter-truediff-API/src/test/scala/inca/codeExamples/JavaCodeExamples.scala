package inca.codeExamples

object JavaCodeExamples {

  val attachWithoutLoadSrcCode: String = // TODO: Fix attaching of node without loading it before.
    s"""
       |public class Main {
       |}
       |public class Test {
       |}
       |@interface test {
       |  String name();
       |  int age();
       |  String[] relatives();
       |}
       |""".stripMargin

  val attachWithoutLoadDestCode: String =
    s"""
       |public class Main {
       |}
       |@interface test {
       |  String name();
       |  int age();
       |  String[] relatives();
       |}
       |""".stripMargin

  val incorrectLinkTrackingWithMultipleAttachAndDetachOperationsSrcCode: String = // TODO: Fix link tracking when multiple Attach and Detach operations occur in EditScript.
    s"""
       |public class Main {
       |}
       |public class Test {
       |}
       |@interface test {
       |  String name();
       |  int age();
       |  int siblingCount();
       |  String[] relatives();
       |}
       |""".stripMargin

  val incorrectLinkTrackingWithMultipleAttachAndDetachOperationsDestCode: String =
    s"""
       |public class Main {
       |}
       |public class Test {
       |}
       |@interface test {
       |  int age();
       |  int siblingCount();
       |}
       |""".stripMargin

  val annotationTypeDeclarationSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val annotationTypeDeclarationDestCode: String =
    s"""
       |public class Main {
       |}
       |@interface test {
       |  String name();
       |  int age();
       |  String[] relatives();
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

  val doStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val doStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    do {
       |      x += 1;
       |    } while (x <= 4)
       |  }
       |}
       |""".stripMargin

  val elementValuePairSrcCode: String =
    s"""
       |public class Main {
       |  @annotation
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val elementValuePairDestCode: String =
    s"""
       |public class Main {
       |  @annotation(key1 = value1, key2 = value2)
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val enhancedForStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val enhancedForStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    for (object obj : Object) {
       |      x += 1;
       |    }
       |  }
       |}
       |""".stripMargin

  val enumConstantSrcCode: String =
    s"""
       |public enum Enum1 {
       |  ONE;
       |}
       |public enum Enum2 {
       |  ONE;
       |}
       |""".stripMargin

  val enumConstantDestCode: String =
    s"""
       |public enum Enum1 {
       |  ONE(42), TWO(12);
       |}
       |public enum Enum2 {
       |  ONE {
       |    @Override
       |    public void myFunc() {
       |    }
       |  },
       |  TWO {
       |    @Override
       |    public void myFunc() {
       |    }
       |  };
       |
       |  public abstract void myFunc();
       |}
       |""".stripMargin

  val enumDeclarationSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val enumDeclarationDestCode: String =
    s"""
       |public class Main {
       |}
       |public enum Enum1 implements Enum2 {
       |  ONE, TWO, THREE, FOUR;
       |}
       |""".stripMargin

  val explicitConstructorInvocationSrcCode: String =
    s"""
       |public class Main {
       |  public Main() {
       |  }
       |  public Main() {
       |  }
       |  public Main() {
       |  }
       |}
       |""".stripMargin

  val explicitConstructorInvocationDestCode: String =
    s"""
       |public class Main {
       |  public Main() {
       |    <int>super(3, 4);
       |  }
       |  public Main() {
       |    <int>this(3, 4);
       |  }
       |  public Main() {
       |    obj.super(3, 4);
       |  }
       |""".stripMargin

  val fieldAccessSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val fieldAccessDestCode: String =
    s"""
       |public class Main {
       |}
       |super.super.module;
       |super.module;
       |super.field1;
       |obj.super.module;
       |obj.module;
       |obj.field1;
       |""".stripMargin

  val fieldDeclarationSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val fieldDeclarationDestCode: String =
    s"""
       |public class Main {
       |  public int a, b, c = 0;
       |}
       |""".stripMargin

  val forStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val forStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    for (int i = 0; i < 5; i++) {
       |      System.out.println(i);
       |    }
       |    for (x * 2; x < 42; x++) {
       |      System.out.println(x);
       |    }
       |  }
       |}
       |""".stripMargin

  val formalParameterSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val formalParameterDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc(int x, double y, String z) {
       |  }
       |}
       |""".stripMargin

  val ifStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val ifStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    if (5 > 3) {
       |      System.out.println(5);
       |    } else {
       |      System.out.println(3);
       |    }
       |  }
       |}
       |""".stripMargin

  val instanceofExpressionSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    boolean b;
       |  }
       |}
       |""".stripMargin

  val instanceofExpressionDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    boolean b = (5 + 3) instanceof int;
       |  }
       |}
       |""".stripMargin

  val interfaceDeclarationSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val interfaceDeclarationDestCode: String =
    s"""
       |public class Main {
       |}
       |public interface Interface<int> extends int {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val lambdaExpressionSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val lambdaExpressionDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    x -> (x + 5);
       |    x -> { x + 5; }
       |    (int i, double d) -> i <= d;
       |    (int i, double d) -> { i <= d; }
       |    (x, y) -> x + y;
       |    (x, y) -> { x == y; }
       |  }
       |}
       |""".stripMargin

  val localVariableDeclarationSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val localVariableDeclarationDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x, y, z = 0;
       |  }
       |}
       |""".stripMargin

  val markerAnnotationSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val markerAnnotationDestCode: String =
    s"""
       |public class Main {
       |  @Override
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val methodDeclarationSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val methodDeclarationDestCode: String =
    s"""
       |public class Main {
       |  public <int>@annotation void myFunc(int x, int y) @dimensions[] {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val methodInvocationSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val methodInvocationDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    myOtherFunc(x, y);
       |    obj.super.<int, double>func(a, b, c);
       |  }
       |}
       |""".stripMargin

  val moduleDeclarationSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val moduleDeclarationDestCode: String =
    s"""
       |public class Main {
       |}
       |@annotation open module Module {
       |  requires transitive otherMod;
       |  exports This to Module1, Module2;
       |  opens This to Module1, Module2;
       |  uses Module3;
       |  provides func1 with func2, func3;
       |}
       |""".stripMargin

  val objectCreationExpressionSrcCode: String =
    s"""
       |public class Main {
       |}
       |""".stripMargin

  val objectCreationExpressionDestCode: String =
    s"""
       |public class Main {
       |  int x = new <int>List<double>(x, y);
       |  int y = new <int>List<double>(x, y) {
       |    double d = 0;
       |  };
       |  int z = new List<double>(x, y);
       |  int w = new List<double>(x, y) {
       |    double d = 0;
       |  };
       |  int d = obj.new <int>List<double>(x, y);
       |  int e = obj.new <int>List<double>(x, y) {
       |    double d = 0;
       |  };
       |  int f = obj.new List<double>(x, y);
       |  int g = obj.new List<double>(x, y) {
       |    double d = 0;
       |  };
       |}
       |""".stripMargin

  val recordDeclarationSrcCode: String =
    s"""
       |public class Main {
       |  int x = 0;
       |}
       |""".stripMargin

  val recordDeclarationDestCode: String =
    s"""
       |public class Main {
       |  int x = 0;
       |  public record Record(int x, double y, String z) {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val resourceSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val resourceDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    try (public int x = 5; double d = 33) {
       |      double e = 0;
       |    } catch (Exception e) {
       |      return;
       |    }
       |    try (a; b; c) {
       |      double e = 0;
       |    } catch (Exception e) {
       |      return;
       |    }
       |    try (obj.field) {
       |      double e = 0;
       |    } catch (Exception e) {
       |      return;
       |    }
       |  }
       |}
       |""".stripMargin

  val switchExpressionSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val switchExpressionDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    switch (obj) {
       |      case 0 -> return;
       |      case 1 -> (2 + 5);
       |      case 2 -> a == b;
       |      default -> 42;
       |    }
       |  }
       |}
       |""".stripMargin

  val synchronizedStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val synchronizedStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    synchronized (3 + 5) {
       |      double e = 0;
       |    }
       |  }
       |}
       |""".stripMargin

  val scopedIdentifierSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val scopedIdentifierDestCode: String =
    s"""
       |public class Main {
       |  @system.out
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val ternaryExpressionSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val ternaryExpressionDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    a > b ? 12 : 42;
       |  }
       |}
       |""".stripMargin

  val tryStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val tryStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    try {
       |      a = 12 + 42;
       |    } catch (Exception e) {
       |      return;
       |    } finally {
       |      a = 42;
       |    }
       |  }
       |}
       |""".stripMargin

  val tryWithResourcesStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val tryWithResourcesStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    try (a; b; c) {
       |      a > b ? c : 42;
       |    } catch (Exception e) {
       |      return;
       |    } finally {
       |      c = a + b;
       |    }
       |  }
       |}
       |""".stripMargin

  val unaryExpressionSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |  }
       |}
       |""".stripMargin

  val unaryExpressionDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    int x = 0;
       |    -42;
       |  }
       |}
       |""".stripMargin

  val variableDeclaratorSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val variableDeclaratorDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    x@dimensions[] = { 1, 2, 3 };
       |    x@dimensions[];
       |""".stripMargin

  val whileStatementSrcCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |  }
       |}
       |""".stripMargin

  val whileStatementDestCode: String =
    s"""
       |public class Main {
       |  public void myFunc() {
       |    while (1) {
       |      x += 1;
       |    }
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
       |    System.out.println(\"I have been executed!\");
       |  }
       |}
       |""".stripMargin

  val addedNewFunctionModifierSrcCode: String =
    s"""
       |public class Main {
       |  void myMethod() {
       |    System.out.println(\"I just got executed!\");
       |  }
       |}
       |""".stripMargin
  val addedNewFunctionModifierDestCode: String =
    s"""
       |public class Main {
       |  public void myMethod() {
       |    System.out.println(\"I just got executed!\");
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
