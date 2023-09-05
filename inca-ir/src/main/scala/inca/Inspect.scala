//package inca
//
////import dotty.tools.dotc.ast.untpd.{Ident, Literal, Select}
//import dotty.tools.dotc
//import dotty.tools.dotc.core.Names.Name
//import dotty.tools.dotc.semanticdb.Descriptor.Term
//
//import scala.annotation.experimental
//import scala.meta.{Defn, Dialect, XtensionQuasiquoteTerm}
//
////import dotty.tools.dotc.core.Decorators.toTermName
////import dotty.tools.dotc.core.Constants.Constant
//import dotty.tools.dotc.util.SourceFile
//
//import scala.quoted.*
//
//// make available the necessary compiler for runtime code generation
//given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
//
////case class Constant[T](value: T)
//
//def foo(): String = "foo"
//
//object Inspect {
//  import scala.meta
//
//  /*inline def transTree(tree: meta.Tree)(using Quotes): quotes.reflect.Symbol = {
//    tree match
//      case m: meta.Method =>
//        println("Translate method...")
//        quotes.reflect.noSymbol
//  }*/
//
//  /*@experimental
//  def transTemplate(templ: meta.Template)(using Quotes): List[quotes.reflect.Definition] = {
//    templ.stats.map(s => transStat(s))
//  }
//
//  @experimental
//  def transClass(defnCls: meta.Defn.Class)(using Quotes): quotes.reflect.ClassDef = {
//    val methods = defnCls.templ.stats.flatMap {
//      case m: meta.Decl.Def => Some(m)
//      case _ => None
//    }
//
//    def decls(cls: quotes.reflect.Symbol): List[quotes.reflect.Symbol] = {
//      methods.map { m =>
//        val mName = m.name.value
//        val mType = quotes.reflect.MethodType(Nil)(_ => Nil, _ => quotes.reflect.TypeRepr.of[Unit])
//        quotes.reflect.Symbol.newMethod(cls, mName, mType)
//      }
//    }
//
//    val parents = List(quotes.reflect.TypeTree.of[Object])
//    val clsSymbol = quotes.reflect.Symbol.newClass(
//      quotes.reflect.Symbol.spliceOwner,
//      defnCls.name.value,
//      parents = parents.map(_.tpe),
//      decls,
//      None
//    )
//
//    //given clsSymbol
//    val defs = transTemplate(defnCls.templ)
//    println(s"The defs are: $defs")
//    quotes.reflect.ClassDef(clsSymbol, parents, body = defs)
//  }
//
//  inline def transDef(defnDef: meta.Defn.Def)(using Quotes): quotes.reflect.Definition = {
//    println(s"Symbol:  ${quotes.reflect.Symbol.requiredClass("Main").declaredMethod("test")}")
//    //val defSymbol = clsSymbol.declaredMethod(defnDef.name.value).head
//    val defSymbol = quotes.reflect.Symbol.spliceOwner
//    quotes.reflect.DefDef(defSymbol, argss => None)
//  }
//
//  @experimental
//  def transStat(stat: meta.Stat)(using Quotes): quotes.reflect.Definition = stat match {
//    case s: meta.Defn.Class => transClass(s)
//    case d: meta.Defn.Def => transDef(d)
//    case _ => throw new IllegalArgumentException(s"Can not convert stat\n $stat : ${stat.getClass}")
//  }*/
//
//  def runScala3(code: String): Any = {
//    import dotty.tools.io.AbstractFile
//    import dotty.tools.dotc.core.Contexts.Context
//    import dotty.tools.dotc.Driver
//    import dotty.tools.dotc.util.SourceFile
//    import dotty.tools.io.{VirtualDirectory, VirtualFile}
//    import java.net.URLClassLoader
//    import java.nio.charset.StandardCharsets
//    import dotty.tools.repl.AbstractFileClassLoader
//    import scala.io.Codec
//
//    def compileCode(
//                     code: String,
//                     classpathDirectories: List[AbstractFile],
//                     outputDirectory: AbstractFile
//                   ): Unit = {
//      class DriverImpl extends Driver {
//        private val compileCtx0 = initCtx.fresh
//
//        given Context = compileCtx0.fresh
//          .setSetting(
//            compileCtx0.settings.classpath,
//            classpathDirectories.map(_.path).mkString(":")
//          ).setSetting(
//          compileCtx0.settings.usejavacp,
//          true
//        ).setSetting(
//          compileCtx0.settings.outputDir,
//          outputDirectory
//        )
//
//        val compiler: dotc.Compiler = newCompiler
//      }
//
//      val driver = new DriverImpl
//      import driver.given Context
//
//      val sourceFile = SourceFile(VirtualFile("(inline)", code.getBytes(StandardCharsets.UTF_8)), Codec.UTF8)
//      val run = driver.compiler.newRun
//      run.compileSources(List(sourceFile))
//    }
//
//    def runObjectMethod(
//                         objectName: String,
//                         classLoader: ClassLoader,
//                         methodName: String,
//                         paramClasses: Seq[Class[?]],
//                         arguments: Any*
//                       ): Any = {
//      val clazz = Class.forName(s"$objectName$$", true, classLoader)
//      val module = clazz.getField("MODULE$").get(null)
//      val method = module.getClass.getMethod(methodName, paramClasses *)
//      method.invoke(module, arguments *)
//    }
//
//    val outputDirectory = VirtualDirectory("(memory)")
//    compileCode(code, List() /*files.map(f => AbstractFile.getFile(f.toURI.toURL.getPath)).toList*/ , outputDirectory)
//    val classLoader = AbstractFileClassLoader(outputDirectory, this.getClass.getClassLoader /*depClassLoader*/)
//    runObjectMethod("mypackage.Main", classLoader, "main", Seq(classOf[Array[String]]), Array.empty[String])
//  }
//
//  // Still working even in Scala3 to run Scala2 code
//  def runScala2(code: String): Any = {
//    import scala.tools.reflect.ToolBox // implicit
//
//    val tb = scala.reflect.runtime.universe
//      .runtimeMirror(getClass.getClassLoader)
//      .mkToolBox()
//    val tree = tb.parse(code)
//    val compiled = tb.compile(tree)
//    compiled()
//  }
//
//  //@experimental
//  def test_parse(): Any = {
//    import scala.meta.Source
//    import meta.XtensionParseInputLike
//    // This allows us to parse Scala3 source code
//    //import scala.meta.dialects.Scala3
//
//    // Important: No package + write main method in module body
//    val program2 =
//      """
//        |class Main {
//        | def test(): String = {
//        |   println("Hello, World!")
//        |   return "ok"
//        | }
//        |}
//        |val m = new Main()
//        |m.test()
//        |
//        | """.stripMargin
//    //val tree = program.parse[Source].get
//
//    // Important: Include package and do not write code in the module body
//    val program3 =
//      s"""
//         |package mypackage
//         |
//         |object Main {
//         |  def main(args: Array[String]): Any = {
//         |    println("Hello, World!")
//         |    return "ok"
//         |  }
//         |}""".stripMargin
//
//    val scala2 = runScala2(program2)
//    val scala3 = runScala3(program3)
//
//    s"scala2: $scala2  && scala3: $scala3"
//
//    /*val apl: (Int, String) => Any = staging.run {
//      import quotes.reflect.*
//
//      println(tree.stats.map { s => transStat(s) })
//
//      val addSuffix: Expr[(Int, String) => Any] = '{
//        (y: Int, suffix: String) =>
//          $ {
//            Apply(Select.unique('{ y }.asTerm, "toString"), List()).asExprOf[String]
//          } + "_Hello_there"
//      }
//      addSuffix
//    }
//
//    apl.apply(3, "blub")*/
//
//  }
//
//  def test(): Any = {
//
//    // Not possible to create function as expr...
//    // https://stackoverflow.com/questions/67870834/scala-3-create-expr-int-int
//
//    val x: Int = 4
//    //val closure = '{ (x: Int) => x + 1 }
//    //val c = Constant(x)
//    val fun = "toString"
//
//    //val toStringAST = Select(Ident("y".toTermName), "toString".toTermName)
//    //toStringAST.asExpr[Fun]
//    //val src = new SourceFile(null, null)
//
//    /*val apl: (Int, String) => String = staging.run {
//      import quotes.reflect.*
//      val stringAST = Literal(StringConstant("Hello"))
//
//      val addSuffix: Expr[(Int, String) => String] = '{
//        (y: Int, suffix: String) => $ { stringAST.asExprOf[String] } + "_" + suffix
//      }
//      //val tree: Tree = addSuffix.asTerm
//      //val res = tree.show(using Printer.TreeStructure)
//      addSuffix
//    }
//    apl.apply(x, "test")*/
//
//    // Call function
//    /*val apl: (Int, String) => Any = staging.run {
//      import quotes.reflect.*
//
//      val addSuffix: Expr[(Int, String) => Any] = '{
//        (y: Int, suffix: String) => $ {
//          //Apply(Select('{y}.asTerm, Symbol.requiredMethod(s"foo")), List()).asExprOf[String]
//          Apply(Ref(Symbol.requiredMethod(s"inca.foo")), List()).asExprOf[String]
//        }
//      }
//      //val tree: Tree = addSuffix.asTerm
//      //val res = tree.show(using Printer.TreeStructure)
//      addSuffix
//    }*/
//
//    // Call method
//    val apl: (Int, String) => Any = staging.run {
//      import quotes.reflect.*
//
//      val addSuffix: Expr[(Int, String) => Any] = '{
//        (y: Int, suffix: String) =>
//          $ {
//            Apply(Select.unique('{y}.asTerm, "toString"), List()).asExprOf[String]
//          } + "_Hello_there"
//      }
//      addSuffix
//    }
//
//    apl.apply(3, "blub")
//  }
//}
