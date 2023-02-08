package language.types;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import kotlin.Pair;
import language.psi.FuncIncaDataDef;
import language.psi.FuncIncaDecl;
import language.psi.FuncIncaParamType;
import language.psi.FuncIncaVar;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

public class TypeContext {
    private static TypeContext instance = new TypeContext();
    private static Map<String, Pair<FuncIncaDecl, FuncIncaType>> vars; // name -> (declaration, type)
    private static Map<String, FuncIncaParamType> tyVars; // name -> declaration
    private static Map<String, FuncIncaType> funDefs; //
    private static Map<String, FuncIncaDataDef> dataDefs; // name -> declaration

    private TypeContext() {
        vars = new HashMap<>();
        tyVars = new HashMap<>();
        funDefs = new HashMap<>();
        dataDefs = new HashMap<>();
    }

    public TypeContext getInstance() {
        return instance;
    }

    // ------------------------------------- context stuff ------------------------------------

    private static <A, B> Map<A, B> copyMap(Map<A, B> map) {
        Gson gson = new Gson();
        String jsonString = gson.toJson(map);
        Type type = new TypeToken<HashMap<A, B>>(){}.getType();
        return gson.fromJson(jsonString, type);
    }

    public static void scopedTypeContext(Runnable runnable) {
        Map<String, Pair<FuncIncaDecl, FuncIncaType>> varsSaved = copyMap(vars);
        Map<String, FuncIncaParamType> tyVarsSaved = copyMap(tyVars);
        Map<String, FuncIncaType> funDefsSaved = copyMap(funDefs);
        Map<String, FuncIncaDataDef> dataDefsSaved = dataDefs;

        runnable.run();

        /*


action(new Runnable(){
    void run(){
        System.out.println("Hello");
    }
});
        * */

        vars = varsSaved;
        tyVars = tyVarsSaved;
        funDefs = funDefsSaved;
        dataDefs = dataDefsSaved;
    }

    public static void bindVar(String name, FuncIncaDecl decl, FuncIncaType type, AnnotationHolder holder) {
        Pair<FuncIncaDecl, FuncIncaType> prevDecl = vars.put(name, new Pair<>(decl, type));
        if (prevDecl != null) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                            "Type Variable " + name + " shadows previously defined variable")
                    .range(decl)
                    .create();
            holder.newAnnotation(HighlightSeverity.ERROR,
                            "Variable " + name + " shadowed by later declaration")
                    .range(prevDecl.getFirst())
                    .create();
        }
    }

    public static Pair<FuncIncaDecl, FuncIncaType> lookupVar(String name, FuncIncaVar var, AnnotationHolder holder) {
        Pair<FuncIncaDecl, FuncIncaType> v = vars.get(name);
        if (var == null) {
            // TODO case None =>
            //        funs.get(name) match {
            //          case set if set.size == 1 =>
            //            Some(set.head._2)
            //          case set if set.size >= 2 =>
            //            val modules = set.toSeq.map(_._1)
            //            val modulesStr = modules.map(_.name).mkString(", ")
            //            error(s"Ambiguous call to $name, found definitions in $modulesStr", (name +: modules): _*)
            //            None
            //          case _ =>
            //            error(s"Unbound variable $name", name)
            //            None
            holder.newAnnotation(HighlightSeverity.ERROR,
                            "Unbound variable " + name)
                    .range(var)
                    .create();
        }
        return v;
    }

    public boolean isFreeVar(String name) {
        return !vars.containsKey(name);
    }

    public void bindTyVar(String name, FuncIncaParamType decl, AnnotationHolder holder) {
        PsiElement prevDecl = tyVars.put(name, decl);
        if (prevDecl != null) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                    "Type Variable " + name + " shadows previously defined Type variable")
                    .range(decl)
                    .create();
            holder.newAnnotation(HighlightSeverity.ERROR,
                    "Type Variable " + name + " shadowed by later declaration")
                    .range(prevDecl)
                    .create();
        }
    }

    public FuncIncaParamType lookupTyVar(String name) {
        return tyVars.get(name);
    }

    public boolean isTypeVar(String name) {
        return tyVars.containsKey(name);
    }


    /*
    * def bindFun(fun: FunctionDef, module: Module): Unit = {
    funs += fun.name -> (module, (fun, fun.funType))
  }

  def lookupCalled(name: Name): Option[(Var.Target, TFun)] =
    funs.get(name) match {
      case set if set.size == 1 =>
        Some(set.head._2)
      case set if set.size >= 2 =>
        val modules = set.toSeq.map(_._1)
        val modulesStr = modules.map(_.name).mkString(", ")
        error(s"Ambiguous call to $name, found definitions in $modulesStr", (name +: modules): _*)
        None
      case set if set.isEmpty => vars.get(name) match {
        case Some((trg, ty: TFun)) =>
          Some((trg, ty))
        case Some((_, ty)) =>
          error(s"Variable $name has type $ty, but required function type")
          None
        case None =>
          error(s"Unbound name $name", name)
          None
      }

    }

    * */


    public void bindData(FuncIncaDataDef data) {
        String dataName = data.getName();
        dataDefs.put(dataName, data); // list of constructors hier in Map?
        // data.constrs.foreach(c => funs += c.name -> (module, (c, c.constructorType(data))))
    }

    public boolean isData(String name) {
        return dataDefs.containsKey(name);
    }

    public boolean isDataOrTypeVar(String name) {
        return dataDefs.containsKey(name) || tyVars.containsKey(name);
    }

    public FuncIncaDataDef lookupData(String name) {
        return dataDefs.get(name);
    }

}
