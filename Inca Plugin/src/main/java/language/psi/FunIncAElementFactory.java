package language.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import language.FunIncAFileType;

public class FunIncAElementFactory {
    /*
    * Methods take a name and return a PSI node with that name or null
    * */
    public static FunIncAVarRefExp createVar(Project project, String name){
        return ((FunIncAVarRefExp) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncAParamDef createParam(Project project, String name){
        return ((FunIncAParamDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncAImport createImport(Project project, String name){
        return ((FunIncAImport) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncAFunDef createFunDef(Project project, String name){
        return ((FunIncAFunDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncATypeVarDef createTypeVariable(Project project, String name){
        return ((FunIncATypeVarDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncADataDef createDataDef(Project project, String name){
        return ((FunIncADataDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncADataConstructorDef createDataConstructor(Project project, String name){
        return ((FunIncADataConstructorDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncAPatternVarDef createConsPatternId(Project project, String name){
        return ((FunIncAPatternVarDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncAConstructorPat createConstructorPattern(Project project, String name){
        return ((FunIncAConstructorPat) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncAVarDef createVarId(Project project, String name){
        return ((FunIncAVarDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FunIncATypeNameRef createTypeName(Project project, String name){
        return ((FunIncATypeNameRef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    // TODO zweck dieser methoden?
    public static PsiElement createExpressionFromText(Project project, String name) {
        FunIncAFile fileFromText = createFileFromText(project, name);
        PsiElement rhs = fileFromText.getFirstChild().getFirstChild().getLastChild();
        PsiElement nodeOfInterest = rhs.getLastChild().getLastChild().getLastChild();
        return nodeOfInterest;
    }

    public static FunIncAFile createFileFromText(Project project, String text) {
        String name = "dummy.finca";
        return (FunIncAFile) PsiFileFactory.getInstance(project).createFileFromText(name, FunIncAFileType.INSTANCE, text);
    }
}
