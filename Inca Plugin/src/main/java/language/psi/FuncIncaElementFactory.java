package language.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import language.FuncIncaFileType;

public class FuncIncaElementFactory {
    /*
    * Methods take a name and return a PSI node with that name or null
    * */
    public static FuncIncaVar createVar(Project project, String name){
        return ((FuncIncaVar) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaParam createParam(Project project, String name){
        return ((FuncIncaParam) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaImport createImport(Project project, String name){
        return ((FuncIncaImport) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaFunDef createFunDef(Project project, String name){
        return ((FuncIncaFunDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaTypeVariable createTypeVariable(Project project, String name){
        return ((FuncIncaTypeVariable) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaDataDef createDataDef(Project project, String name){
        return ((FuncIncaDataDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaDataConstructor createDataConstructor(Project project, String name){
        return ((FuncIncaDataConstructor) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaConsPatternId createConsPatternId(Project project, String name){
        return ((FuncIncaConsPatternId) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaConstructorPattern createConstructorPattern(Project project, String name){
        return ((FuncIncaConstructorPattern) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaVarId createVarId(Project project, String name){
        return ((FuncIncaVarId) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaTypeName createTypeName(Project project, String name){
        return ((FuncIncaTypeName) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    // TODO zweck dieser methoden?
    public static PsiElement createExpressionFromText(Project project, String name) {
        FuncIncaFile fileFromText = createFileFromText(project, name);
        PsiElement rhs = fileFromText.getFirstChild().getFirstChild().getLastChild();
        PsiElement nodeOfInterest = rhs.getLastChild().getLastChild().getLastChild();
        return nodeOfInterest;
    }

    public static FuncIncaFile createFileFromText(Project project, String text) {
        String name = "dummy.finca";
        return (FuncIncaFile) PsiFileFactory.getInstance(project).createFileFromText(name, FuncIncaFileType.INSTANCE, text);
    }
}
