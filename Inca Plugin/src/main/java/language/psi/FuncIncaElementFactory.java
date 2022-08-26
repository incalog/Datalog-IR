package language.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import language.FuncIncaFileType;

public class FuncIncaElementFactory {
    /*
    * Methods take a name and return a PSI node with that name or null
    * */
    public static FuncIncaImport createImport(Project project, String name){
        return ((FuncIncaImport) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaFunDef createFunDef(Project project, String name){
        return ((FuncIncaFunDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaParamType createParamType(Project project, String name){
        return ((FuncIncaParamType) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaDataDef createDataDef(Project project, String name){
        return ((FuncIncaDataDef) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaDataConstructor createDataConstructor(Project project, String name){
        return ((FuncIncaDataConstructor) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    public static FuncIncaVarId createVarId(Project project, String name){
        return ((FuncIncaVarId) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
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
