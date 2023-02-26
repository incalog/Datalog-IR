package language.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import language.FunIncAReference;
import language.psi.*;


public class FunIncAPsiImplUtil {

    private static String getName(ASTNode node){
        if(node != null)
            return node.getText();
        else
            return null;
    }

    private static PsiElement getNameIdentifier(ASTNode node){
        if (node != null) {
            return node.getPsi();
        } else {
            return null;
        }
    }

    public static PsiReference getReference(PsiElement element){
        return new FunIncAReference(element, element.getTextRange());
    }

    // -------------------------------- var ------------------------------------
    public static String getName(FunIncAVarRefExp element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncAVarRefExp element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncAVarRefExp var = FunIncAElementFactory.createVar(element.getProject(), newName);
            ASTNode newNode = var.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncAVarRefExp element) {
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // ------------------------------- param -----------------------------------
    public static String getName(FunIncAParamDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncAParamDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncAParamDef paramId = FunIncAElementFactory.createParam(element.getProject(), newName);
            ASTNode newNode = paramId.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncAParamDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // ------------------------------- import ----------------------------------
    public static String getName(FunIncAImport element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncAImport element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncAImport imp = FunIncAElementFactory.createImport(element.getProject(), newName);
            ASTNode newNode = imp.getFirstChild().getNextSibling().getNode(); // first child is keyword import, second child is id
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncAImport element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // --------------------------------- fun_def ------------------------------------
    public static String getName(FunIncAFunDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncAFunDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncAFunDef funDef = FunIncAElementFactory.createFunDef(element.getProject(), newName);
            ASTNode newNode = funDef.getFirstChild().getNextSibling().getNode(); // first child is keyword_def, next silbling is id
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncAFunDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // --------------------------------- type_variables ---------------------------------

    public static String getName(FunIncATypeVarDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncATypeVarDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncATypeVarDef paramType = FunIncAElementFactory.createTypeVariable(element.getProject(), newName);
            ASTNode newNode = paramType.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncATypeVarDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // --------------------------------- data_def -------------------------------------
    public static String getName(FunIncADataDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncADataDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncADataDef dataDef = FunIncAElementFactory.createDataDef(element.getProject(), newName);
            ASTNode newNode = dataDef.getFirstChild().getNextSibling().getNode(); // first child is keyword_data, next silbling is id
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncADataDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // ------------------------------------- data_constructor ------------------------------
    public static String getName(FunIncADataConstructorDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncADataConstructorDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncADataConstructorDef dataCons = FunIncAElementFactory.createDataConstructor(element.getProject(), newName);
            ASTNode newNode = dataCons.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncADataConstructorDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // ----------------------------- cons_pattern_id ---------------------------------
    public static String getName(FunIncAPatternVarDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncAPatternVarDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncAPatternVarDef cpi = FunIncAElementFactory.createConsPatternId(element.getProject(), newName);
            ASTNode newNode = cpi.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncAPatternVarDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // ----------------------------- constructor_pattern -----------------------------
    public static String getName(FunIncAConstructorPat element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncAConstructorPat element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncAConstructorPat consPattern = FunIncAElementFactory.createConstructorPattern(element.getProject(), newName);
            ASTNode newNode = consPattern.getFirstChild().getNode(); // first node is the name of the called constructor
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncAConstructorPat element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    // -------------------------------- var_id ---------------------------------------
    public static String getName(FunIncAVarDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FunIncAVarDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        if(idNode != null){
            FunIncAVarDef varId = FunIncAElementFactory.createVarId(element.getProject(), newName);
            ASTNode newNode = varId.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FunIncAVarDef element){
        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
        return getNameIdentifier(idNode);
    }

    //--------------------------------- type_name -------------------------------------
//
//    public static String getName(FunIncATypeName element){
//        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
//        return getName(idNode);
//    }
//
//    public static PsiElement setName(FunIncATypeName element, String newName){
//        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
//        if(idNode != null){
//            FunIncATypeName typeName = FunIncAElementFactory.createTypeName(element.getProject(), newName);
//            ASTNode newNode = typeName.getFirstChild().getNode();
//            element.getNode().replaceChild(idNode, newNode);
//        }
//        return element;
//    }
//
//    public static PsiElement getNameIdentifier(FunIncATypeName element){
//        ASTNode idNode = element.getNode().findChildByType(FunIncATypes.ID);
//        return getNameIdentifier(idNode);
//    }
//
}
