package language.resolve;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import language.FuncIncaCodeInsightTest;

import java.io.File;
import java.util.Collection;

public abstract class FuncIncaResolveTestCase extends FuncIncaCodeInsightTest {
    protected PsiElement resolvedElement;
    protected PsiReference referencedElement;

    public FuncIncaResolveTestCase(String srcName) {
        super(srcName, srcName);
    }

    public FuncIncaResolveTestCase(){
        this("resolve");
    }

    protected void doTest() { doTest(true); }

    protected void doTest(boolean succeed) {
        if (succeed && referencedElement == null) { fail("Could not find reference at caret."); }
        if (succeed && resolvedElement == null) { fail("Could not find resolved element."); }
        if (succeed) {
            PsiElement resolvedActual = referencedElement.resolve();
            assertEquals(
                    "Could not resolve expected reference.\n" +
                            "Expected: " + resolvedElement + " (" + resolvedElement.getText() + ")\n" +
                            "Actual: " + resolvedActual + " (" + resolvedActual.getText() + ")",
                    resolvedElement,
                    resolvedActual
            );
        } else {
            assertFalse("Resolved unexpected reference.", resolvedElement.equals(referencedElement.resolve()));
        }
    }

}
