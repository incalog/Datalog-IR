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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class FuncIncaResolveTestCase extends FuncIncaCodeInsightTest {
    protected PsiReference referencedElement;
    protected PsiElement resolvedElement;

    public FuncIncaResolveTestCase(String srcName) {
        super(srcName, srcName);
    }

    public FuncIncaResolveTestCase() {
        this("resolve");
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        for (File file : getTestDataFiles(new File(getTestDataPath()))) {
            if (file.isDirectory()) continue;
            String text = FileUtil.loadFile(file, CharsetToolkit.UTF8);
            text = StringUtil.convertLineSeparators(text);
            int referencedOffset = text.indexOf("<ref>");
            text = text.replace("<ref>", "");
            int resolvedOffset = text.indexOf("<resolved>");
            text = text.replace("<resolved>", "");
            String relativePath = file.getCanonicalPath().substring(
                    file.getCanonicalPath().indexOf(getTestDataPath()) + getTestDataPath().length() + 1
            );
            VirtualFile vFile = myFixture.getTempDirFixture().createFile(relativePath, text);
            PsiFile psiFile = myFixture.configureFromTempProjectFile(relativePath);
            if (referencedOffset != -1) {
                referencedElement = psiFile.findReferenceAt(referencedOffset);
                if (referencedElement == null) fail("Reference was null in " + file.getName());
            }
            if (resolvedOffset != -1) {
                final PsiReference ref = psiFile.findReferenceAt(resolvedOffset);
                if (ref == null) { fail("Reference was null in " + file.getName()); }
                resolvedElement = ref.getElement();
                if (resolvedElement == null) { fail("Reference returned null element in " + file.getName()); }
            }
        }
    }

    @Override
    protected String getTestDataPath(){
        return super.getTestDataPath() + getTestName(false).replaceFirst("^\\.(\\\\|/)", "");
    }

    protected Collection<File> getTestDataFiles(File dir){
        List<File> testData = new ArrayList<>();
        for (File entry : dir.listFiles()) {
            if (entry.isDirectory()) {
                getTestDataFiles(entry);
            } else {
                System.out.println(entry.getName());
            }
        }
        return testData;
    }

    protected void doTest() {
        doTest(true);
    }

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
