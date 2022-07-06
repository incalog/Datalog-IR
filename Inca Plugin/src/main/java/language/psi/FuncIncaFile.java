package language.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import language.FuncIncaFileType;
import language.FuncIncaLanguage;
import org.jetbrains.annotations.NotNull;

public class FuncIncaFile extends PsiFileBase{

    public FuncIncaFile(@NotNull FileViewProvider viewProvider){
        super(viewProvider, FuncIncaLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return FuncIncaFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "Functional Inca File";
    }
}
