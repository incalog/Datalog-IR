package language.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import language.FunIncAFileType;
import language.FunIncALanguage;
import org.jetbrains.annotations.NotNull;

public class FunIncAFile extends PsiFileBase{

    public FunIncAFile(@NotNull FileViewProvider viewProvider){
        super(viewProvider, FunIncALanguage.INSTANCE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return FunIncAFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "Functional Inca File";
    }
}
