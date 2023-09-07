package language;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class FunIncAFileType extends LanguageFileType {

    public static final FunIncAFileType INSTANCE = new FunIncAFileType();

    private FunIncAFileType() {
        super(FunIncALanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Functional IncA File";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Functional IncA language file";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "Functional IncA";
    }

    @Nullable
    @Override
    public Icon getIcon() {
        return FunIncAIcons.FILE;
    }

}