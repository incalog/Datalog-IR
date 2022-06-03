package language;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class FuncIncaFileType extends LanguageFileType {

    public static final FuncIncaFileType INSTANCE = new FuncIncaFileType();

    private FuncIncaFileType() {
        super(FuncIncaLanguage.INSTANCE);
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
        return FuncIncaIcons.FILE;
    }

}