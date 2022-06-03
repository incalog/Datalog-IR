package language.psi;

import com.intellij.psi.tree.IElementType;
import language.FuncIncaLanguage;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class FuncIncaElementType extends IElementType {

    public FuncIncaElementType(@NotNull @NonNls String debugName) {
        super(debugName, FuncIncaLanguage.INSTANCE);
    }

}
