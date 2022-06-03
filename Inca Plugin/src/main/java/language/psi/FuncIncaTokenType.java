package language.psi;

import com.intellij.psi.tree.IElementType;
import language.FuncIncaLanguage;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class FuncIncaTokenType extends IElementType {

    public FuncIncaTokenType(@NotNull @NonNls String debugName) {
        super(debugName, FuncIncaLanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "FuncIncaTokenType." + super.toString();
    }

}