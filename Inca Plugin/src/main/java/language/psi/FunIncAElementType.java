package language.psi;

import com.intellij.psi.tree.IElementType;
import language.FunIncALanguage;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class FunIncAElementType extends IElementType {

    public FunIncAElementType(@NotNull @NonNls String debugName) {
        super(debugName, FunIncALanguage.INSTANCE);
    }

}
