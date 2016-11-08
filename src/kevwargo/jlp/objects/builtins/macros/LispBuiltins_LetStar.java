package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispObject;


public class LispBuiltins_LetStar extends LispBuiltins_Let {

    public LispBuiltins_LetStar() {
        super("let*");
    }

    protected LispNamespace getVarValNamespace(LispNamespace namespace, HashMap<String, LispObject> prevDefs) {
        return namespace.prepend(prevDefs);
    }
    
}
