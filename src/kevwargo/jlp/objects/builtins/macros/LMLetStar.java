package kevwargo.jlp.objects.builtins.macros;

import java.util.Map;

import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispObject;


public class LMLetStar extends LMLet {

    public LMLetStar() {
        super("let*");
    }

    protected LispNamespace getVarValNamespace(LispNamespace namespace, Map<String, LispObject> prevDefs) {
        return namespace.prepend(prevDefs);
    }
    
}