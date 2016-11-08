package kevwargo.jlp.objects;

import java.util.HashMap;
import java.util.List;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;

public class LispFunction extends LispBuiltinFunction {

    private LispObject[] body;

    public LispFunction(String name, String[] formalArguments, boolean allowRest, LispObject[] body) {
        super(name, formalArguments, allowRest);
        this.body = body;
    }

    public LispObject eval(LispNamespace basicNamespace) throws LispException {
        LispObject result = LispNil.getInstance();
        LispNamespace namespace = basicNamespace.prepend(this.arguments);
        for (LispObject expr : body) {
            result = expr.eval(namespace);
        }
        return result;
    }
    
}
