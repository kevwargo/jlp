package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;

public class LispMacro extends LispBuiltinMacro {

    private LispObject[] body;

    public LispMacro(String name, String formalArguments[], boolean allowRest, LispObject[] body) {
        super(name, formalArguments, allowRest);
        evalArgs = false;
        this.body = body;
    }

    public LispObject eval(LispNamespace basicNamespace) throws LispException {
        LispObject result = LispNil.getInstance();
        LispNamespace namespace = basicNamespace.prepend(this.arguments);
        for (LispObject expr : body) {
            result = expr.eval(namespace);
        }
        return result.eval(basicNamespace);
    }
    
}
