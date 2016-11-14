package kevwargo.jlp.objects;

import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

public abstract class LispBuiltinMacro extends LispBuiltinFunction {

    public LispBuiltinMacro(String name, FormalArguments formalArguments) {
        super(name, formalArguments);
    }

    protected LispObject evalArg(LispObject arg, LispNamespace namespace) {
        return arg;
    }
    
}
