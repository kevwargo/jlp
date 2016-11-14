package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.FormalArguments;


public class LispBuiltins_Quote extends LispBuiltinMacro {

    public LispBuiltins_Quote() {
        super("quote", new FormalArguments().addPositional("obj"));
    }

    public LispObject eval(LispNamespace namespace) {
        return arguments.get("obj");
    }
    
}
