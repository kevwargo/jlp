package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.FormalArguments;


public class Quote_M extends LispBuiltinMacro {

    public Quote_M() {
        super("quote", new FormalArguments().addPositional("obj"));
    }

    public LispObject eval(LispNamespace namespace) {
        return arguments.get("obj");
    }
    
}
