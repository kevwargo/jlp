package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispObject;


public class LispBuiltins_Quote extends LispBuiltinMacro {

    public LispBuiltins_Quote() {
        super("quote", new String[] {"arg"}, false);
    }

    public LispObject eval(LispNamespace namespace) {
        return arguments.get("arg");
    }
    
}
