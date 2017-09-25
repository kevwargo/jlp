package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.FormalArguments;

public class Print_F extends LispBuiltinFunction {

    public Print_F() {
        super("print", new FormalArguments().addPositional("obj"));
    }

    public LispObject eval(LispNamespace namespace) {
        System.out.println(arguments.get("obj").toString());
        return arguments.get("obj");
    }
    
}
