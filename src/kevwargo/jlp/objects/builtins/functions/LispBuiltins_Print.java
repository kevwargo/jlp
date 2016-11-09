package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;

public class LispBuiltins_Print extends LispBuiltinFunction {

    public LispBuiltins_Print() {
        super("print", new String[0], true);
    }

    public LispObject eval(LispNamespace namespace) {
        for (LispObject object : rest) {
            System.out.println(object.toString());
        }
        return LispNil.getInstance();
    }
    
}
