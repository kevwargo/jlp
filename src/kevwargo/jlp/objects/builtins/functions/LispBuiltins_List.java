package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;


public class LispBuiltins_List extends LispBuiltinFunction {

    public LispBuiltins_List() {
        super("list", new String[0], true);
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        Sexp result = new Sexp();
        for (LispObject object : rest) {
            result.add(object.eval(namespace));
        }
        return result.size() > 0 ? result : LispNil.getInstance();
    }
    
}
