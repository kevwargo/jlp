package kevwargo.jlp.objects.builtins.functions;

import java.util.ArrayList;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;


public class List_F extends LispBuiltinFunction {

    public List_F() {
        super("list", new FormalArguments(new ArrayList<String>(), "args"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        Sexp result = Sexp.getInstance();
        for (LispObject object : (Sexp)parseArgs(basicNamespace, arguments).resolve("args")) {
            result = result.add(object);
        }
        return result;
    }
    
}
