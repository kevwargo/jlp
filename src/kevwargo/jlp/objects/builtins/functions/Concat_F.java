package kevwargo.jlp.objects.builtins.functions;

import java.util.ArrayList;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class Concat_F extends LispBuiltinFunction {

    public Concat_F() {
        super("concat", new FormalArguments(new ArrayList<String>(), "args"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        String result = "";
        for (LispObject object : (Sexp)parseArgs(basicNamespace, arguments).resolve("args")) {
            object.assertType("string");
            result += ((LispString)object).getValue();
        }
        return new LispString(result);
    }
    
}
