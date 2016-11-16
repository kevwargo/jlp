package kevwargo.jlp.objects.builtins.functions;

import java.util.ArrayList;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class Concat_F extends LispBuiltinFunction {

    public Concat_F() {
        super("concat", new FormalArguments(new ArrayList<String>(), "args"));
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        String result = "";
        Iterator<LispObject> argsIterator = ((Sexp)arguments.get("args")).iterator();
        while (argsIterator.hasNext()) {
            LispObject object = argsIterator.next();
            if (!(object instanceof LispString)) {
                throw new LispException("Wrong argument type: string expected");
            }
            result += ((LispString)object).getValue();
        }
        return new LispString(result);
    }
    
}
