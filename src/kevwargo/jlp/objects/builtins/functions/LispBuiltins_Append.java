package kevwargo.jlp.objects.builtins.functions;

import java.util.ArrayList;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;


public class LispBuiltins_Append extends LispBuiltinFunction {

    public LispBuiltins_Append() {
        super("append", new FormalArguments(new ArrayList<String>(), "args"));
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        Sexp result = Sexp.getInstance();
        Iterator<LispObject> argsIterator = ((Sexp)arguments.get("args")).iterator();
        while (argsIterator.hasNext()) {
            LispObject object = argsIterator.next();
            if (!(object instanceof Sexp)) {
                throw new LispException("Only lists can be appended");
            }
            Iterator<LispObject> iterator = ((Sexp)object).iterator();
            while (iterator.hasNext()) {
                result = result.add(iterator.next());
            }
        }
        return result;
    }
    
}
