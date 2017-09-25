package kevwargo.jlp.objects.builtins.functions;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.FormalArguments;


public class Eval_F extends LispBuiltinFunction {

    public Eval_F() {
        super("eval", (new FormalArguments()).addPositional("object"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        return parseArgs(basicNamespace, arguments).resolve("object").eval(basicNamespace);
    }
    
}
