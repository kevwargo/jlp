package kevwargo.jlp.objects.builtins.functions;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.FormalArguments;

public class Print_F extends LispBuiltinFunction {

    public Print_F() {
        super("print", new FormalArguments().addPositional("obj"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispObject object = parseArgs(basicNamespace, arguments).resolve("obj");
        System.out.println(object.toString());
        return object;
    }
    
}
