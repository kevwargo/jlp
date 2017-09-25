package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.FormalArguments;


public class Quote_M extends LispBuiltinMacro {

    public Quote_M() {
        super("quote", new FormalArguments().addPositional("obj"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        return parseArgs(basicNamespace, arguments).resolve("obj");
    }
    
}
