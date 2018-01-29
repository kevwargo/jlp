package kevwargo.jlp.objects;

import java.util.HashMap;
import java.util.Iterator;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LispFunction extends LispObject {

    protected String name;
    protected FormalArguments formalArguments;


    public LispFunction(LispType type, String name, FormalArguments formalArguments) {
        super(type);
        this.name = name;
        this.formalArguments = formalArguments;
    }

    public String getName() {
        return name;
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        HashMap<String, LispObject> argMap = new HashMap<String, LispObject>();
        Iterator<String> formal = formalArguments.pos().iterator();
        int argCount = 0;
        while (formal.hasNext() && arguments.hasNext()) {
            argMap.put(formal.next(), arguments.next());
            argCount++;
        }
        if (formal.hasNext()) {
            throw new LispException(String.format("Too few arguments for %s: %d", name, argCount));
        }
        if (formalArguments.rest() != null) {
            LispList rest = new LispList();
            while (arguments.hasNext()) {
                rest.add(arguments.next());
            }
            argMap.put(formalArguments.rest(), rest);
        } else if (arguments.hasNext()) {
            while (arguments.hasNext()) {
                arguments.next();
                argCount++;
            }
            throw new LispException(String.format("Too many arguments for %s: %d", name, argCount));
        }

        return callInternal(namespace, argMap);
    }


    abstract protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException;

}
