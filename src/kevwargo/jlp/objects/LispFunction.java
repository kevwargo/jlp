package kevwargo.jlp.objects;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LispFunction extends LispObject {

    protected String name;
    protected FormalArguments formalArguments;


    public LispFunction(String name, FormalArguments formalArguments) {
        super();
        this.name = name;
        this.formalArguments = formalArguments;
    }

    public LispFunction(LispType type, String name, FormalArguments formalArguments) {
        this(name, formalArguments);
        setType(type);
    }

    public String getName() {
        return name;
    }

    public String toString() {
        return String.format("function '%s' at 0x%x", name, System.identityHashCode(this));
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        HashMap<String, LispObject> argMap = new HashMap<String, LispObject>();
        int al = arguments.getLength();
        int fl = formalArguments.pos().size();
        if (fl > al || (formalArguments.rest() == null && fl < al)) {
            throw new LispException("'%s' requires %d positional arguments, %d provided", name, fl, al);
        }
        for (String argName : formalArguments.pos()) {
            argMap.put(argName, arguments.next());
        }
        if (formalArguments.rest() != null) {
            LispList rest = new LispList();
            while (arguments.hasNext()) {
                rest.add(arguments.next());
            }
            argMap.put(formalArguments.rest(), rest);
        }
        return callInternal(namespace, argMap);
    }


    abstract protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException;

}
