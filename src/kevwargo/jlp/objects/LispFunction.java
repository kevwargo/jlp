package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;

import java.util.HashMap;
import java.util.Map;

public abstract class LispFunction extends LispBaseObject implements LispCallable, LispNamedObject {

    protected String name;
    protected FormalArguments formalArguments;

    public LispFunction(String name, FormalArguments formalArguments) {
        this(LispType.FUNCTION, name, formalArguments);
    }

    public LispFunction(LispType type, String name, FormalArguments formalArguments) {
        super(type);
        this.name = name;
        this.formalArguments = formalArguments;
    }

    public String getName() {
        return name;
    }

    public FormalArguments getFormalArgs() {
        return formalArguments;
    }

    public String repr() {
        return String.format("function '%s' at 0x%x", name, System.identityHashCode(this));
    }

    public boolean bool() {
        return true;
    }

    public LispObject call(LispRuntime runtime, ArgumentsIterator arguments) throws LispException {
        Map<String, LispObject> argMap = new HashMap<String, LispObject>();
        int al = arguments.getLength();
        int min = formalArguments.pos().size();
        int max = min + formalArguments.opt().size();
        if (al < min || (formalArguments.rest() == null && al > max)) {
            throw new LispException(
                    "'%s' requires [%d..%d] positional arguments, %d provided", name, min, max, al);
        }
        for (String argName : formalArguments.pos()) {
            argMap.put(argName, arguments.next());
        }
        if (arguments.hasNext()) {
            for (String argName : formalArguments.opt()) {
                argMap.put(argName, arguments.next());
            }
        }
        if (formalArguments.rest() != null) {
            LispList rest = new LispList();
            while (arguments.hasNext()) {
                rest.add(arguments.next());
            }
            argMap.put(formalArguments.rest(), rest);
        }
        return callInternal(runtime, argMap);
    }

    protected abstract LispObject callInternal(
            LispRuntime runtime, Map<String, LispObject> arguments) throws LispException;
}

class FunctionType extends LispType {

    FunctionType(String name) {
        super(name, new LispType[] {OBJECT});
    }

    FunctionType(String name, LispType base) {
        super(name, new LispType[] {base});
    }

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
        throw new LispException("Cannot instantiate '%s'", getName());
    }
}
