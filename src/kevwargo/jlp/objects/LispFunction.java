package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;

import java.util.HashMap;
import java.util.Map;

public abstract class LispFunction extends LispObject {

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

    public FormalArguments getFormalArguments() {
        return formalArguments;
    }

    public String repr() {
        return String.format("function '%s' at 0x%x", name, System.identityHashCode(this));
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments)
            throws LispException {
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
        return callInternal(namespace, argMap);
    }

    protected abstract LispObject callInternal(
            LispNamespace namespace, Map<String, LispObject> arguments) throws LispException;
}

class FunctionType extends LispType {

    FunctionType(String name) {
        super(name, new LispType[] {OBJECT});
    }

    FunctionType(String name, LispType base) {
        super(name, new LispType[] {base});
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments)
            throws LispException {
        LispObject callable = arguments.next();

        String name = "<anonymous>";
        FormalArguments formalArguments = new FormalArguments();
        try {
            LispFunction func = (LispFunction) callable.cast(LispType.FUNCTION);
            name = func.getName();
            formalArguments = func.getFormalArguments();
        } catch (LispCastException e) {
        }

        return new WrappedFunction(callable, this, name, formalArguments);
    }
}

class WrappedFunction extends LispFunction {

    private LispObject callable;

    public WrappedFunction(
            LispObject callable, LispType type, String name, FormalArguments formalArguments) {
        super(type, name, formalArguments);
        this.callable = callable;
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments)
            throws LispException {
        return callable.call(namespace, arguments);
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        // This method will never be called since the `call()` is overriden.
        return null;
    }
}
