package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.utils.ArgumentsIterator;

public class LispMethod extends LispObject {

    protected LispObject object;
    protected LispFunction function;
    protected String name;

    public LispMethod(LispObject object, LispFunction function) {
        super(LispType.METHOD);
        this.object = object;
        this.function = function;
        name = function.getName();
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments)
            throws LispException {
        arguments.setFirst(object);
        return function.call(namespace, arguments);
    }

    public String getName() {
        return name;
    }

    public String repr() {
        return String.format("<bound method %s of %s>", name, object.repr());
    }
}
