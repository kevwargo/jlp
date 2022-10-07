package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;

public class LispMethod extends LispBaseObject {

    protected LispObject object;
    protected LispFunction function;
    protected String name;

    public LispMethod(LispObject object, LispFunction function) {
        super(LispType.METHOD);
        this.object = object;
        this.function = function;
        name = function.getName();
    }

    public LispObject call(LispRuntime runtime, ArgumentsIterator arguments) throws LispException {
        arguments.setFirst(object);
        return function.call(runtime, arguments);
    }

    public String getName() {
        return name;
    }

    public String repr() {
        return String.format("<bound method %s of %s>", name, object.repr());
    }
}
