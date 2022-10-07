package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;

public interface LispCallable extends LispObject {

    public FormalArguments getFormalArgs();

    public LispObject call(LispRuntime runtime, ArgumentsIterator arguments) throws LispException;
}
