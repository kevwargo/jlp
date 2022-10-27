package kevwargo.jlp.runtime.builtins.exceptions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.runtime.LispRuntime;

public class Handler {

    private LispList body;
    private LispRuntime runtime;

    public Handler(LispList body, LispRuntime runtime) {
        this.body = body;
        this.runtime = runtime;
    }

    public LispObject handle(Throwable throwable) throws LispException {
        LispObject result = LispNil.NIL;
        for (LispObject form : body) {
            result = form.eval(runtime);
        }
        return result;
    }
}
