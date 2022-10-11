package kevwargo.jlp.objects.builtins.macros.loop;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

public class LMFor extends LoopBase {

    public static final String NAME = "for";
    public static final String ARG_COND = "cond";
    public static final String ARG_BODY = "body";

    public LMFor() {
        super(NAME, new CallArgs(ARG_COND).rest(ARG_BODY));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispList cond = (LispList) args.get(ARG_COND).cast(LispType.LIST);
        if (cond.size() != 3) {
            throw new LispException(
                    "'for' loop condition must contain 3 elements, not %d (%s)", cond.size(), cond);
        }

        LispList body = (LispList) args.get(ARG_BODY).cast(LispType.LIST);
        LispList collector = new LispList();

        execute(cond, body, runtime, collector);

        return collector;
    }

    private void execute(LispList forCond, LispList body, LispRuntime runtime, LispList collector)
            throws LispException {
        LispObject init = forCond.get(0);
        LispObject cond = forCond.get(1);
        LispObject incr = forCond.get(2);

        init.eval(runtime);

        while (cond.eval(runtime).bool()) {
            if (executeBody(runtime, body, collector)) {
                return;
            }

            incr.eval(runtime);
        }
    }
}
