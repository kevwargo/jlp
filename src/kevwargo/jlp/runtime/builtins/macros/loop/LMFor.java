package kevwargo.jlp.objects.builtins.macros.loop;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LMFor extends LispFunction {

    public static final String NAME = "for";
    public static final String ARG_COND = "cond";
    public static final String ARG_BODY = "body";
    public static final String FN_BREAK = "break";
    public static final String FN_CONTINUE = "continue";
    public static final String FN_EMIT = "emit";

    public LMFor() {
        super(LispType.MACRO, NAME, new CallArgs(ARG_COND).rest(ARG_BODY));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispList cond = (LispList) args.get(ARG_COND).cast(LispType.LIST);
        if (cond.size() != 3) {
            throw new LispException(
                    "'%s' loop condition must contain 3 elements, not %d (%s)",
                    NAME, cond.size(), cond);
        }

        LispList body = (LispList) args.get(ARG_BODY);
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

        Layer map = new Layer();
        map.put(FN_BREAK, new LoopExit(false));
        map.put(FN_CONTINUE, new LoopExit(true));
        map.put(FN_EMIT, new Emit(collector));
        LispRuntime bodyRuntime = runtime.with(map);
        while (cond.eval(runtime).bool()) {
            if (executeBody(bodyRuntime, body)) {
                return;
            }

            incr.eval(runtime);
        }
    }

    private boolean executeBody(LispRuntime runtime, LispList body) throws LispException {
        for (LispObject form : body) {
            LispLoopException exc = null;
            try {
                form.eval(runtime);
            } catch (LispLoopException lle) {
                if (lle.getLevel() > 1) {
                    exc = new LispLoopException(lle);
                } else if (lle.getLevel() == 1 && !lle.isContinue()) {
                    return true;
                }
            }
            if (exc != null) {
                throw exc;
            }
        }

        return false;
    }

    private static class LoopExit extends LispFunction {

        private static final String ARG_LEVEL = "level";
        private static final CallArgs callArgs = new CallArgs().opt(ARG_LEVEL);
        private boolean isContinue;

        LoopExit(boolean isContinue) {
            super(LispType.FUNCTION, (isContinue ? FN_CONTINUE : FN_BREAK), callArgs);
            this.isContinue = isContinue;
        }

        public LispObject call(LispRuntime runtime, Layer args) throws LispException {
            long level = 1;
            LispObject levelObject = args.get(ARG_LEVEL);
            if (levelObject != null) {
                level = ((LispInt) levelObject.cast(LispType.INT)).getValue();
            }
            throw new LispLoopException(isContinue, level);
        }
    }

    private static class Emit extends LispFunction {

        private static final String ARG_OBJ = "obj";
        private LispList collector;

        Emit(LispList collector) {
            super(LispType.FUNCTION, FN_EMIT, new CallArgs(ARG_OBJ));
            this.collector = collector;
        }

        public LispObject call(LispRuntime runtime, Layer args) throws LispException {
            LispObject obj = args.get(ARG_OBJ);
            collector.add(obj);
            return obj;
        }
    }
}
