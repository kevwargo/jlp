package kevwargo.jlp.objects.builtins.macros.loop;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

public abstract class LoopBase extends LispFunction {

    public static final String FN_BREAK = "break";
    public static final String FN_CONTINUE = "continue";
    public static final String FN_EMIT = "emit";

    public LoopBase(String name, CallArgs callArgs) {
        super(LispType.MACRO, name, callArgs);
    }

    protected boolean executeBody(LispRuntime runtime, LispList body, LispList collector)
            throws LispException {
        LispNamespace.Layer map = new LispNamespace.Layer();
        map.put(FN_BREAK, new LoopExit(false));
        map.put(FN_CONTINUE, new LoopExit(true));
        map.put(FN_EMIT, new Emit(collector));

        for (LispObject form : body) {
            LispLoopException exc = null;
            try {
                form.eval(runtime.with(map));
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
        private boolean isContinue;

        LoopExit(boolean isContinue) {
            super(
                    LispType.FUNCTION,
                    (isContinue ? FN_CONTINUE : FN_BREAK),
                    new CallArgs().opt(ARG_LEVEL));
            this.isContinue = isContinue;
        }

        public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
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

        public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
            LispObject obj = args.get(ARG_OBJ);
            collector.add(obj);
            return obj;
        }
    }
}
