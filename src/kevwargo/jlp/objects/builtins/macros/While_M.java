package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispLoopException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispInt;

public class While_M extends LispFunction {

    public While_M() {
        super(LispType.MACRO, "while", new FormalArguments().pos("condition").rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject condition = arguments.get("condition");
        HashMap<String, LispObject> map = new HashMap<String, LispObject>();
        map.put("break", new LoopExit(false));
        map.put("continue", new LoopExit(true));
        LispNamespace bodyNamespace = namespace.prepend(map);
        boolean finished = false;
        while (condition.eval(bodyNamespace) != LispBool.FALSE) {
            for (LispObject form : (LispList)arguments.get("body")) {
                LispLoopException exc = null;
                try {
                    form.eval(bodyNamespace);
                } catch (LispLoopException lle) {
                    if (lle.getLevel() > 1) {
                        exc = new LispLoopException(lle);
                    } else if (lle.getLevel() == 1) {
                        if (!lle.isContinue()) {
                            finished = true;
                        }
                        break;
                    }
                }
                if (exc != null) {
                    throw exc;
                }
            }
            if (finished) {
                break;
            }
        }
        return LispBool.FALSE;
    }


    private class LoopExit extends LispFunction {

        private boolean isContinue;

        public LoopExit(boolean isContinue) {
            super(LispType.FUNCTION, (isContinue ? "continue" : "break"), new FormalArguments().opt("level"));
            this.isContinue = isContinue;
        }

        protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
            long level = 1;
            LispObject levelObject = arguments.get("level");
            if (levelObject != null) {
                level = ((LispInt)levelObject.cast(LispType.INT)).getValue();
            }
            throw new LispLoopException(isContinue, level);
        }

    }

}
