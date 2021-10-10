package kevwargo.jlp.objects.builtins.macros.loop;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LoopBase extends LispFunction {

    public LoopBase(String name, FormalArguments formalArguments) {
        super(LispType.MACRO, name, formalArguments);
    }

    protected boolean executeBody(LispNamespace namespace, LispList body) throws LispException {
        Map<String, LispObject> map = new HashMap<String, LispObject>();
        map.put("break", new LoopExit(false));
        map.put("continue", new LoopExit(true));
        LispNamespace bodyNamespace = namespace.prepend(map);
        for (LispObject form : body) {
            LispLoopException exc = null;
            try {
                form.eval(bodyNamespace);
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


    private class LoopExit extends LispFunction {

        private boolean isContinue;

        public LoopExit(boolean isContinue) {
            super(LispType.FUNCTION, (isContinue ? "continue" : "break"), new FormalArguments().opt("level"));
            this.isContinue = isContinue;
        }

        protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
            long level = 1;
            LispObject levelObject = arguments.get("level");
            if (levelObject != null) {
                level = ((LispInt)levelObject.cast(LispType.INT)).getValue();
            }
            throw new LispLoopException(isContinue, level);
        }

    }

}
