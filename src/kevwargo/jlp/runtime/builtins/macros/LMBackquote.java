package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.ArrayList;
import java.util.List;

public class LMBackquote extends LispFunction {

    public static final String NAME = "`";
    public static final String ARG_EXPR = "expr";

    private static final LispSymbol symEval = new LispSymbol(",");
    private static final LispSymbol symUnfold = new LispSymbol(",@");

    public LMBackquote() {
        super(LispFunction.MACRO_TYPE, NAME, new CallArgs(ARG_EXPR));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject expr = args.get(ARG_EXPR);

        if (!expr.isInstance(LispList.TYPE)) {
            return expr;
        }

        return processList((LispList) expr.cast(LispList.TYPE), runtime);
    }

    private LispList processList(LispList expr, LispRuntime runtime) throws LispException {
        List<LispObject> contents = new ArrayList<LispObject>();

        for (LispObject obj : expr) {
            if (!obj.isInstance(LispList.TYPE)) {
                contents.add(obj);
                continue;
            }

            LispList list = (LispList) obj.cast(LispList.TYPE);
            LispObject first = list.get(0);
            if (symEval.equals(first)) {
                LispObject second = list.get(1).eval(runtime);
                contents.add(second);
                continue;
            }
            if (symUnfold.equals(first)) {
                LispObject second = list.get(1).eval(runtime);
                LispList sublist = (LispList) second.cast(LispList.TYPE);
                // TODO: throw more readable exception if not a list
                for (LispObject elt : sublist) {
                    contents.add(elt);
                }
                continue;
            }

            contents.add(processList(list, runtime));
        }

        return new LispList(contents);
    }
}
