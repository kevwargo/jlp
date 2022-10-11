package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.ArrayList;
import java.util.List;

public class LMBackquote extends LispFunction {

    public static final String NAME = "`";
    public static final String ARG_EXPR = "expr";

    private static final LispSymbol symEval = new LispSymbol(",");
    private static final LispSymbol symUnfold = new LispSymbol(",@");

    public LMBackquote() {
        super(LispType.MACRO, NAME, new CallArgs(ARG_EXPR));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispObject expr = args.get(ARG_EXPR);

        if (!expr.isInstance(LispType.LIST)) {
            return expr;
        }

        return processList((LispList) expr.cast(LispType.LIST), runtime);
    }

    private LispList processList(LispList expr, LispRuntime runtime) throws LispException {
        List<LispObject> contents = new ArrayList<LispObject>();

        for (LispObject obj : expr) {
            if (!obj.isInstance(LispType.LIST)) {
                contents.add(obj);
                continue;
            }

            LispList list = (LispList) obj.cast(LispType.LIST);
            LispObject first = list.get(0);
            if (symEval.equals(first)) {
                LispObject second = list.get(1).eval(runtime);
                contents.add(second);
                continue;
            }
            if (symUnfold.equals(first)) {
                LispObject second = list.get(1).eval(runtime);
                LispList sublist = (LispList) second.cast(LispType.LIST);
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
