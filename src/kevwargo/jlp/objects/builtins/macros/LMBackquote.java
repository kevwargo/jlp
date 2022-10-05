package kevwargo.jlp.objects.builtins.macros;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LMBackquote extends LispFunction {

    public static final String NAME = "`";
    public static final String ARG_EXPR = "expr";

    private static final LispSymbol symEval = new LispSymbol(",");
    private static final LispSymbol symUnfold = new LispSymbol(",@");

    public LMBackquote() {
        super(LispType.MACRO, NAME, new FormalArguments(ARG_EXPR));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject expr = arguments.get(ARG_EXPR);

        if (!expr.isInstance(LispType.LIST)) {
            return expr;
        }

        return processList((LispList)expr.cast(LispType.LIST), namespace);
    }

    private LispList processList(LispList expr, LispNamespace namespace) throws LispException {
        List<LispObject> contents = new ArrayList<LispObject>();

        for (LispObject obj : expr) {
            if (!obj.isInstance(LispType.LIST)) {
                contents.add(obj);
                continue;
            }

            LispList list = (LispList)obj.cast(LispType.LIST);
            LispObject first = list.get(0);
            if (symEval.equals(first)) {
                LispObject second = list.get(1).eval(namespace);
                contents.add(second);
                continue;
            }
            if (symUnfold.equals(first)) {
                LispObject second = list.get(1).eval(namespace);
                LispList sublist = (LispList)second.cast(LispType.LIST); // TODO: throw more readable exception if not list
                for (LispObject elt : sublist) {
                    contents.add(elt);
                }
                continue;
            }

            contents.add(processList(list, namespace));
        }

        return new LispList(contents);
    }

}
