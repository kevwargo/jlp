package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFNth extends LispFunction {

    public LFNth() {
        super(LispType.FUNCTION, "nth", new FormalArguments().pos("n").pos("list"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        long idx = ((LispInt)arguments.get("n").cast(LispType.INT)).getValue();
        LispList list = (LispList)arguments.get("list").cast(LispType.LIST);

        return list.get((int)idx);
    }

}
