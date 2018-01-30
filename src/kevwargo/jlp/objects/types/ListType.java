package kevwargo.jlp.objects.types;

import java.util.ArrayList;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class ListType extends LispType {

    ListType() {
        super(LispType.TYPE, "list", new LispType[] { LispType.OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        ArrayList<LispObject> result = new ArrayList<LispObject>();
        while (arguments.hasNext()) {
            result.add(arguments.next());
        }
        if (result.isEmpty()) {
            return LispBool.FALSE;
        }
        return new LispList(result);
    }

}
