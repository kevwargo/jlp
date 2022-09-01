package kevwargo.jlp.objects;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class LispList extends LispObject implements Iterable<LispObject> {

    private List<LispObject> contents;
    private boolean special;

    public LispList(List<LispObject> contents, boolean special) {
        super(LispType.LIST);
        this.contents = contents;
        this.special = special;
    }

    public LispList(List<LispObject> contents) {
        this(contents, false);
    }

    public LispList(boolean special) {
        this(new ArrayList<LispObject>(), special);
    }

    public LispList(Iterator<LispObject> contents) {
        this(false);
        while (contents.hasNext()) {
            this.contents.add(contents.next());
        }
    }

    public LispList() {
        this(new ArrayList<LispObject>(), false);
    }


    public Iterator<LispObject> iterator() {
        return contents.iterator();
    }

    public LispList add(LispObject object) {
        contents.add(object);
        return this;
    }

    public LispObject get(int index) {
        return this.contents.get(index);
    }

    public int size() {
        return contents.size();
    }

    public boolean isSpecial() {
        return special;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        Iterator<LispObject> it = this.iterator();
        if (! it.hasNext()) {
            return LispBool.NIL;
        }
        LispObject function = it.next().eval(namespace);
        boolean isMacro = function.isInstance(LispType.MACRO);
        ArgumentsIterator args = new ArgumentsIterator(it, isMacro ? null : namespace, contents.size() - 1);
        return function.call(namespace, args);
    }

    public String repr() {
        StringBuffer sb = new StringBuffer();
        sb.append("(");
        Iterator<LispObject> it = iterator();
        if (it.hasNext()) {
            sb.append(it.next().repr());
        }
        while (it.hasNext()) {
            sb.append(" ");
            sb.append(it.next().repr());
        }
        sb.append(")");
        return sb.toString();
    }
}

class ListType extends LispType {

    ListType() {
        super("list", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        ArrayList<LispObject> result = new ArrayList<LispObject>();
        while (arguments.hasNext()) {
            result.add(arguments.next());
        }
        if (result.isEmpty()) {
            return LispBool.NIL;
        }
        return new LispList(result);
    }

}
