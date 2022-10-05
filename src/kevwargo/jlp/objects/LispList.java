package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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

    public LispObject eval(LispRuntime runtime) throws LispException {
        Iterator<LispObject> it = this.iterator();
        if (!it.hasNext()) {
            return LispBool.NIL;
        }

        LispObject function = it.next().eval(runtime);
        LispRuntime evalRuntime = function.isInstance(LispType.MACRO) ? null : runtime;
        ArgumentsIterator args = new ArgumentsIterator(it, evalRuntime, contents.size() - 1);

        return function.call(runtime, args);
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
        super("list", new LispType[] {OBJECT});
    }

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
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
