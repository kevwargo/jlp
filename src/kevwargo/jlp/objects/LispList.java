package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class LispList extends LispBaseObject implements LispIterable {

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

    public boolean isEmpty() {
        return contents.isEmpty();
    }

    public boolean isSpecial() {
        return special;
    }

    public LispObject eval(LispRuntime runtime) throws LispException {
        Iterator<LispObject> it = this.iterator();
        if (!it.hasNext()) {
            return this;
        }

        LispObject head = it.next().eval(runtime);
        if (!(head instanceof LispCallable)) {
            throw new LispException("Object '%s' is not callable", head.getType().getName());
        }
        LispCallable callable = (LispCallable) head;

        LispRuntime evalRuntime = callable.isInstance(LispType.MACRO) ? null : runtime;
        ArgumentsIterator args = new ArgumentsIterator(it, evalRuntime, contents.size() - 1);

        return callable.call(runtime, args);
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

    public boolean bool() {
        return !isEmpty();
    }
}

class ListType extends LispType {

    ListType() {
        super("list", new LispType[] {OBJECT});
    }

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
        LispList result = new LispList();
        while (arguments.hasNext()) {
            result.add(arguments.next());
        }

        return result;
    }
}
