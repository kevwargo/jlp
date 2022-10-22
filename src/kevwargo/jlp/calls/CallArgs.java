package kevwargo.jlp.calls;

import kevwargo.jlp.exceptions.DuplicatedArgException;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

public class CallArgs {

    private List<Positional> positionals;
    private List<String> optionals;
    private Map<String, LispObject> optionalDefaults;
    private String rest;
    private Map<String, LispObject> keys; // key => default (null if no default)
    private String otherKeys;

    private CallArgs origin;

    private boolean ignore;
    private static CallArgs ignored = new CallArgs(true, null);

    private static final String KEYWORD_OPTIONAL = "&optional";
    private static final String KEYWORD_REST = "&rest";
    private static final String KEYWORD_KEY = "&key";
    private static final String KEYWORD_OTHER_KEYS = "&other-keys";

    private CallArgs(boolean ignore, CallArgs origin) {
        positionals = new ArrayList<Positional>();
        optionals = new ArrayList<String>();
        optionalDefaults = new HashMap<String, LispObject>();
        keys = new HashMap<String, LispObject>();
        this.origin = origin;
    }

    public CallArgs(String... args) {
        this(false, null);
        for (String arg : args) {
            positionals.add(new Positional(arg));
        }
    }

    public CallArgs(LispList lambdaExpr, LispRuntime runtime) throws LispException {
        this(lambdaExpr, runtime, null);
    }

    private CallArgs(LispList lambdaExpr, LispRuntime runtime, CallArgs origin)
            throws LispException {
        this(false, origin);

        ListIterator<LispObject> it = lambdaExpr.listIterator();
        parsePositionals(it, runtime);

        if (popKeyword(it, KEYWORD_OPTIONAL)) {
            parseOptionals(it, runtime);
        }
        if (popKeyword(it, KEYWORD_REST)) {
            parseRest(it);
        }
        if (popKeyword(it, KEYWORD_KEY)) {
            parseKeys(it, runtime);
        }
        if (popKeyword(it, KEYWORD_OTHER_KEYS)) {
            parseOtherKeys(it);
        }

        if (it.hasNext()) {
            LispObject arg = it.next();
            throw new LispException("Unexpected argument '%s'", arg);
        }
    }

    private void parsePositionals(ListIterator<LispObject> it, LispRuntime runtime)
            throws LispException {
        while (it.hasNext() && !atKeyword(it)) {
            LispObject arg = it.next();
            if (arg.isInstance(LispType.SYMBOL)) {
                String name = ((LispSymbol) arg.cast(LispType.SYMBOL)).getName();
                checkArg(name);
                positionals.add(new Positional(name));
            } else if (arg.isInstance(LispType.LIST)) {
                LispList list = (LispList) arg.cast(LispType.LIST);
                positionals.add(new Positional(new CallArgs(list, runtime, this)));
            } else {
                throw new LispException(
                        "expected symbol or list, not '%s'", arg.getType().getName());
            }
        }
    }

    private void parseOptionals(ListIterator<LispObject> it, LispRuntime runtime)
            throws LispException {
        if (!it.hasNext()) {
            throw new LispException("Expected one or more arguments after %s", KEYWORD_OPTIONAL);
        }

        while (it.hasNext() && !atKeyword(it)) {
            OptionalDefault optDef = new OptionalDefault(it.next(), runtime);
            String name = optDef.getName();
            LispObject defaultValue = optDef.getDefault();

            checkArg(name);
            optionals.add(name);
            if (defaultValue == null) {
                defaultValue = LispNil.NIL;
            }
            optionalDefaults.put(name, defaultValue);
        }
    }

    private void parseRest(ListIterator<LispObject> it) throws LispException {
        if (!it.hasNext()) {
            throw new LispException("Expected an argument after %s", KEYWORD_REST);
        }

        LispObject arg = it.next();
        if (arg.isInstance(LispType.SYMBOL)) {
            String name = ((LispSymbol) arg.cast(LispType.SYMBOL)).getName();
            checkArg(name);
            rest = name;
        } else {
            throw new LispException("expected symbol, not '%s'", arg.getType().getName());
        }
    }

    private void parseKeys(ListIterator<LispObject> it, LispRuntime runtime) throws LispException {
        if (!it.hasNext()) {
            throw new LispException("Expected one or more arguments after %s", KEYWORD_KEY);
        }

        while (it.hasNext() && !atKeyword(it)) {
            OptionalDefault optDef = new OptionalDefault(it.next(), runtime);
            String name = optDef.getName();

            checkArg(name);
            keys.put(name, optDef.getDefault());
        }
    }

    private void parseOtherKeys(ListIterator<LispObject> it) throws LispException {
        if (!it.hasNext()) {
            throw new LispException("Expected an argument after %s", KEYWORD_OTHER_KEYS);
        }

        LispObject arg = it.next();
        if (arg.isInstance(LispType.SYMBOL)) {
            String name = ((LispSymbol) arg.cast(LispType.SYMBOL)).getName();
            checkArg(name);
            otherKeys = name;
        } else {
            throw new LispException("expected symbol, not '%s'", arg.getType().getName());
        }
    }

    private boolean atKeyword(ListIterator<LispObject> it) {
        if (!it.hasNext()) {
            return false;
        }

        LispObject arg = it.next();
        it.previous();
        try {
            String name = ((LispSymbol) arg.cast(LispType.SYMBOL)).getName();
            return name.equals(KEYWORD_OPTIONAL)
                    || name.equals(KEYWORD_REST)
                    || name.equals(KEYWORD_KEY)
                    || name.equals(KEYWORD_OTHER_KEYS);
        } catch (LispCastException exc) {
            return false;
        }
    }

    private boolean popKeyword(ListIterator<LispObject> it, String keyword) {
        if (!it.hasNext()) {
            return false;
        }

        LispObject arg = it.next();
        try {
            String name = ((LispSymbol) arg.cast(LispType.SYMBOL)).getName();
            if (name.equals(keyword)) {
                return true;
            }
        } catch (LispCastException exc) {
        }

        it.previous();
        return false;
    }

    public static CallArgs ignored() {
        return ignored;
    }

    public CallArgs clone() {
        CallArgs clone = new CallArgs();
        clone.positionals = positionals;
        clone.optionals = optionals;
        clone.rest = rest;
        return clone;
    }

    public String pop() {
        if (positionals.isEmpty()) {
            return null;
        }
        return positionals.remove(0).getName();
    }

    public CallArgs opt(String arg) {
        optionals.add(arg);
        return this;
    }

    public CallArgs rest(String rest) {
        this.rest = rest;
        return this;
    }

    public CallArgs key(String arg, LispObject defaultValue) {
        keys.put(arg, defaultValue);
        return this;
    }

    private void checkArg(String arg) throws DuplicatedArgException {
        if (origin != null) {
            origin.checkArg(arg);
        }

        if (positionals.contains(new Positional(arg))) {
            throw new DuplicatedArgException(arg);
        }
        if (optionals.contains(arg)) {
            throw new DuplicatedArgException(arg);
        }
        if (arg.equals(rest)) {
            throw new DuplicatedArgException(arg);
        }
        if (keys.containsKey(arg)) {
            throw new DuplicatedArgException(arg);
        }
        if (arg.equals(otherKeys)) {
            throw new DuplicatedArgException(arg);
        }
    }

    public Layer apply(LispList args) throws LispException {
        if (ignore) {
            return null;
        }

        Layer layer = new Layer();
        apply(layer, args);
        return layer;
    }

    private void apply(Layer layer, LispList args) throws LispException {
        args = applyKeys(layer, args);

        if (args.size() < positionals.size()) {
            throw new LispException(
                    "Not enough arguments (%d %d)", args.size(), positionals.size());
        }

        Iterator<LispObject> it = args.iterator();
        for (Positional arg : positionals) {
            if (arg.getName() != null) {
                layer.put(arg.getName(), it.next());
            } else {
                arg.getStruct().apply(layer, (LispList) it.next().cast(LispType.LIST));
            }
        }

        Iterator<String> optIterator = optionals.iterator();
        while (it.hasNext() && optIterator.hasNext()) {
            layer.put(optIterator.next(), it.next());
        }
        while (optIterator.hasNext()) {
            String name = optIterator.next();
            if (optionalDefaults.containsKey(name)) {
                layer.put(name, optionalDefaults.get(name));
            }
        }

        if (rest == null) {
            if (it.hasNext()) {
                throw new LispException("Too many arguments");
            }
        } else {
            layer.put(rest, new LispList(it));
        }
    }

    private LispList applyKeys(Layer layer, LispList args) throws LispException {
        LispList nonKeys = new LispList();
        // TODO: change to LispDict
        LispList otherKeys = new LispList();
        Set<String> usedKeys = new HashSet<String>();

        Iterator<LispObject> it = args.iterator();
        while (it.hasNext()) {
            LispObject arg = it.next();
            if (arg.isInstance(LispType.SYMBOL)) {
                LispSymbol symbol = (LispSymbol) arg.cast(LispType.SYMBOL);
                if (symbol.isKeyword()) {
                    String name = symbol.getName().substring(1);
                    if (!it.hasNext()) {
                        throw new LispException("Key '%s' specified without a value", name);
                    }

                    if (usedKeys.contains(name)) {
                        throw new LispException("Key '%s' specified more than once", name);
                    }
                    usedKeys.add(name);

                    if (keys.containsKey(name)) {
                        layer.put(name, it.next());
                    } else if (this.otherKeys != null) {
                        // TODO: use LispDict
                        otherKeys.add(symbol).add(it.next());
                    } else {
                        throw new LispException("Unexpected key '%s'", name);
                    }
                    continue;
                }
            }

            nonKeys.add(arg);
        }

        for (Map.Entry<String, LispObject> e : keys.entrySet()) {
            String name = e.getKey();
            LispObject value = e.getValue();

            if (!layer.containsKey(name)) {
                if (value == null) {
                    throw new LispException("Missing required key '%s'", name);
                }
                layer.put(name, value);
            }
        }

        if (this.otherKeys != null) {
            layer.put(this.otherKeys, otherKeys);
        }

        return nonKeys;
    }
}
