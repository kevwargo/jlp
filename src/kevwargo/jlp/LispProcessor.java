package kevwargo.jlp;

import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import kevwargo.jlp.objects.*;
import kevwargo.jlp.objects.builtins.macros.LispBuiltins_Defun;
import kevwargo.jlp.objects.builtins.macros.LispBuiltins_Defmacro;

public class LispProcessor {

    private static LispProcessor instance;
    private LispNamespace basicNamespace;

    public static LispProcessor getInstance() {
        if (instance == null) {
            instance = new LispProcessor();
        }
        return instance;
    }

    private LispProcessor() {
        HashMap<String, LispObject> namespace = new HashMap<String, LispObject>();
        namespace.put("print", new LispBuiltinFunction("print", new String[0], true) {
                public LispObject eval(LispNamespace namespace) {
                    for (LispObject object : rest) {
                        System.out.println(object.toString());
                    }
                    return LispNil.getInstance();
                }
            });
        namespace.put("quote", new LispBuiltinMacro("quote", new String[] {"arg"}, false) {
                public LispObject eval(LispNamespace namespace) {
                    return arguments.get("arg");
                }
            });
        namespace.put("concat", new LispBuiltinFunction("concat", new String[0], true) {
                public LispObject eval(LispNamespace namespace) throws LispException {
                    String result = "";
                    for (LispObject object : rest) {
                        if (!(object instanceof LispString)) {
                            throw new LispException("Wrong argument type: string expected");
                        }
                        result += ((LispString)object).getValue();
                    }
                    return new LispString(result);
                }
            });
        namespace.put("defun", new LispBuiltins_Defun());
        namespace.put("defmacro", new LispBuiltins_Defmacro());
        namespace.put("let", new LispBuiltinMacro("let", new String[] {"mappings"}, true) {
                public LispObject eval(LispNamespace namespace) throws LispException {
                    LispObject mappingsObject = arguments.get("mappings");
                    if (!(mappingsObject instanceof Sexp)) {
                        throw new LispException("Wrong argument type: mappings must be a sexp: " + mappingsObject.toString());
                    }
                    HashMap<String, LispObject> mappings = new HashMap<String, LispObject>();
                    ListIterator<LispObject> iterator = ((Sexp)mappingsObject).listIterator();
                    while (iterator.hasNext()) {
                        LispObject mapping = iterator.next();
                        if (mapping instanceof LispSymbol) {
                            mappings.put(((LispSymbol)mapping).getName(), LispNil.getInstance());
                        } else if (mapping instanceof Sexp) {
                            ListIterator<LispObject> mappingIterator = ((Sexp)mapping).listIterator();
                            LispObject varObject = mappingIterator.next();
                            if (!(varObject instanceof LispSymbol)) {
                                throw new LispException("Wrong argument type: variable must be a symbol: " + varObject.toString());
                            }
                            LispObject valObject;
                            if (mappingIterator.hasNext()) {
                                valObject = mappingIterator.next().eval(namespace);
                            } else {
                                valObject = LispNil.getInstance();
                            }
                            mappings.put(((LispSymbol)varObject).getName(), valObject);
                            if (mappingIterator.hasNext()) {
                                throw new LispException("Mapping must be of structure (var val): " + mapping.toString());
                            }
                        } else {
                                throw new LispException("Mapping must be a symbol or a sexp: " + mapping.toString());
                        }
                    }
                    LispObject result = LispNil.getInstance();
                    LispNamespace localNamespace = namespace.prepend(mappings);
                    for (LispObject expr : rest) {
                        result = expr.eval(localNamespace);
                    }
                    return result;
                }
            });
        basicNamespace = new LispNamespace(namespace);
    }

    public void process(String lispSource) throws LispException {
        LispObject result = (new LispParser()).parse(lispSource).eval(basicNamespace);
        System.out.println("Result: " + result.toString());
    }

}
