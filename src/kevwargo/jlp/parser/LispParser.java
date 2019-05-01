package kevwargo.jlp.parser;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Stack;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;


public class LispParser {

    private LispScanner scanner;
    private Stack<LispList> sexpStack;
    private LispList currentSexp;
    private HashMap<String, LispSymbol> symbolMap;
    

    public LispParser(String filename) throws IOException {
        this(new FileInputStream(filename));
    }
    
    public LispParser(InputStream stream) {
        scanner = new LispScanner(stream);
        sexpStack = new Stack<LispList>();
        symbolMap = new HashMap<String, LispSymbol>();
    }

    public LispObject read() throws LispException {
        LispObject object = readInternal();
        if (object != null && object.isInstance(LispType.LIST) && ! ((LispList)object).iterator().hasNext()) {
            return LispBool.NIL;
        }
        return object;
    }

    private LispObject readInternal() throws LispException {
        currentSexp = null;
        LispToken token;
        while ((token = scanner.nextLispToken()) != null) {
            LispObject object;
            LispList sexp;
            String specialName;
            switch (token.getType()) {
                case OPEN_PAREN:
                    if (currentSexp != null) {
                        sexpStack.push(currentSexp);
                    }
                    currentSexp = new LispList();
                    break;
                case CLOSE_PAREN:
                    if (currentSexp == null) {
                        throw new LispException("Invalid input: `)'");
                    }
                    if (sexpStack.empty()) {
                        return currentSexp;
                    }
                    currentSexp = sexpStack.pop().add(currentSexp);
                    while (currentSexp.isSpecial() && !sexpStack.empty()) {
                        currentSexp = sexpStack.pop().add(currentSexp);
                    }
                    if (currentSexp.isSpecial() && sexpStack.empty()) {
                        return currentSexp;
                    }
                    break;
                case SPECIAL:
                    if (currentSexp != null) {
                        sexpStack.push(currentSexp);
                    }
                    specialName = token.getValue();
                    if (specialName.equals("'")) {
                        specialName = "quote";
                    }
                    currentSexp = (new LispList(true)).add(getSymbol(specialName));
                    break;
                default:
                    if ((object = processToken(token)) != null) {
                        return object;
                    }
            }
        }
        if (!sexpStack.empty() || currentSexp != null) {
            throw new LispException("End of file during parsing");
        }
        return null;
    }

    private LispSymbol getSymbol(String name) {
        LispSymbol symbol = symbolMap.get(name);
        if (symbol == null) {
            symbol = new LispSymbol(name);
            symbolMap.put(name, symbol);
        }
        return symbol;
    }

    private LispObject processToken(LispToken token) throws LispException {
        LispObject object = null;
        switch (token.getType()) {
            case SYMBOL:
                if (token.getValue().equals("t")) {
                    object = LispBool.T;
                } else if (token.getValue().equals("nil")) {
                    object = LispBool.NIL;
                } else {
                    try {
                        object = new LispInt(Long.parseLong(token.getValue()));
                    } catch (NumberFormatException nfe) {
                        try {
                            object = new LispFloat(Double.parseDouble(token.getValue()));
                        } catch (NumberFormatException nfe1) {
                            object = getSymbol(token.getValue());
                        }
                    }
                }
                break;
            case STRING:
                object = new LispString(token.getValue());
                break;
        }
        if (currentSexp != null) {
            currentSexp = currentSexp.add(object);
            while (currentSexp.isSpecial() && !sexpStack.empty()) {
                currentSexp = sexpStack.pop().add(currentSexp);
            }
            if (sexpStack.empty() && currentSexp.isSpecial()) {
                return currentSexp;
            }
            return null;
        } else {
            return object;
        }
    }

    private String stackToString() {
        StringBuffer sb = new StringBuffer();
        for (LispList s : sexpStack) {
            sb.append(String.format("Sexp %d: %s\n", s.hashCode(), s.toString()));
        }
        return sb.toString();
    }

}
