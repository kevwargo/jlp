package kevwargo.jlp.parser;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.objects.scalars.LispString;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.objects.scalars.numbers.LispFloat;
import kevwargo.jlp.objects.scalars.numbers.LispInt;

import java.io.InputStream;
import java.util.Stack;

public class LispParser {

    private LispScanner scanner;
    private Stack<LispList> sexpStack;
    private LispList currentSexp;

    public LispParser(InputStream stream) {
        scanner = new LispScanner(stream);
        sexpStack = new Stack<LispList>();
    }

    public LispObject read() throws LispException {
        currentSexp = null;
        LispToken token;
        while ((token = scanner.nextLispToken()) != null) {
            LispObject object;
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
                    currentSexp = (new LispList(true)).add(LispSymbol.make(specialName));
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

    private LispObject processToken(LispToken token) throws LispException {
        LispObject object = null;

        switch (token.getType()) {
            case SYMBOL:
                if (token.getValue().equals("t")) {
                    object = LispBool.TRUE;
                } else if (token.getValue().equals("false")) {
                    object = LispBool.FALSE;
                } else if (token.getValue().equals("nil")) {
                    object = LispNil.NIL;
                } else {
                    object = parseNumber(token.getValue());
                    if (object == null) {
                        object = LispSymbol.make(token.getValue());
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

    private LispObject parseNumber(String token) {
        char suffix = token.charAt(token.length() - 1);
        String stripped = token.substring(0, token.length() - 1);

        try {
            switch (suffix) {
                case 'l':
                    return new LispInt(Long.parseLong(stripped));
                case 's':
                    return new LispInt(Short.parseShort(stripped));
                case 'c':
                    return new LispInt((char) Integer.parseInt(stripped));
                case 'b':
                    return new LispInt(Byte.parseByte(stripped));
                default:
                    return new LispInt(Integer.parseInt(token));
            }
        } catch (NumberFormatException nfe) {
            try {
                switch (suffix) {
                    case 'f':
                        return new LispFloat(Float.parseFloat(stripped));
                    default:
                        return new LispFloat(Double.parseDouble(token));
                }
            } catch (NumberFormatException nfe1) {
            }
        }

        return null;
    }
}
