package kevwargo.jlp.parser;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Stack;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispNumber;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispT;
import kevwargo.jlp.objects.Sexp;

public class LispParser {

    private LispScanner scanner;
    private Stack<Sexp> sexpStack;
    private Sexp currentSexp;
    

    public LispParser(String filename) throws IOException {
        this(new FileInputStream(filename));
    }

    public LispParser(InputStream stream) {
        scanner = new LispScanner(stream);
        sexpStack = new Stack<Sexp>();
    }

    public LispObject read() throws LispException {
        currentSexp = null;
        LispToken token;
        while ((token = scanner.nextLispToken()) != null) {
            LispObject object;
            Sexp sexp;
            String specialName;
            switch (token.getType()) {
                case OPEN_PAREN:
                    if (currentSexp != null) {
                        sexpStack.push(currentSexp);
                    }
                    currentSexp = Sexp.getInstance();
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
                    currentSexp = Sexp.getSpecialInstance().add(new LispSymbol(specialName));
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
                    object = LispT.getInstance();
                } else if (token.getValue().equals("nil")) {
                    object = Sexp.getInstance();
                } else {
                    try {
                        object = new LispNumber(token.getValue());
                    } catch (NumberFormatException nfe) {
                        object = new LispSymbol(token.getValue());
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
        for (Sexp s : sexpStack) {
            sb.append(String.format("Sexp %d: %s\n", s.hashCode(), s.toString()));
        }
        return sb.toString();
    }

}
