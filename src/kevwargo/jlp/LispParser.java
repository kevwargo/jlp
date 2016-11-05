package kevwargo.jlp;

import kevwargo.jlp.object.*;
import java.util.Stack;

public class LispParser {

    private Stack<Sexp> sexpStack;
    private int position;

    public LispObject parse(String lispSource) throws LispException {
        sexpStack = new Stack<Sexp>();
        int length = lispSource.length();
        for (position = 0; position < length; ) {
            Sexp sexp;
            switch (lispSource.charAt(position)) {
                case ' ': case '\t': case '\n': case '\r':
                    position++;
                    break;
                case '(':
                    position++;
                    sexp = new Sexp();
                    if (!sexpStack.empty()) {
                        sexpStack.peek().add(sexp);
                    }
                    sexpStack.push(sexp);
                    break;
                case ')':
                    position++;
                    if (sexpStack.empty()) {
                        throw new LispException("Invalid input: ')'");
                    }
                    sexp = sexpStack.pop();
                    if (sexpStack.empty()) {
                        return sexp;
                    }
                    break;
                case '"':
                    LispString string = parseString(lispSource, length);
                    if (sexpStack.empty()) {
                        return string;
                    }
                    sexpStack.peek().add(string);
                    break;
                case '1': case '2': case '3': case '4': case '5':
                case '6': case '7': case '8': case '9': case '0':
                case '-': case '+': case '.':
                    LispObject number = parseNumber(lispSource, length);
                    if (sexpStack.empty()) {
                        return number;
                    }
                    sexpStack.peek().add(number);
                    break;
                default:
                    LispObject symbol = parseSymbol(lispSource, length);
                    if (sexpStack.empty()) {
                        return symbol;
                    }
                    sexpStack.peek().add(symbol);
            }
        }
        if (sexpStack.empty()) {
            return null;
        } else {
            throw new LispException("Unexpected end of stream while parsing a sexp");
        }
    }

    private char escapeChar(char c) {
        switch (c) {
            case 'n':
                return '\n';
            case 'r':
                return '\r';
            case 't':
                return '\t';
            default:
                return c;
        }
    }

    private LispString parseString(String source, int length) throws LispException {
        position++;
        StringBuffer sb = new StringBuffer();
        for (; position < length; position++) {
            char currentChar = source.charAt(position);
            switch (currentChar) {
                case '"':
                    position++;
                    return new LispString(sb.toString());
                case '\\':
                    if (position == length - 1) {
                        throw new LispException("A character was expected after a backslash");
                    }
                    sb.append(escapeChar(source.charAt(++position)));
                    break;
                default:
                    sb.append(currentChar);
            }
        }
        throw new LispException("Unexpected end of stream while parsing a string");
    }

    private LispObject parseSymbol(String source, int length) throws LispException {
        String possibleT = source.substring(position, position + (position < length - 1 ? 2 : 1));
        if (possibleT.matches("t([ \t\n\r\"()]|$)")) {
            position++;
            return new LispT();
        }
        if (position <= length - 3) {
            String possibleNil = source.substring(position, position + (position < length - 3 ? 4 : 3));
            if (possibleNil.matches("nil([ \t\n\r\"()]|$)")) {
                position += 3;
                return new LispNil();
            }
        }
        StringBuffer sb = new StringBuffer();
        for (; position < length; position++) {
            char currentChar = source.charAt(position);
            switch (currentChar) {
                case '\\':
                    if (position == length - 1) {
                        throw new LispException("A character was expected after a backslash");
                    }
                    sb.append(source.charAt(++position));
                    break;
                case '"': case '(': case ')': case ' ': case '\t': case '\n': case '\r':
                    return new Symbol(sb.toString());
                default:
                    sb.append(currentChar);
            }
        }
        return new Symbol(sb.toString());
    }

    private LispObject parseNumber(String source, int length) throws LispException {
        int oldPos = position;
        StringBuffer sb = new StringBuffer();
        boolean dotAllowed = true;
        boolean expAllowed = false;
        boolean signAllowed = true;
        boolean done = false;
        for (; position < length; position++) {
            char currentChar = source.charAt(position);
            switch (currentChar) {
                case '-': case '+':
                    if (!signAllowed) {
                        position = oldPos;
                        return parseSymbol(source, length);
                    }
                    break;
                case '1': case '2': case '3': case '4': case '5':
                case '6': case '7': case '8': case '9': case '0':
                    expAllowed = true;
                    break;
                case '.':
                    if (!dotAllowed) {
                        position = oldPos;
                        return parseSymbol(source, length);
                    }
                    dotAllowed = false;
                break;
                case 'e': case 'E':
                    if (!expAllowed) {
                        position = oldPos;
                        return parseSymbol(source, length);
                    }
                    expAllowed = false;
                    dotAllowed = false;
                    break;
                case ' ': case '(': case ')': case '\n': case '\t': case '\r': case '"':
                    done = true;
                    break;
                default:
                    position = oldPos;
                    return parseSymbol(source, length);
            }
            signAllowed = false;
            if (done) {
                break;
            }
            sb.append(currentChar);
        }
        char lastChar = sb.charAt(sb.length() - 1);
        if (lastChar == 'e' || lastChar == 'E') {
            position = oldPos;
            return parseSymbol(source, length);
        }
        return stringToNumber(sb.toString());
    }

    private LispObject stringToNumber(String number) {
        if (number.matches("^[+-]?[0-9]+$")) {
            return new LispInt(Long.parseLong(number));
        } else {
            return new LispFloat(Double.parseDouble(number));
        }
    }


}
