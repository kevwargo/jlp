package kevwargo.jlp;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.Stack;
import kevwargo.jlp.objects.*;

public class LispParser {

    private static final int STATE_INIT = 0;
    private static final int STATE_SYMBOL = 1;
    private static final int STATE_STRING = 2;
    private static final int STATE_COMMENT = 3;

    private BufferedReader reader;
    private Stack<Sexp> sexpStack;
    private StringBuffer stringBuffer;
    private LispObject parsedObject;
    private LispObject currentSexp;
    private int charNum;
    private int state;

    public LispParser(InputStream stream) throws UnsupportedEncodingException {
        reader = new BufferedReader(new InputStreamReader(stream, "utf-8"));
    }

    public LispParser(String filename) throws IOException {
        this(new FileInputStream(filename));
    }

    public LispObject read() throws IOException, LispException {
        sexpStack = new Stack<Sexp>();
        stringBuffer = new StringBuffer();
        state = STATE_INIT;
        parsedObject = null;
        try {
            while (true) {
                char currentChar = nextChar();
                charNum++;
                switch (state) {
                    case STATE_INIT:
                        handleInit(currentChar);
                        break;
                    case STATE_SYMBOL:
                        handleSymbol(currentChar);
                        break;
                    case STATE_STRING:
                        handleString(currentChar);
                        break;
                    case STATE_COMMENT:
                        handleComment(currentChar);
                        break;
                }
                if (parsedObject != null) {
                    return parsedObject;
                }
            }
        } catch (EOFException e) {
            if (!sexpStack.empty() || state == STATE_SYMBOL || state == STATE_STRING) {
                throw new LispException("Unexpected end of file during parsing " + charNum);
            }
        } catch (LispException e) {
            if (reader != null) {
                reader.close();
                reader = null;
            }
            throw e;
        }
        return null;
    }

    private char nextChar() throws IOException {
        if (reader == null) {
            throw new EOFException();
        }
        int c;
        try {
            c = reader.read();
            if (c < 0) {
                throw new EOFException();
            }
        } catch (IOException e) {
            reader.close();
            reader = null;
            throw e;
        }
        return (char)c;
    }

    private void addToSexpOnTop(LispObject object) {
        if (!sexpStack.empty()) {
            Sexp sexp = sexpStack.pop();
            sexp = sexp.add(object);
            sexpStack.push(sexp);
        }
    }

    private void handleInit(char currentChar) throws LispException {
        switch (currentChar) {
            case ' ': case '\t': case '\n': case '\r':
                break;
            case '"':
                state = STATE_STRING;
                break;
            case ';':
                state = STATE_COMMENT;
                break;
            case '(':
                sexpStack.push(Sexp.getInstance());
                break;
            case ')':
                if (sexpStack.empty()) {
                    throw new LispException("Invalid input: ')'");
                }
                Sexp sexp = sexpStack.pop();
                if (sexpStack.empty()) {
                    parsedObject = sexp;
                } else {
                    addToSexpOnTop(sexp);
                }
                break;
            default:
                state = STATE_SYMBOL;
                stringBuffer.append(currentChar);
                break;
        }
    }

    private void handleSymbol(char currentChar) throws IOException, LispException {
        switch (currentChar) {
            case ' ': case '\t': case '\r': case '\n': case '(': case ')':
                state = STATE_INIT;
                break;
            case '"':
                state = STATE_STRING;
                break;
            case ';':
                state = STATE_COMMENT;
                break;
            case '\\':
                stringBuffer.append(nextChar());
                break;
            default:
                stringBuffer.append(currentChar);
        }
        if (state != STATE_SYMBOL) {
            String symbol = stringBuffer.toString();
            stringBuffer = new StringBuffer();
            LispObject object;

            if (symbol.equals("nil")) {
                object = Sexp.getInstance();
            } else if (symbol.equals("t")) {
                object = LispT.getInstance();
            } else {
                try {
                    object = new LispNumber(symbol);
                } catch (NumberFormatException nfe) {
                    object = new LispSymbol(symbol);
                }
            }

            if (sexpStack.empty()) {
                parsedObject = object;
            } else {
                addToSexpOnTop(object);
            }

            if (state == STATE_INIT) {
                handleInit(currentChar);
            }
        }
    }

    private void handleString(char currentChar) throws IOException {
        switch (currentChar) {
            case '"':
                state = STATE_INIT;
                LispString string = new LispString(stringBuffer.toString());
                stringBuffer = new StringBuffer();
                if (sexpStack.empty()) {
                    parsedObject = string;
                } else {
                    addToSexpOnTop(string);
                }
                break;
            case '\\':
                currentChar = nextChar();
                switch(currentChar) {
                    case 'b':
                        stringBuffer.append('\b');
                        break;
                    case 'f':
                        stringBuffer.append('\f');
                        break;
                    case 'n':
                        stringBuffer.append('\n');
                        break;
                    case 'r':
                        stringBuffer.append('\r');
                        break;
                    case 't':
                        stringBuffer.append('\t');
                        break;
                    default:
                        stringBuffer.append(currentChar);
                }
                break;
            default:
                stringBuffer.append(currentChar);
        }
    }

    private void handleComment(char currentChar) {
        if (currentChar == '\n' || currentChar == '\r') {
            state = STATE_INIT;
        }
    }
}
