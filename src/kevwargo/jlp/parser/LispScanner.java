package kevwargo.jlp.parser;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Scanner;
import java.util.regex.MatchResult;
import java.util.regex.Pattern;


public class LispScanner {

    private Scanner scanner;
    private Pattern regex;
    private Pattern inString;

    public LispScanner(String filename) throws IOException {
        this(new FileInputStream(filename));
    }

    public LispScanner(InputStream stream) {
        scanner = new Scanner(stream);

        StringBuffer sb = new StringBuffer();
        sb.append("(");
        sb.append("(\\()|");
        sb.append("(\\))|");
        sb.append("('|`|,@?)|");
        sb.append("(\")|");
        sb.append("([ \n\r\t]+)|");
        sb.append("(;.*(\n|\r\n))|");
        sb.append("([^()\n\t\r '\";,`]+)");
        sb.append(")");
        regex = Pattern.compile(sb.toString());

        sb = new StringBuffer();
        sb.append("(");
        sb.append("([^\\\\\"]+)|");
        sb.append("(\\\\n)|");
        sb.append("(\\\\r)|");
        sb.append("(\\\\t)|");
        sb.append("(\\\\\\\\)|");
        sb.append("(\\\\\")|");
        sb.append("(\")");
        sb.append(")");
        inString = Pattern.compile(sb.toString());
    }

    public LispToken nextLispToken() throws LispScanException {
        String token;
        while ((token = scanner.findWithinHorizon(regex, 0)) != null) {
            MatchResult m = scanner.match();
            if (m.group(2) != null) {
                return new LispToken(LispToken.Type.OPEN_PAREN, token);
            } else if (m.group(3) != null) {
                return new LispToken(LispToken.Type.CLOSE_PAREN, token);
            } else if (m.group(4) != null) {
                return new LispToken(LispToken.Type.SPECIAL, token);
            } else if (m.group(5) != null) {
                StringBuffer sb = new StringBuffer();
                boolean terminated = false;
                while ((token = scanner.findWithinHorizon(inString, 0)) != null) {
                    m = scanner.match();
                    if (m.group(2) != null) {
                        sb.append(token);
                    } else if (m.group(3) != null) {
                        sb.append('\n');
                    } else if (m.group(4) != null) {
                        sb.append('\r');
                    } else if (m.group(5) != null) {
                        sb.append('\t');
                    } else if (m.group(6) != null) {
                        sb.append('\\');
                    } else if (m.group(7) != null) {
                        sb.append('"');
                    } else if (m.group(8) != null) {
                        terminated = true;
                        break;
                    }
                }
                if (!terminated) {
                    throw new LispScanException("Unterminated string literal.");
                } else {
                    return new LispToken(LispToken.Type.STRING, sb.toString());
                }
            } else if (m.group(9) != null) {
                return new LispToken(LispToken.Type.SYMBOL, token);
            }
        }
        return null;
    }

}
