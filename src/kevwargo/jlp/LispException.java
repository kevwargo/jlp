package kevwargo.jlp;

public class LispException extends Exception {

    public LispException(String fmt, Object ...args) {
        super(String.format(fmt, args));
    }

}
