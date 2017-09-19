package kevwargo.jlp.parser;

public class LispToken {

    private Type type;
    private String value;

    public LispToken(Type type, String value) {
        this.type = type;
        this.value = value;
    }

    public Type getType() {
        return type;
    }

    public String getValue() {
        return value;
    }

    public String toString() {
        String value = this.value;
        return type.toString() + " " + value;
    }

    public enum Type {
        OPEN_PAREN,
        CLOSE_PAREN,
        SPECIAL,
        STRING,
        SYMBOL
    }

}
