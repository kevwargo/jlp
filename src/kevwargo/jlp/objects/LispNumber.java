package kevwargo.jlp.objects;


public class LispNumber extends LispDataObject {

    private double doubleValue;
    private long longValue;
    private boolean floatp;

    public LispNumber(double value) {
        doubleValue = value;
        floatp = true;
    }

    public LispNumber(long value) {
        longValue = value;
        floatp = false;
    }

    public LispNumber(String value) throws NumberFormatException {
        try {
            longValue = Long.parseLong(value);
            floatp = false;
        } catch (NumberFormatException nfe) {
            doubleValue = Double.parseDouble(value);
            floatp = true;
        }
    }

    public String toString() {
        if (floatp) {
            return Double.toString(doubleValue);
        } else {
            return Long.toString(longValue);
        }
    }

    public String type() {
        return "number";
    }

}
