package kevwargo.jlp.object;


public class LispFloat extends LispDataObject {

    private double value;

    public LispFloat(double value) {
        this.value = value;
    }

    public double getValue() {
        return this.value;
    }

    public void setValue(double value) {
        this.value = value;
    }

    public String toString() {
        return String.format("%f", value);
    }
}
