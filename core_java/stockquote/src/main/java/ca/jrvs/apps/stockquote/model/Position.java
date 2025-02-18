package ca.jrvs.apps.stockquote.model;

public class Position {

    private String ticker; // id
    private int numOfShares;
    private double valuePaid; // total amount paid for shares

    // Default constructor
    public Position() {
    }

    // Constructor with all fields
    public Position(String ticker, int numOfShares, double valuePaid) {
        this.ticker = ticker;
        this.numOfShares = numOfShares;
        this.valuePaid = valuePaid;
    }

    // Getters and Setters
    public String getTicker() {
        return ticker;
    }

    public void setTicker(String ticker) {
        this.ticker = ticker;
    }

    public int getNumOfShares() {
        return numOfShares;
    }

    public void setNumOfShares(int numOfShares) {
        this.numOfShares = numOfShares;
    }

    public double getValuePaid() {
        return valuePaid;
    }

    public void setValuePaid(double valuePaid) {
        this.valuePaid = valuePaid;
    }

    // toString method for better readability
    @Override
    public String toString() {
        return "{\n" +
                "  \"Position\": {\n" +
                "    \"symbol\": \"" + ticker + "\",\n" +
                "    \"number_of_shares\": \"" + numOfShares + "\",\n" +
                "    \"value_paid\": \"" + valuePaid + "\"\n" +
                "  }\n" +
                "}";
    }
}
