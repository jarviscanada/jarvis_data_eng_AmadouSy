package ca.jrvs.apps.stockquote.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.sql.Timestamp;
import java.util.Date;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Quote {

    @JsonProperty("01. symbol")
    private String symbol;

    @JsonProperty("02. open")
    private double open;

    @JsonProperty("03. high")
    private double high;

    @JsonProperty("04. low")
    private double low;

    @JsonProperty("05. price")
    private double price;

    @JsonProperty("06. volume")
    private int volume;

    @JsonProperty("07. latest trading day")
    private Date latestTradingDay;

    @JsonProperty("08. previous close")
    private double previousClose;

    @JsonProperty("09. change")
    private double change;

    @JsonProperty("10. change percent")
    private String changePercent;

    private Timestamp timestamp; // time when the info was pulled

    // Default constructor
    public Quote() {
    }

    // Constructor with all fields (correction)
    public Quote(String symbol, double open, double high, double low, double price, int volume,
                 Date latestTradingDay, double previousClose, double change, String changePercent, Timestamp timestamp) {
        this.symbol = symbol;
        this.open = open;
        this.high = high;
        this.low = low;
        this.price = price;
        this.volume = volume;
        this.latestTradingDay = latestTradingDay;
        this.previousClose = previousClose;
        this.change = change;
        this.changePercent = changePercent;
        this.timestamp = timestamp;
    }

    // Getters and Setters
    // Alias methods for backward compatibility with older code
    public String getTicker() {
        return getSymbol();
    }

    public void setTicker(String ticker) {
        setSymbol(ticker);
    }


    public String getSymbol() {
        return symbol;
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    public double getOpen() {
        return open;
    }

    public void setOpen(double open) {
        this.open = open;
    }

    public double getHigh() {
        return high;
    }

    public void setHigh(double high) {
        this.high = high;
    }

    public double getLow() {
        return low;
    }

    public void setLow(double low) {
        this.low = low;
    }

    public double getPrice() {
        return price;
    }

    public void setPrice(double price) {
        this.price = price;
    }

    public int getVolume() {
        return volume;
    }

    public void setVolume(int volume) {
        this.volume = volume;
    }

    public Date getLatestTradingDay() {
        return latestTradingDay;
    }

    public void setLatestTradingDay(Date latestTradingDay) {
        this.latestTradingDay = latestTradingDay;
    }

    public double getPreviousClose() {
        return previousClose;
    }

    public void setPreviousClose(double previousClose) {
        this.previousClose = previousClose;
    }

    public double getChange() {
        return change;
    }

    public void setChange(double change) {
        this.change = change;
    }

    public String getChangePercent() {
        return changePercent;
    }

    public void setChangePercent(String changePercent) {
        this.changePercent = changePercent;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        if (timestamp == null) {
            this.timestamp = new Timestamp(System.currentTimeMillis()); // Set current time
        } else {
            this.timestamp = timestamp;
        }
    }

    @Override
    public String toString() {
        return "{\n" +
                "  \"Global Quote\": {\n" +
                "    \"01. symbol\": \"" + symbol + "\",\n" +
                "    \"02. open\": \"" + open + "\",\n" +
                "    \"03. high\": \"" + high + "\",\n" +
                "    \"04. low\": \"" + low + "\",\n" +
                "    \"05. price\": \"" + price + "\",\n" +
                "    \"06. volume\": \"" + volume + "\",\n" +
                "    \"07. latest trading day\": \"" + latestTradingDay + "\",\n" +
                "    \"08. previous close\": \"" + previousClose + "\",\n" +
                "    \"09. change\": \"" + change + "\",\n" +
                "    \"10. change percent\": \"" + changePercent + "\"\n" +
                "  }\n" +
                "}";
    }
}

