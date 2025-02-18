package ca.jrvs.apps.stockquote.model;

import java.sql.Timestamp;
import java.util.Date;

public class Quote {

    private String ticker; // id
    private double open;
    private double high;
    private double low;
    private double price;
    private int volume;
    private Date latestTradingDay;
    private double previousClose;
    private double change;
    private String changePercent;
    private Timestamp timestamp; // time when the info was pulled

    // Default constructor
    public Quote() {
    }

    // Constructor with all fields
    public Quote(String ticker, double open, double high, double low, double price, int volume,
                 Date latestTradingDay, double previousClose, double change, String changePercent, Timestamp timestamp) {
        this.ticker = ticker;
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


    public String getTicker() {
        return ticker;
    }

    public void setTicker(String ticker) {
        this.ticker = ticker;
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
        this.timestamp = timestamp;
    }

    // toString method formatted as the API response
    @Override
    public String toString() {
        return "{\n" +
                "  \"Global Quote\": {\n" +
                "    \"01. symbol\": \"" + ticker + "\",\n" +
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
