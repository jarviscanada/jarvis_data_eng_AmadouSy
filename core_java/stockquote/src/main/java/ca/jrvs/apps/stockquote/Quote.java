package ca.jrvs.apps.stockquote;

import com.fasterxml.jackson.annotation.JsonProperty;

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
    private long volume;

    @JsonProperty("07. latest trading day")
    private String latestTradingDay;

    @JsonProperty("08. previous close")
    private double previousClose;

    @JsonProperty("09. change")
    private double change;

    @JsonProperty("10. change percent")
    private String changePercent;

    // Getters and Setters
    public String getSymbol() { return symbol; }
    public void setSymbol(String symbol) { this.symbol = symbol; }

    public double getOpen() { return open; }
    public void setOpen(double open) { this.open = open; }

    public double getHigh() { return high; }
    public void setHigh(double high) { this.high = high; }

    public double getLow() { return low; }
    public void setLow(double low) { this.low = low; }

    public double getPrice() { return price; }
    public void setPrice(double price) { this.price = price; }

    public long getVolume() { return volume; }
    public void setVolume(long volume) { this.volume = volume; }

    public String getLatestTradingDay() { return latestTradingDay; }
    public void setLatestTradingDay(String latestTradingDay) { this.latestTradingDay = latestTradingDay; }

    public double getPreviousClose() { return previousClose; }
    public void setPreviousClose(double previousClose) { this.previousClose = previousClose; }

    public double getChange() { return change; }
    public void setChange(double change) { this.change = change; }

    public String getChangePercent() { return changePercent; }
    public void setChangePercent(String changePercent) { this.changePercent = changePercent; }

    // Override toString() for readable output
    @Override
    public String toString() {
        return "Quote{" +
                "symbol='" + symbol + '\'' +
                ", open=" + open +
                ", high=" + high +
                ", low=" + low +
                ", price=" + price +
                ", volume=" + volume +
                ", latestTradingDay='" + latestTradingDay + '\'' +
                ", previousClose=" + previousClose +
                ", change=" + change +
                ", changePercent='" + changePercent + '\'' +
                '}';
    }
}
