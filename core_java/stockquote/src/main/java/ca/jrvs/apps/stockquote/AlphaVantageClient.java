package ca.jrvs.apps.stockquote;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class AlphaVantageClient {

    private static final String API_KEY = "07f59cff42mshbd937a6d205b307p1367f3jsnc1dac0844937";
    private static final String BASE_URL = "https://alpha-vantage.p.rapidapi.com/query";
    private static final String HOST = "alpha-vantage.p.rapidapi.com";

    /**
     * Method to fetch stock data for a given symbol
     */
    public String getStockQuote(String symbol) throws IOException, InterruptedException {
        String url = BASE_URL + "?function=GLOBAL_QUOTE&symbol=" + symbol + "&datatype=json";

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://alpha-vantage.p.rapidapi.com/query?function=GLOBAL_QUOTE&symbol=MSFT&datatype=json"))
                .header("X-RapidAPI-Key", API_KEY)
                .header("X-RapidAPI-Host", "alpha-vantage.p.rapidapi.com")
                .GET()
                .build();

        HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());

        // Check if response is valid
        if (response.body().contains("Global Quote")) {
            return response.body();
        } else {
            return "Error: Invalid response received from API. Check API key and request parameters.";
        }
    }

    /**
     * Main method to test API
     */
    public static void main(String[] args) {
        AlphaVantageClient client = new AlphaVantageClient();
        String symbol = "MSFT"; // Microsoft stock symbol

        try {
            String response = client.getStockQuote(symbol);
            System.out.println("API Response for " + symbol + ":\n" + response);
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
