package ca.jrvs.apps.stockquote;

import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;

public class QuoteHttpHelper {

    private static final String BASE_URL = "https://www.alphavantage.co/query";
    private static final String FUNCTION = "GLOBAL_QUOTE";

    private String apiKey;
    private OkHttpClient client;
    private ObjectMapper objectMapper;

    /**
     * Constructor to initialize API key and OkHttpClient
     * @param apiKey - Alpha Vantage API key
     */
    public QuoteHttpHelper(String apiKey) {
        this.apiKey = apiKey;
        this.client = new OkHttpClient();
        this.objectMapper = new ObjectMapper();
    }

    /**
     * Fetch the latest quote data from Alpha Vantage API
     * @param symbol - stock ticker symbol
     * @return Quote with latest data
     * @throws IllegalArgumentException - if no data was found for the given symbol
     */
    public Quote fetchQuoteInfo(String symbol) throws IllegalArgumentException {
        // Validate ticker symbol format
        if (symbol == null || symbol.trim().isEmpty() || !symbol.matches("^[A-Z]{1,5}$")) {
            throw new IllegalArgumentException("Invalid ticker symbol: " + symbol);
        }

        // Build request URL
        HttpUrl url = HttpUrl.parse(BASE_URL).newBuilder()
                .addQueryParameter("function", FUNCTION)
                .addQueryParameter("symbol", symbol)
                .addQueryParameter("apikey", apiKey)
                .build();

        // Create an HTTP GET request
        Request request = new Request.Builder().url(url).build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Unexpected HTTP response: " + response);
            }

            // Read response body
            String responseBody = response.body().string();
            if (responseBody.isEmpty()) {
                throw new IllegalArgumentException("No data found for symbol: " + symbol);
            }

            // Parse JSON response
            JsonNode rootNode = objectMapper.readTree(responseBody);
            JsonNode quoteNode = rootNode.get("Global Quote");

            if (quoteNode == null || quoteNode.isEmpty()) {
                throw new IllegalArgumentException("No quote data found for symbol: " + symbol);
            }

            // Deserialize JSON to Quote object
            return objectMapper.treeToValue(quoteNode, Quote.class);

        } catch (IOException e) {
            throw new RuntimeException("Failed to fetch quote info for symbol: " + symbol, e);
        }
    }

    /**
     * Main method for testing
     */
    public static void main(String[] args) {
        String apiKey = "07f59cff42mshbd937a6d205b307p1367f3jsnc1dac0844937";  // Replace with your Alpha Vantage API key
        QuoteHttpHelper helper = new QuoteHttpHelper(apiKey);

        try {
            Quote quote = helper.fetchQuoteInfo("AAPL");
            System.out.println("Stock Quote: " + quote);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}
