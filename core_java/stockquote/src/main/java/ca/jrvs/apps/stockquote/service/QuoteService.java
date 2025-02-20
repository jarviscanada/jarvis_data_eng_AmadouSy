package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.dao.QuoteDao;
import ca.jrvs.apps.stockquote.http.QuoteHttpHelper;
import ca.jrvs.apps.stockquote.model.Quote;

import java.util.Optional;

public class QuoteService {

    private final QuoteDao quoteDao;
    private final QuoteHttpHelper quoteHttpHelper;

    /**
     * Constructor to initialize QuoteService with dependencies.
     * @param quoteDao DAO for accessing quote data.
     * @param quoteHttpHelper Helper for fetching quote data from API.
     */
    public QuoteService(QuoteDao quoteDao, QuoteHttpHelper quoteHttpHelper) {
        this.quoteDao = quoteDao;
        this.quoteHttpHelper = quoteHttpHelper;
    }

    /**
     * Fetches the latest quote data from an external API and saves it to the database.
     * @param symbol Stock ticker symbol (e.g., "AAPL").
     * @return An Optional containing the Quote if successfully fetched, otherwise empty.
     * @throws IllegalArgumentException If the ticker is invalid.
     */
    public Optional<Quote> fetchQuoteDataFromAPI(String symbol) {
        try {
            // Fetch quote from API
            Quote quote = quoteHttpHelper.fetchQuoteInfo(symbol);
            if (quote == null) {
                System.out.println("Failed to fetch quote data for symbol: " + symbol);
                return Optional.empty();
            }

            // Save to database
            quoteDao.save(quote);
            return Optional.of(quote);
        } catch (Exception e) {
            System.out.println("Exception while fetching quote: " + e.getMessage());
            e.printStackTrace();
            return Optional.empty();
        }
    }

    /**
     * Retrieves a quote from the database by ticker symbol.
     * @param ticker Stock ticker symbol.
     * @return An Optional containing the Quote if found, otherwise empty.
     * @throws IllegalArgumentException If the ticker is null or empty.
     */
    public Optional<Quote> findQuoteInDatabase(String ticker) {
        if (ticker == null || ticker.trim().isEmpty()) {
            throw new IllegalArgumentException("Ticker cannot be null or empty.");
        }
        return quoteDao.findById(ticker);
    }
}
