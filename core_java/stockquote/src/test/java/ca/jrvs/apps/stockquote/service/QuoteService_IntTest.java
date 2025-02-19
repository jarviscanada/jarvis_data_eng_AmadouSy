package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.dao.QuoteDao;
import ca.jrvs.apps.stockquote.http.QuoteHttpHelper;
import ca.jrvs.apps.stockquote.model.Quote;
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class QuoteService_IntTest {

    private QuoteService quoteService;
    private QuoteDao quoteDao;
    private QuoteHttpHelper quoteHttpHelper;
    private Connection connection;

    @BeforeAll
    void setup() throws SQLException {
        // Setup database connection
        String url = "jdbc:postgresql://localhost:5432/stock_quote"; // Remplace avec ton URL de base de données
        String user = "your_username"; // Remplace avec ton nom d'utilisateur PostgreSQL
        String password = "your_password"; // Remplace avec ton mot de passe PostgreSQL
        connection = DriverManager.getConnection(url, user, password);

        // Initialize DAO and HTTP helper
        quoteDao = new QuoteDao(connection);
        quoteHttpHelper = new QuoteHttpHelper("your_api_key"); // Remplace avec ta clé API
        quoteService = new QuoteService(quoteDao, quoteHttpHelper);
    }

    @Test
    @Order(1)
    void testFetchQuoteDataFromAPI() {
        String ticker = "AAPL";

        Optional<Quote> quote = quoteService.fetchQuoteDataFromAPI(ticker);

        assertTrue(quote.isPresent());
        assertEquals(ticker, quote.get().getTicker());
        assertTrue(quote.get().getPrice() > 0);
    }

    @Test
    @Order(2)
    void testFindQuoteInDatabase() {
        String ticker = "AAPL";

        Optional<Quote> quote = quoteService.findQuoteInDatabase(ticker);

        assertTrue(quote.isPresent());
        assertEquals(ticker, quote.get().getTicker());
    }

    @AfterAll
    void cleanup() throws SQLException {
        connection.close();
    }
}
