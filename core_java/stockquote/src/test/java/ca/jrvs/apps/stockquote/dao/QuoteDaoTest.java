package ca.jrvs.apps.stockquote.dao;

import ca.jrvs.apps.stockquote.model.Quote;
import org.junit.jupiter.api.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class QuoteDaoTest {

    private QuoteDao quoteDao;
    private Connection connection;

    @BeforeAll
    void setup() throws SQLException {
        // Connexion à PostgreSQL
        String url = "jdbc:postgresql://localhost:5432/stock_quote";
        String user = "your_username"; // Remplace par ton utilisateur PostgreSQL
        String password = "your_password"; // Remplace par ton mot de passe PostgreSQL

        connection = DriverManager.getConnection(url, user, password);
        quoteDao = new QuoteDao(connection);
    }

    @Test
    @Order(1)
    void saveTest() {
        Quote quote = new Quote();
        quote.setTicker("AAPL");
        quote.setOpen(150.0);
        quote.setHigh(155.0);
        quote.setLow(148.0);
        quote.setPrice(152.0);
        quote.setVolume(1_000_000);

        Quote savedQuote = quoteDao.save(quote);

        assertNotNull(savedQuote);
        assertEquals("AAPL", savedQuote.getTicker());
    }

    @Test
    @Order(2)
    void findByIdTest() {
        Optional<Quote> foundQuote = quoteDao.findById("AAPL");

        assertTrue(foundQuote.isPresent());
        assertEquals(152.0, foundQuote.get().getPrice());
    }

    @Test
    @Order(3)
    void findAllTest() {
        Iterable<Quote> quotes = quoteDao.findAll();
        assertNotNull(quotes);
    }

    @Test
    @Order(4)
    void deleteByIdTest() {
        quoteDao.deleteById("AAPL");
        Optional<Quote> foundQuote = quoteDao.findById("AAPL");

        assertFalse(foundQuote.isPresent());
    }

    @AfterAll
    void cleanup() throws SQLException {
        connection.close();
    }
}
