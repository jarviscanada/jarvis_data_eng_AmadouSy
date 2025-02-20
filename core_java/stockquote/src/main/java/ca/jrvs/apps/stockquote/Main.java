package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.config.ConfigLoader;
import ca.jrvs.apps.stockquote.dao.PositionDao;
import ca.jrvs.apps.stockquote.dao.QuoteDao;
import ca.jrvs.apps.stockquote.http.QuoteHttpHelper;
import ca.jrvs.apps.stockquote.service.PositionService;
import ca.jrvs.apps.stockquote.service.QuoteService;
import ca.jrvs.apps.stockquote.controller.StockQuoteController;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Main {

    private static final Logger logger = LoggerFactory.getLogger(Main.class);

    public static void main(String[] args) {
        logger.info("Starting Stock Quote Application...");

        // Load application properties from configuration file
        ConfigLoader config = new ConfigLoader("src/resources/properties.txt");

        // Verify and load PostgreSQL driver
        try {
            logger.info("Trying to load PostgreSQL Driver...");
            Class.forName("org.postgresql.Driver");
            logger.info("PostgreSQL Driver loaded successfully!");
        } catch (ClassNotFoundException e) {
            logger.error("PostgreSQL Driver not found! Ensure the dependency is correctly set.", e);
            return;
        }

        // Construct the database connection URL
        String url = "jdbc:postgresql://" + config.getProperty("server") + ":" +
                config.getProperty("port") + "/" + config.getProperty("database");

        // Connect to PostgreSQL database
        try (Connection conn = DriverManager.getConnection(url, config.getProperty("username"), config.getProperty("password"))) {
            logger.info("Connected to PostgreSQL database successfully.");

            // Initialize application components
            QuoteDao quoteDao = new QuoteDao(conn);
            PositionDao positionDao = new PositionDao(conn);
            QuoteHttpHelper quoteHttpHelper = new QuoteHttpHelper(config.getProperty("api-key"));

            QuoteService quoteService = new QuoteService(quoteDao, quoteHttpHelper);
            PositionService positionService = new PositionService(positionDao);

            // Start the user interface controller
            StockQuoteController controller = new StockQuoteController(quoteService, positionService);
            controller.initClient();

        } catch (SQLException e) {
            logger.error("Failed to connect to PostgreSQL database. Check your connection details.", e);
        }
    }
}
