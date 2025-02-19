package ca.jrvs.apps.stockquote;

import ca.jrvs.apps.stockquote.config.ConfigLoader;
import ca.jrvs.apps.stockquote.dao.QuoteDao;
import ca.jrvs.apps.stockquote.dao.PositionDao;
import ca.jrvs.apps.stockquote.http.QuoteHttpHelper;
import ca.jrvs.apps.stockquote.service.QuoteService;
import ca.jrvs.apps.stockquote.service.PositionService;
import ca.jrvs.apps.stockquote.controller.StockQuoteController;
import okhttp3.OkHttpClient;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class Main {
    public static void main(String[] args) {
        // Charger les propriétés depuis le fichier
        ConfigLoader config = new ConfigLoader("src/resources/properties.txt");

        try {
            // Charger le driver PostgreSQL
            Class.forName(config.getProperty("db-class"));

            // Connexion à la base de données
            String url = "jdbc:postgresql://" + config.getProperty("server") + ":" +
                    config.getProperty("port") + "/" + config.getProperty("database");
            Connection conn = DriverManager.getConnection(url, config.getProperty("username"), config.getProperty("password"));

            // Initialisation des composants
            QuoteDao quoteDao = new QuoteDao(conn);
            PositionDao positionDao = new PositionDao(conn);
            QuoteHttpHelper quoteHttpHelper = new QuoteHttpHelper("07f59cff42mshbd937a6d205b307p1367f3jsnc1dac0844937");

            QuoteService quoteService = new QuoteService(quoteDao, quoteHttpHelper);
            PositionService positionService = new PositionService(positionDao);

            StockQuoteController controller = new StockQuoteController(quoteService, positionService);
            controller.initClient(); // Démarrer l?application

        } catch (ClassNotFoundException | SQLException e) {
            e.printStackTrace();
        }
    }
}
