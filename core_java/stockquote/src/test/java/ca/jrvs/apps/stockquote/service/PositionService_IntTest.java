package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.dao.PositionDao;
import ca.jrvs.apps.stockquote.model.Position;
import org.junit.jupiter.api.*;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class PositionService_IntTest {

    private PositionService positionService;
    private PositionDao positionDao;
    private Connection connection;

    @BeforeAll
    void setUp() throws SQLException, NoSuchFieldException, IllegalAccessException {
        // Initialisation de la connexion à la base de données
        String url = "jdbc:postgresql://localhost:5432/stock_quote";
        String user = "your_username"; // Remplace par ton utilisateur PostgreSQL
        String password = "your_password"; // Remplace par ton mot de passe PostgreSQL
        connection = DriverManager.getConnection(url, user, password);

        // Création d'un mock de PositionDao
        positionDao = mock(PositionDao.class);

        // Instancier PositionService
        positionService = new PositionService();

        // Utiliser la réflexion pour injecter PositionDao dans PositionService
        Field daoField = PositionService.class.getDeclaredField("dao");
        daoField.setAccessible(true);
        daoField.set(positionService, positionDao);
    }

    @Test
    @Order(1)
    void testBuy() {
        // Simuler un comportement attendu de positionDao
        Position position = new Position("AAPL", 10, 150.0);

        when(positionDao.findById("AAPL")).thenReturn(Optional.empty());
        when(positionDao.save(any(Position.class))).thenReturn(position);

        // Exécuter la méthode buy()
        Position result = positionService.buy("AAPL", 10, 150.0);

        assertNotNull(result);
        assertEquals("AAPL", result.getTicker());
        assertEquals(10, result.getNumOfShares());
        assertEquals(150.0, result.getValuePaid());
    }

    @Test
    @Order(2)
    void testBuyExisting() {
        // Simuler qu'une position existe déjà
        Position existingPosition = new Position("AAPL", 10, 150.0);
        Position updatedPosition = new Position("AAPL", 15, 160.0);

        when(positionDao.findById("AAPL")).thenReturn(Optional.of(existingPosition));
        when(positionDao.save(any(Position.class))).thenReturn(updatedPosition);

        Position result = positionService.buy("AAPL", 5, 160.0);

        assertNotNull(result);
        assertEquals("AAPL", result.getTicker());
        assertEquals(15, result.getNumOfShares());
    }

    @Test
    @Order(3)
    void testSell() {
        // Simuler une suppression réussie
        doNothing().when(positionDao).deleteById("AAPL");

        positionService.sell("AAPL");

        verify(positionDao, times(1)).deleteById("AAPL");
    }

    @AfterAll
    void tearDown() throws SQLException {
        connection.close();
    }
}
