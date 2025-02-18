package ca.jrvs.apps.stockquote.dao;

import ca.jrvs.apps.stockquote.model.Position;
import org.junit.jupiter.api.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class PositionDaoTest {

    private PositionDao positionDao;
    private Connection connection;

    @BeforeAll
    void setup() throws SQLException {
        // Connect to PostgreSQL
        String url = "jdbc:postgresql://localhost:5432/stock_quote";
        String user = "your_username"; // Replace with your PostgreSQL username
        String password = "your_password"; // Replace with your PostgreSQL password

        connection = DriverManager.getConnection(url, user, password);
        positionDao = new PositionDao(connection);
    }

    @Test
    @Order(1)
    void saveTest() {
        Position position = new Position();
        position.setAccountId(1);
        position.setTicker("AAPL");
        position.setQuantity(50);

        Position savedPosition = positionDao.save(position);

        assertNotNull(savedPosition);
        assertEquals("AAPL", savedPosition.getTicker());
        assertEquals(50, savedPosition.getQuantity());
    }

    @Test
    @Order(2)
    void findByIdTest() {
        Optional<Position> foundPosition = positionDao.findById("AAPL");

        assertTrue(foundPosition.isPresent());
        assertEquals(50, foundPosition.get().getQuantity());
    }

    @Test
    @Order(3)
    void findAllTest() {
        Iterable<Position> positions = positionDao.findAll();
        assertNotNull(positions);
    }

    @Test
    @Order(4)
    void deleteByIdTest() {
        positionDao.deleteById("AAPL");
        Optional<Position> foundPosition = positionDao.findById("AAPL");

        assertFalse(foundPosition.isPresent());
    }


    @AfterAll
    void cleanup() throws SQLException {
        connection.close();
    }
}
