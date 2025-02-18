package ca.jrvs.apps.stockquote.dao;

import ca.jrvs.apps.stockquote.model.Position;
import ca.jrvs.apps.stockquote.model.Quote;
import ca.jrvs.apps.stockquote.dao.CrudDao;
import java.util.Optional;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;


public class PositionDao implements CrudDao<Position, String> {

    private final Connection connection;

    // Constructor to inject the database connection
    public PositionDao(Connection connection) {
        this.connection = connection;
    }

    @Override
    public Position save(Position position) throws IllegalArgumentException {
        if (position == null || position.getTicker() == null) {
            throw new IllegalArgumentException("Position or ticker must not be null");
        }

        String upsertQuery = "INSERT INTO position (symbol, number_of_shares, value_paid) " +
                "VALUES (?, ?, ?) ON CONFLICT (symbol) " +
                "DO UPDATE SET number_of_shares = EXCLUDED.number_of_shares, value_paid = EXCLUDED.value_paid";

        try (PreparedStatement stmt = connection.prepareStatement(upsertQuery)) {
            stmt.setString(1, position.getTicker());
            stmt.setInt(2, position.getNumOfShares());
            stmt.setDouble(3, position.getValuePaid());
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to save position: " + position.getTicker(), e);
        }

        return position;
    }

    @Override
    public Optional<Position> findById(String ticker) throws IllegalArgumentException {
        if (ticker == null || ticker.isEmpty()) {
            throw new IllegalArgumentException("Ticker must not be null or empty");
        }

        String selectQuery = "SELECT * FROM position WHERE symbol = ?";
        try (PreparedStatement stmt = connection.prepareStatement(selectQuery)) {
            stmt.setString(1, ticker);
            ResultSet rs = stmt.executeQuery();

            if (rs.next()) {
                Position position = new Position(
                        rs.getString("symbol"),
                        rs.getInt("number_of_shares"),
                        rs.getDouble("value_paid")
                );
                return Optional.of(position);
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to find position with ticker: " + ticker, e);
        }

        return Optional.empty();
    }

    @Override
    public Iterable<Position> findAll() {
        List<Position> positions = new ArrayList<>();
        String selectQuery = "SELECT * FROM position";

        try (PreparedStatement stmt = connection.prepareStatement(selectQuery);
             ResultSet rs = stmt.executeQuery()) {

            while (rs.next()) {
                positions.add(new Position(
                        rs.getString("symbol"),
                        rs.getInt("number_of_shares"),
                        rs.getDouble("value_paid")
                ));
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to retrieve all positions", e);
        }

        return positions;
    }

    @Override
    public void deleteById(String ticker) throws IllegalArgumentException {
        if (ticker == null || ticker.isEmpty()) {
            throw new IllegalArgumentException("Ticker must not be null or empty");
        }

        String deleteQuery = "DELETE FROM position WHERE symbol = ?";
        try (PreparedStatement stmt = connection.prepareStatement(deleteQuery)) {
            stmt.setString(1, ticker);
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete position with ticker: " + ticker, e);
        }
    }

    @Override
    public void deleteAll() {
        String deleteAllQuery = "DELETE FROM position";
        try (PreparedStatement stmt = connection.prepareStatement(deleteAllQuery)) {
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete all positions", e);
        }
    }
}
