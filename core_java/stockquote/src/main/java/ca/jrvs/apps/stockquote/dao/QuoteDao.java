package ca.jrvs.apps.stockquote.dao;

import ca.jrvs.apps.stockquote.model.Position;
import ca.jrvs.apps.stockquote.model.Quote;
import ca.jrvs.apps.stockquote.dao.CrudDao;
import java.util.Optional;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class QuoteDao implements CrudDao<Quote, String> {

    private final Connection connection;

    // Constructor to initialize database connection
    public QuoteDao(Connection connection) {
        this.connection = connection;
    }

    @Override
    public Quote save(Quote quote) throws IllegalArgumentException {
        if (quote == null || quote.getSymbol() == null) {
            throw new IllegalArgumentException("Quote or ID cannot be null");
        }

        // Ensure timestamp is never null before saving to DB
        if (quote.getTimestamp() == null) {
            quote.setTimestamp(new Timestamp(System.currentTimeMillis())); // Auto-set current timestamp
        }


        String insertOrUpdateSql = "INSERT INTO quote (symbol, open, high, low, price, volume, latest_trading_day, previous_close, change, change_percent, timestamp) " +
                "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) " +
                "ON CONFLICT (symbol) DO UPDATE SET " +
                "open = EXCLUDED.open, high = EXCLUDED.high, low = EXCLUDED.low, price = EXCLUDED.price, " +
                "volume = EXCLUDED.volume, latest_trading_day = EXCLUDED.latest_trading_day, previous_close = EXCLUDED.previous_close, " +
                "change = EXCLUDED.change, change_percent = EXCLUDED.change_percent, timestamp = EXCLUDED.timestamp";

        try (PreparedStatement stmt = connection.prepareStatement(insertOrUpdateSql)) {
            stmt.setString(1, quote.getSymbol());
            stmt.setDouble(2, quote.getOpen());
            stmt.setDouble(3, quote.getHigh());
            stmt.setDouble(4, quote.getLow());
            stmt.setDouble(5, quote.getPrice());
            stmt.setInt(6, quote.getVolume());
            stmt.setDate(7, new Date(quote.getLatestTradingDay().getTime()));
            stmt.setDouble(8, quote.getPreviousClose());
            stmt.setDouble(9, quote.getChange());
            stmt.setString(10, quote.getChangePercent());
            stmt.setTimestamp(11, quote.getTimestamp());

            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to save Quote", e);
        }
        return quote;
    }

    @Override
    public Optional<Quote> findById(String ticker) throws IllegalArgumentException {
        if (ticker == null || ticker.isEmpty()) {
            throw new IllegalArgumentException("Ticker cannot be null or empty");
        }

        String sql = "SELECT * FROM quote WHERE symbol = ?";
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, ticker);
            ResultSet rs = stmt.executeQuery();

            if (rs.next()) {
                Quote quote = mapResultSetToQuote(rs);
                return Optional.of(quote);
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to find Quote by ID", e);
        }
        return Optional.empty();
    }

    @Override
    public Iterable<Quote> findAll() {
        List<Quote> quotes = new ArrayList<>();
        String sql = "SELECT * FROM quote";

        try (Statement stmt = connection.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            while (rs.next()) {
                quotes.add(mapResultSetToQuote(rs));
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to retrieve all Quotes", e);
        }
        return quotes;
    }

    @Override
    public void deleteById(String ticker) throws IllegalArgumentException {
        if (ticker == null || ticker.isEmpty()) {
            throw new IllegalArgumentException("Ticker cannot be null or empty");
        }

        String sql = "DELETE FROM quote WHERE symbol = ?";
        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            stmt.setString(1, ticker);
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete Quote by ID", e);
        }
    }

    @Override
    public void deleteAll() {
        String sql = "DELETE FROM quote";
        try (Statement stmt = connection.createStatement()) {
            stmt.executeUpdate(sql);
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete all Quotes", e);
        }
    }

    private Quote mapResultSetToQuote(ResultSet rs) throws SQLException {
        return new Quote(
                rs.getString("symbol"),
                rs.getDouble("open"),
                rs.getDouble("high"),
                rs.getDouble("low"),
                rs.getDouble("price"),
                rs.getInt("volume"),
                rs.getDate("latest_trading_day"),
                rs.getDouble("previous_close"),
                rs.getDouble("change"),
                rs.getString("change_percent"),
                rs.getTimestamp("timestamp")
        );
    }
}
