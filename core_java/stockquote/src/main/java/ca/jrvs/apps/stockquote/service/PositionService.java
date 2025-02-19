package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.dao.PositionDao;
import ca.jrvs.apps.stockquote.model.Position;

public class PositionService {

    private PositionDao dao;

    public PositionService(PositionDao dao) {
        this.dao = dao;
    }

    /**
     * Processes a buy order and updates the database accordingly.
     * @param ticker Stock ticker symbol
     * @param numberOfShares Number of shares to buy
     * @param price Price per share
     * @return The updated position in the database
     */
    public Position buy(String ticker, int numberOfShares, double price) {
        // Check if the position already exists
        Position position = dao.findById(ticker).orElse(null);

        if (position != null) {
            // Update the number of shares
            position.setNumOfShares(position.getNumOfShares() + numberOfShares);
        } else {
            // Create a new position
            position = new Position(ticker, numberOfShares, price);
        }

        // Save the updated position in the database
        return dao.save(position);
    }

    /**
     * Sells all shares of the given ticker symbol.
     * @param ticker Stock ticker symbol
     */
    public void sell(String ticker) {
        dao.deleteById(ticker);
    }
}
