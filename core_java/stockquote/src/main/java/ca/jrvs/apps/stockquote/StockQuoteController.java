package ca.jrvs.apps.stockquote.controller;

import ca.jrvs.apps.stockquote.service.PositionService;
import ca.jrvs.apps.stockquote.service.QuoteService;
import ca.jrvs.apps.stockquote.model.Position;
import ca.jrvs.apps.stockquote.model.Quote;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;
import java.util.Scanner;

public class StockQuoteController {

    private static final Logger logger = LoggerFactory.getLogger(StockQuoteController.class);

    private final QuoteService quoteService;
    private final PositionService positionService;

    public StockQuoteController(QuoteService quoteService, PositionService positionService) {
        this.quoteService = quoteService;
        this.positionService = positionService;
    }

    /**
     * User interface for our application
     */
    public void initClient() {
        Scanner scanner = new Scanner(System.in);
        logger.info("StockQuoteController started.");

        while (true) {
            System.out.println("\nWelcome to Stock Quote App!");
            System.out.println("1 - View Stock Quote");
            System.out.println("2 - Buy Stock");
            System.out.println("3 - Sell Stock");
            System.out.println("4 - View Portfolio");
            System.out.println("5 - Exit");
            System.out.print("Enter your choice: ");

            String choice = scanner.nextLine().trim();
            logger.info("User selected option: {}", choice);

            switch (choice) {
                case "1":
                    viewStockQuote(scanner);
                    break;
                case "2":
                    buyStock(scanner);
                    break;
                case "3":
                    sellStock(scanner);
                    break;
                case "4":
                    viewPortfolio();
                    break;
                case "5":
                    logger.info("Exiting application...");
                    System.out.println("Exiting application...");
                    return;
                default:
                    logger.warn("Invalid input received: {}", choice);
                    System.out.println("Invalid input! Please enter a valid option.");
            }
        }
    }

    /**
     * Fetch and display the latest stock quote.
     */
    private void viewStockQuote(Scanner scanner) {
        System.out.print("Enter stock ticker symbol: ");
        String ticker = scanner.nextLine().toUpperCase().trim();
        logger.info("Fetching stock quote for: {}", ticker);

        Optional<Quote> quote = quoteService.fetchQuoteDataFromAPI(ticker);
        if (quote.isPresent()) {
            logger.info("Stock quote retrieved: {}", quote.get());
            System.out.println("Stock Quote: " + quote.get());
        } else {
            logger.error("Failed to retrieve stock quote for {}", ticker);
            System.out.println("Failed to retrieve stock quote for " + ticker);
        }
    }

    /**
     * Buy a stock and update the portfolio.
     */
    private void buyStock(Scanner scanner) {
        System.out.print("Enter stock ticker symbol: ");
        String ticker = scanner.nextLine().toUpperCase().trim();
        logger.info("User is attempting to buy stock: {}", ticker);

        System.out.print("Enter number of shares to buy: ");
        int numShares;
        try {
            numShares = Integer.parseInt(scanner.nextLine().trim());
            logger.info("Number of shares to buy: {}", numShares);
        } catch (NumberFormatException e) {
            logger.error("Invalid number of shares entered", e);
            System.out.println("Invalid number of shares! Please enter a valid integer.");
            return;
        }

        Optional<Quote> quote = quoteService.findQuoteInDatabase(ticker);
        if (quote.isEmpty()) {
            logger.warn("Stock quote not found in database for {}", ticker);
            System.out.println("Stock quote not found in database. Fetching from API...");
            quote = quoteService.fetchQuoteDataFromAPI(ticker);
            if (quote.isEmpty()) {
                logger.error("Failed to fetch stock quote from API for {}", ticker);
                System.out.println("Failed to fetch stock quote. Cannot proceed with purchase.");
                return;
            }
        }

        double price = quote.get().getPrice();
        logger.info("Buying {} shares of {} at price {}", numShares, ticker, price);
        Position position = positionService.buy(ticker, numShares, price);
        logger.info("Stock purchase successful: {}", position);
        System.out.println("Successfully bought " + numShares + " shares of " + ticker);
        System.out.println("Updated position: " + position);
    }

    /**
     * Sell all shares of a stock.
     */
    private void sellStock(Scanner scanner) {
        System.out.print("Enter stock ticker symbol to sell: ");
        String ticker = scanner.nextLine().toUpperCase().trim();
        logger.info("User is attempting to sell stock: {}", ticker);

        positionService.sell(ticker);
        logger.info("Successfully sold all shares of {}", ticker);
        System.out.println("Successfully sold all shares of " + ticker);
    }

    /**
     * View portfolio (current stock holdings).
     */
    private void viewPortfolio() {
        logger.info("User attempted to view portfolio.");
        System.out.println("Portfolio feature is not yet implemented.");
    }
}
