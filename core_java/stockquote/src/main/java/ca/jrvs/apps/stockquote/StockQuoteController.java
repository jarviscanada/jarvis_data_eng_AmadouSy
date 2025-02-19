package ca.jrvs.apps.stockquote.controller;

import ca.jrvs.apps.stockquote.service.PositionService;
import ca.jrvs.apps.stockquote.service.QuoteService;
import ca.jrvs.apps.stockquote.model.Position;
import ca.jrvs.apps.stockquote.model.Quote;

import java.util.Optional;
import java.util.Scanner;

public class StockQuoteController {

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
        while (true) {
            System.out.println("\nWelcome to Stock Quote App!");
            System.out.println("1 - View Stock Quote");
            System.out.println("2 - Buy Stock");
            System.out.println("3 - Sell Stock");
            System.out.println("4 - View Portfolio");
            System.out.println("5 - Exit");
            System.out.print("Enter your choice: ");

            String choice = scanner.nextLine().trim();

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
                    System.out.println("Exiting application...");
                    return;
                default:
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

        Optional<Quote> quote = quoteService.fetchQuoteDataFromAPI(ticker);
        if (quote.isPresent()) {
            System.out.println("Stock Quote: " + quote.get());
        } else {
            System.out.println("Failed to retrieve stock quote for " + ticker);
        }
    }

    /**
     * Buy a stock and update the portfolio.
     */
    private void buyStock(Scanner scanner) {
        System.out.print("Enter stock ticker symbol: ");
        String ticker = scanner.nextLine().toUpperCase().trim();

        System.out.print("Enter number of shares to buy: ");
        int numShares;
        try {
            numShares = Integer.parseInt(scanner.nextLine().trim());
        } catch (NumberFormatException e) {
            System.out.println("Invalid number of shares! Please enter a valid integer.");
            return;
        }

        Optional<Quote> quote = quoteService.findQuoteInDatabase(ticker);
        if (quote.isEmpty()) {
            System.out.println("Stock quote not found in database. Fetching from API...");
            quote = quoteService.fetchQuoteDataFromAPI(ticker);
            if (quote.isEmpty()) {
                System.out.println("Failed to fetch stock quote. Cannot proceed with purchase.");
                return;
            }
        }

        double price = quote.get().getPrice();
        Position position = positionService.buy(ticker, numShares, price);
        System.out.println("Successfully bought " + numShares + " shares of " + ticker);
        System.out.println("Updated position: " + position);
    }

    /**
     * Sell all shares of a stock.
     */
    private void sellStock(Scanner scanner) {
        System.out.print("Enter stock ticker symbol to sell: ");
        String ticker = scanner.nextLine().toUpperCase().trim();

        positionService.sell(ticker);
        System.out.println("Successfully sold all shares of " + ticker);
    }

    /**
     * View portfolio (current stock holdings).
     */
    private void viewPortfolio() {
        // Assuming we can fetch all positions from the database (modify as per your implementation)
        System.out.println("Portfolio feature is not yet implemented.");
    }
}
