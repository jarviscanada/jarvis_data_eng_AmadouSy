package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.dao.QuoteDao;
import ca.jrvs.apps.stockquote.http.QuoteHttpHelper;
import ca.jrvs.apps.stockquote.model.Quote;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.Date;
import java.sql.Timestamp;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class QuoteService_UnitTest {

    @Mock
    private QuoteDao quoteDao;

    @Mock
    private QuoteHttpHelper quoteHttpHelper;

    @InjectMocks
    private QuoteService quoteService;

    private Quote sampleQuote;

    @BeforeEach
    void setUp() {
        sampleQuote = new Quote("AAPL", 150.0, 155.0, 148.0, 152.0, 1000000, new Date(), 149.0, 3.0, "+2.00%", new Timestamp(System.currentTimeMillis()));

    }

    @Test
    void testFetchQuoteDataFromAPI_Success() {
        when(quoteHttpHelper.fetchQuoteInfo("AAPL")).thenReturn(sampleQuote);
        when(quoteDao.save(any(Quote.class))).thenReturn(sampleQuote);

        Optional<Quote> result = quoteService.fetchQuoteDataFromAPI("AAPL");
        assertTrue(result.isPresent());
        assertEquals("AAPL", result.get().getTicker());
    }

    @Test
    void testFetchQuoteDataFromAPI_InvalidTicker() {
        assertThrows(IllegalArgumentException.class, () -> quoteService.fetchQuoteDataFromAPI(""));
    }

    @Test
    void testFindQuoteInDatabase_Success() {
        when(quoteDao.findById("AAPL")).thenReturn(Optional.of(sampleQuote));

        Optional<Quote> result = quoteService.findQuoteInDatabase("AAPL");
        assertTrue(result.isPresent());
        assertEquals("AAPL", result.get().getTicker());
    }

    @Test
    void testFindQuoteInDatabase_NotFound() {
        when(quoteDao.findById("AAPL")).thenReturn(Optional.empty());

        Optional<Quote> result = quoteService.findQuoteInDatabase("AAPL");
        assertFalse(result.isPresent());
    }
}
