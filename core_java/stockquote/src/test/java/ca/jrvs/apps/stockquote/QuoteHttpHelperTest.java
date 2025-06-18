package ca.jrvs.apps.stockquote;

import okhttp3.Call;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class QuoteHttpHelperTest {

    private QuoteHttpHelper quoteHttpHelper;

    @Mock
    private OkHttpClient mockClient;

    @Mock
    private Call mockCall;

    @Mock
    private Response mockResponse;

    @Mock
    private ResponseBody mockResponseBody;

    private static final String MOCK_JSON_RESPONSE =
            "{ \"Global Quote\": { \"01. symbol\": \"AAPL\", \"05. price\": \"150.50\" } }";

    @BeforeEach
    void setUp() throws IOException {
        MockitoAnnotations.openMocks(this);
        quoteHttpHelper = new QuoteHttpHelper("mock-api-key", mockClient);

        // Mock HTTP call behavior
        when(mockClient.newCall(any(Request.class))).thenReturn(mockCall);
        when(mockCall.execute()).thenReturn(mockResponse);
        when(mockResponse.isSuccessful()).thenReturn(true);
        when(mockResponse.body()).thenReturn(mockResponseBody);
        when(mockResponseBody.string()).thenReturn(MOCK_JSON_RESPONSE);
    }

    @Test
    void testFetchQuoteInfo_Success() throws IOException {
        // Fetch a quote
        Quote quote = quoteHttpHelper.fetchQuoteInfo("AAPL");

        // Validate results
        assertNotNull(quote, "Quote should not be null");
        assertEquals("AAPL", quote.getSymbol(), "Symbol should match");
        assertEquals(150.50, quote.getPrice(), "Price should match mock response");
    }

    @Test
    void testFetchQuoteInfo_InvalidSymbol() {
        // Ensure exception is thrown for an invalid symbol
        assertThrows(IllegalArgumentException.class, () -> quoteHttpHelper.fetchQuoteInfo(""),
                "Empty symbol should throw IllegalArgumentException");
    }

    @Test
    void testShutdown() {
        // Ensure shutdown does not throw exceptions
        assertDoesNotThrow(() -> quoteHttpHelper.shutdown(), "Shutdown should not throw exceptions");
    }
}
