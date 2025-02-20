package ca.jrvs.apps.stockquote.service;

import ca.jrvs.apps.stockquote.dao.PositionDao;
import ca.jrvs.apps.stockquote.model.Position;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class PositionService_IntTest {

    private PositionService positionService;

    @Mock
    private PositionDao mockDao;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        positionService = new PositionService(mockDao); // Correction ici
    }

    @Test
    void testBuy_NewPosition() {
        String ticker = "AAPL";
        int shares = 10;
        double price = 150.0;
        Position newPosition = new Position(ticker, shares, price);

        when(mockDao.findById(ticker)).thenReturn(Optional.empty());
        when(mockDao.save(any(Position.class))).thenReturn(newPosition);

        Position result = positionService.buy(ticker, shares, price);

        assertNotNull(result);
        assertEquals(ticker, result.getTicker());
        assertEquals(shares, result.getNumOfShares());
        assertEquals(price, result.getValuePaid());
        verify(mockDao).save(any(Position.class));
    }

    @Test
    void testBuy_ExistingPosition() {
        String ticker = "AAPL";
        int initialShares = 5;
        double price = 150.0;
        Position existingPosition = new Position(ticker, initialShares, price);

        when(mockDao.findById(ticker)).thenReturn(Optional.of(existingPosition));
        when(mockDao.save(any(Position.class))).thenAnswer(invocation -> invocation.getArgument(0));

        Position result = positionService.buy(ticker, 5, price);

        assertNotNull(result);
        assertEquals(ticker, result.getTicker());
        assertEquals(initialShares + 5, result.getNumOfShares());
        verify(mockDao).save(any(Position.class));
    }

    @Test
    void testSell() {
        String ticker = "AAPL";

        doNothing().when(mockDao).deleteById(ticker);

        positionService.sell(ticker);

        verify(mockDao).deleteById(ticker);
    }
}
