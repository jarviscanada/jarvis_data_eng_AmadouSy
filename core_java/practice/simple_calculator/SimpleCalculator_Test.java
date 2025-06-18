import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SimpleCalculator_Test {

    SimpleCalculator calculator;

    @BeforeEach
    void init() {
        // Initialize the calculator before each test
        calculator = new SimpleCalculatorImpl();
    }

    @Test
    void test_add() {
        int expected = 2;
        int actual = calculator.add(1, 1);
        // Verify that the addition gives the correct result
        assertEquals(expected, actual);
    }

    @Test
    void test_subtract() {
        int expected = 4;
        int actual = calculator.subtract(9, 5); // Fixed the method name typo
        // Verify that the subtraction gives the correct result
        assertEquals(expected, actual); // Fixed the comparison
    }

    @Test
    void test_multiply() {
        int expected = 20;
        int actual = calculator.multiply(4, 5); // Multiplication test
        // Verify that the multiplication gives the correct result
        assertEquals(expected, actual);
    }

    @Test
    void test_divide() {
        double expected = 3.0;
        double actual = calculator.divide(6, 2); // Division test
        // Verify that the division gives the correct result
        assertEquals(expected, actual);
    }

}
