public class NotSoSimpleCalculatorImpl implements NotSoSimpleCalculator {

    private SimpleCalculator calc;

    // Constructor accepts a SimpleCalculator object to delegate basic operations
    public NotSoSimpleCalculatorImpl(SimpleCalculator calc) {
        this.calc = calc;
    }

    // Implement power method (x raised to the power of y)
    @Override
    public int power(int x, int y) {
        // If y is 0, return 1 (base case)
        if (y == 0) {
            return 1;
        }
        int result = 1;
        for (int i = 1; i <= y; i++) {
            result = calc.multiply(result, x); // Use SimpleCalculator to multiply
        }
        return result;
    }

    // Implement absolute value method
    @Override
    public int abs(int x) {
        // Using SimpleCalculator to multiply by -1 to find the absolute value
        return x < 0 ? calc.multiply(x, -1) : x;
    }

    // Implement square root method
    @Override
    public double sqrt(int x) {
        // Use Math.sqrt to return the square root of x
        return Math.sqrt(x);
    }
}
