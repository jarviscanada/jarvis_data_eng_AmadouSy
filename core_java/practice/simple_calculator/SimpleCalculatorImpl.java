public class SimpleCalculatorImpl implements SimpleCalculator {

    @Override
    public int add(int x, int y) {
<<<<<<< HEAD
        // TODO Auto-generated method stub
        return 0;
=======
        return x + y;
>>>>>>> feature/core_java
    }

    @Override
    public int subtract(int x, int y) {
<<<<<<< HEAD
        // TODO Auto-generated method stub
        return 0;
=======
        return x - y;
>>>>>>> feature/core_java
    }

    @Override
    public int multiply(int x, int y) {
<<<<<<< HEAD
        // TODO Auto-generated method stub
        return 0;
=======
        return x * y;
>>>>>>> feature/core_java
    }

    @Override
    public double divide(int x, int y) {
<<<<<<< HEAD
        // TODO Auto-generated method stub
        return 0;
=======
        if (y == 0) {
            throw new ArithmeticException("Cannot divide by zero");
        }
        return (double) x / y;
>>>>>>> feature/core_java
    }

    @Override
    public int power(int x, int y) {
<<<<<<< HEAD
        // TODO Auto-generated method stub
        return 0;
=======
        return (int) Math.pow(x, y);
>>>>>>> feature/core_java
    }

    @Override
    public double abs(double x) {
<<<<<<< HEAD
        // TODO Auto-generated method stub
        return 0;
    }

}
=======
        return Math.abs(x);
    }
}
>>>>>>> feature/core_java
