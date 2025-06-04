@ExtendWith(MockitoExtension.class)
class NotSoSimpleCalculator_Test {

    NotSoSimpleCalculator calc;

    @Mock
    SimpleCalculator mockSimpleCalc;

    @BeforeEach
    void init() {
        calc = new NotSoSimpleCalculatorImpl(mockSimpleCalc);
    }

    @Test
    void test_power() {
        //write your test here
    }

    @Test
    void test_abs() {
        //This test will currently fail
        //Consider if the provided logic in NotSoSimpleCalcualtorImpl is correct
        //Consider if you need to add anything to this test case (hint: you do)
        int expected = 10;
        int actual = calc.abs(10);
        assertEquals(expected, actual);
    }

    @Test
    void test_sqrt() {
        //write your test here
    }

}