package ca.jrvs.apps.practice;

import java.util.stream.Collectors;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class LambdaStreamImp implements LambdaStreamExc {

    @Override
    public Stream<String> createStrStream(String... strings) {
        return Stream.of(strings); // Create the stream from the varargs
    }

    @Override
    public Stream<String> toUpperCase(String... strings) {
        return createStrStream(strings) // Create stream
                .map(String::toUpperCase);  // Convert to uppercase
    }

    @Override
    public Stream<String> filter(Stream<String> stringStream, String pattern) {
        return stringStream.filter(str -> !str.contains(pattern)); // Filter strings containing the pattern
    }

    @Override
    public IntStream createIntStream(int[] arr) {
        return IntStream.of(arr); // Create IntStream from array
    }

    // Removed duplicate definition of toList(Stream<E> stream)
    @Override
    public <E> List<E> toList(Stream<E> stream) {
        return stream.collect(Collectors.toList()); // Convert stream to list
    }

    @Override
    public List<Integer> toList(IntStream intStream) {
        return intStream.boxed().collect(Collectors.toList()); // Convert IntStream to List<Integer>
    }

    @Override
    public IntStream createIntStream(int start, int end) {
        return IntStream.rangeClosed(start, end); // Create IntStream in range from start to end
    }

    @Override
    public DoubleStream squareRootIntStream(IntStream intStream) {
        return intStream.asDoubleStream().map(Math::sqrt); // Convert IntStream to DoubleStream and square each element
    }

    @Override
    public IntStream getOdd(IntStream intStream) {
        return intStream.filter(num -> num % 2 != 0); // Filter odd numbers from IntStream
    }

    @Override
    public Consumer<String> getLambdaPrinter(String prefix, String suffix) {
        return message -> System.out.println(prefix + message + suffix); // Lambda for printing with prefix and suffix
    }

    @Override
    public void printMessages(String[] messages, Consumer<String> printer) {
        for (String message : messages) {
            printer.accept(message); //Print each message using the provided printer
        }
    }

    @Override
    public void printOdd(IntStream intStream, Consumer<String> printer) {
        intStream.filter(num -> num % 2 != 0) // Filter odd numbers
                .forEach(num -> printer.accept("odd number:" + num + "!")); // Print each odd number
    }

    @Override
    public Stream<Integer> flatNestedInt(Stream<List<Integer>> ints) {
        return ints.flatMap(List::stream) // Flatten Stream<List<Integer>> to Stream<Integer>
                .map(num -> num * num); // Square each number
    }
}
