package ca.jrvs.apps.practice;

import java.util.List;
import java.util.function.Consumer;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class LambdaStreamImp implements LambdaStreamExc {
    @Override
    public Stream<String> createStrStream(String... strings) {
        // Convert the passed varargs (strings) into a Stream
        return Stream.of(strings);  // Using Stream.of to create the stream
    }

    @Override
    public Stream<String> toUpperCase(String... strings) {
        // Use createStrStream to create a Stream from the passed strings
        return createStrStream(strings) // Create the stream
                .map(String::toUpperCase);  // Convert all strings in the stream to uppercase
    }

    @Override
    public Stream<String> filter(Stream<String> stringStream, String pattern) {
        // Use the filter operation to return only the strings that do not contain the specified pattern
        return stringStream.filter(str -> !str.contains(pattern));  // Filters out strings containing the pattern
    }

    @Override
    public IntStream createIntStream(int[] arr) {
        // Convert the int array into an IntStream
        return IntStream.of(arr);  // Using IntStream.of to create an IntStream from the array
    }

    @Override
    public <E> List<E> toList(Stream<E> stream) {
        // Convert the stream into a list
        return stream.collect(Collectors.toList());  // Using collect with Collectors.toList() to convert stream to list
    }

    @Override
    public <E> List<E> toList(Stream<E> stream) {
        // Convert the stream into a list
        return stream.collect(Collectors.toList());  // Using collect with Collectors.toList() to convert stream to list
    }

    @Override
    public List<Integer> toList(IntStream intStream) {
        // Convert the IntStream into a List<Integer>
        return intStream.boxed().collect(Collectors.toList());  // Convert IntStream to Stream<Integer> and collect into a List
    }

    @Override
    public IntStream createIntStream(int start, int end) {
        // Create an IntStream that includes the range from start to end (inclusive)
        return IntStream.rangeClosed(start, end);  // Using rangeClosed to include the 'end' value
    }

    @Override
    public DoubleStream squareRootIntStream(IntStream intStream) {
        // Convert the IntStream to a DoubleStream and compute the square root of each element
        return intStream
                .asDoubleStream()         // Convert the IntStream to a DoubleStream
                .map(Math::sqrt);         // Apply the square root function to each element
    }

    @Override
    public IntStream getOdd(IntStream intStream) {
        // Filter out all even numbers and return only odd numbers from the IntStream
        return intStream.filter(num -> num % 2 != 0);  // Filter numbers that are not divisible by 2 (odd numbers)
    }

    @Override
    public Consumer<String> getLambdaPrinter(String prefix, String suffix) {
        // Return a lambda function that formats a message with prefix and suffix
        return message -> System.out.println(prefix + message + suffix);  // Print the message with the prefix and suffix
    }

    @Override
    public void printMessages(String[] messages, Consumer<String> printer) {
        // For each message in the array, apply the given printer to format and print the message
        for (String message : messages) {
            printer.accept(message);  // Use the printer to print each message
        }
    }

    @Override
    public void printOdd(IntStream intStream, Consumer<String> printer) {
        // Filter the odd numbers from the IntStream and print each one using the provided printer
        intStream.filter(num -> num % 2 != 0)  // Filter for odd numbers
                .forEach(num -> printer.accept("odd number:" + num + "!"));  // Use the printer to print each odd number
    }

    @Override
    public Stream<Integer> flatNestedInt(Stream<List<Integer>> ints) {
        // Use flatMap to flatten the Stream of Lists and square each number
        return ints
                .flatMap(List::stream)       // Flatten the Stream<List<Integer>> to Stream<Integer>
                .map(num -> num * num);      // Square each integer in the Stream
    }


}