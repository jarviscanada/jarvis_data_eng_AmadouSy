package ca.jrvs.apps.practice;

import java.util.List;
import java.util.function.Consumer;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public interface LambdaStreamExc {

    /**
     * Create a String stream from array
     *
     * note: arbitrary number of value will be stored in an array
     *
     * @param strings
     * @return
     */
    Stream<String> createStrStream(String ... strings);


    /**
     * Convert all strings to uppercase
     * please use createStrStream
     *
     * @param strings
     * @return
     */
    Stream<String> toUpperCase(String ... strings);

    /**
     * filter strings that contains the pattern
     * e.g.
     * filter(stringStream, "a") will return another stream which no element contains a
     *
     *
     * @param stringStream
     * @param pattern
     * @return
     */
    Stream<String> filter(Stream<String> stringStream, String pattern);

    /**
     * Create a intStream from a arr[]
     * @param arr
     * @return
     */
    IntStream createIntStream(int[] arr);

    /**
     * Convert a stream to list
     *
     * @param stream
     * @param <E>
     * @return
     */
    <E> List<E> toList(Stream<E> stream);

    /**
     * Convert a intStream to list
     * @param intStream
     * @return
     */
    List<Integer> toList(IntStream intStream);

    /**
     * Create a IntStream range from start to end inclusive
     * @param start
     * @param end
     * @return
     */
    IntStream createIntStream(int start, int end);

    /**
     * Convert a intStream to a doubleStream
     * and compute square root of each element
     * @param intStream
     * @return
     */
    DoubleStream squareRootIntStream(IntStream intStream);


    /**
     * filter all even number and return odd numbers from a intStream
     * @param intStream
     * @return
     */
    IntStream getOdd(IntStream intStream);

    /**
     * Return a lambda function that print a message with a prefix and suffix
     * This lambda can be useful to format logs
     *
     * You will learn:
     *   - functional interface http://bit.ly/2pTXRwM & http://bit.ly/33onFig
     *   - lambda syntax
     *
     * e.g.
     * LambdaStreamExc lse = new LambdaStreamImp();
     * Consumer<String> printer = lse.getLambdaPrinter("start>", "<end");
     * printer.accept("Message body");
     *
     * sout:
     * start>Message body<end
     *
     * @param prefix prefix str
     * @param suffix suffix str
     * @return
     */
    Consumer<String> getLambdaPrinter(String prefix, String suffix);

    /**
     * Print each message with a given printer
     * Please use `getLambdaPrinter` method
     *
     * e.g.
     * String[] messages = {"a","b", "c"};
     * lse.printMessages(messages, lse.getLambdaPrinter("msg:", "!") );
     *
     * sout:
     * msg:a!
     * msg:b!
     * msg:c!
     *
     * @param messages
     * @param printer
     */
    void printMessages(String[] messages, Consumer<String> printer);

    /**
     * Print all odd number from a intStream.
     * Please use `createIntStream` and `getLambdaPrinter` methods
     *
     * e.g.
     * lse.printOdd(lse.createIntStream(0, 5), lse.getLambdaPrinter("odd number:", "!"));
     *
     * sout:
     * odd number:1!
     * odd number:3!
     * odd number:5!
     *
     * @param intStream
     * @param printer
     */
    void printOdd(IntStream intStream, Consumer<String> printer);

    /**
     * Square each number from the input.
     * Please write two solutions and compare difference
     *   - using flatMap
     *
     * @param ints
     * @return
     */

    Stream<Integer> flatNestedInt(Stream<List<Integer>> ints);

}

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