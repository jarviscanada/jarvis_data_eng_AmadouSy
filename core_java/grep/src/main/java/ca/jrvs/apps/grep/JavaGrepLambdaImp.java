import ca.jrvs.apps.grep.JavaGrepImpl;
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;
import java.util.stream.*;


public class JavaGrepLambdaImp extends JavaGrepImpl {

    private String rootPath;
    private String regex;
    private String outFile;

    @Override
    public void process() throws IOException {
        // List files from the directory
        List<File> files = listFiles(getRootPath());

        // Process each file and check if lines match the regex pattern
        List<String> matchedLines = new ArrayList<>();
        for (File file : files) {
            List<String> lines = readLines(file); // Read lines from the file
            lines.stream()  // Use streams to filter lines
                    .filter(line -> containsPattern(line))  // Filter matching lines
                    .forEach(matchedLines::add);  // Add to the result list
        }

        // Write the matched lines to the output file
        writeToFile(matchedLines);
    }

    @Override
    public List<File> listFiles(String rootDir) {
        // List all files under the given directory
        try (Stream<Path> paths = Files.walk(Paths.get(rootDir))) {
            return paths.filter(Files::isRegularFile)
                    .map(Path::toFile)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    @Override
    public List<String> readLines(File inputFile) throws IOException {
        // Read all lines from the file and return as a List
        return Files.readAllLines(inputFile.toPath());
    }

    @Override
    public boolean containsPattern(String line) {
        // Check if the line contains the given regex pattern
        return Pattern.matches(getRegex(), line);
    }

    @Override
    public void writeToFile(List<String> lines) throws IOException {
        // Write the matched lines to the output file
        Files.write(Paths.get(getOutFile()), lines);
    }

    @Override
    public String getRootPath() {
        return this.rootPath;
    }

    @Override
    public void setRootPath(String rootPath) {
        this.rootPath = rootPath;
    }

    @Override
    public String getRegex() {
        return this.regex;
    }

    @Override
    public void setRegex(String regex) {
        this.regex = regex;
    }

    @Override
    public String getOutFile() {
        return this.outFile;
    }

    @Override
    public void setOutFile(String outFile) {
        this.outFile = outFile;
    }

    public static void main(String[] args) {
        if (args.length != 3) {
            System.out.println("Usage: JavaGrepImpl <regex> <rootPath> <outFile>");
            System.exit(1);
        }

        JavaGrepLambdaImp javaGrepLambdaImp = new JavaGrepLambdaImp();
        javaGrepLambdaImp.setRegex(args[0]);
        javaGrepLambdaImp.setRootPath(args[1]);
        javaGrepLambdaImp.setOutFile(args[2]);

        try {
            javaGrepLambdaImp.process();
            System.out.println("Grep operation completed successfully.");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
