package ca.jrvs.apps.grep;

import java.io.File;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.Collections;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class JavaGrepImpl implements JavaGrep {

    // Déclaration des variables d'instance
    private String rootPath;
    private String regex;
    private String outFile;

        @Override
    public void process() throws IOException {
        // Step 1: List the files in the root directory
        List<File> files = listFiles(getRootPath());

        // Step 2: Read the lines of each file
        for (File file : files) {
            List<String> lines = readLines(file);

            // Step 3: Filter the lines that match the pattern
            for (String line : lines) {
                if (containsPattern(line)) {
                    // Step 4: Write the matching lines to the output file
                    writeToFile(Collections.singletonList(line));
                }
            }
        }
    }

    @Override
    public List<File> listFiles(String rootDir) {
        File rootDirectory = new File(rootDir);
        if (rootDirectory.exists() && rootDirectory.isDirectory()) {
            // Pour lister tous les fichiers, y compris dans les sous-répertoires
            File[] files = rootDirectory.listFiles(file -> file.isDirectory() || file.getName().endsWith(".txt"));
            if (files == null) {
                return Collections.emptyList();
            }
            return Arrays.asList(files);
        } else {
            throw new IllegalArgumentException("The provided directory path is invalid or not a directory.");
        }
    }


    @Override
    public List<String> readLines(File inputFile) throws IllegalArgumentException, IOException {
        // Check if the inputFile is a valid file
        if (inputFile == null || !inputFile.isFile()) {
            // If the file is invalid, throw an IllegalArgumentException
            throw new IllegalArgumentException("The provided file is invalid or not a file.");
        }

        // Use a BufferedReader to read the file line by line
        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
            // Read all lines from the file and return them as a list
            return reader.lines().collect(Collectors.toList());
        } catch (IOException e) {
            // If there is an issue reading the file, throw an IOException
            throw new IOException("Error reading the file.", e);
        }
    }

    @Override
    public boolean containsPattern(String line) {
        // Check if the line matches the regular expression pattern
        return line != null && line.matches(getRegex());  // Use the regex pattern to check if the line matches
    }

    @Override
    public void writeToFile(List<String> lines) throws IOException {
        // Create a file writer to write to the output file
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(getOutFile(), true))) {
            // Write each line in the list to the output file
            for (String line : lines) {
                writer.write(line);   // Write the line
                writer.newLine();      // Add a new line after each line written
            }
        } catch (IOException e) {
            // If there is an issue writing to the file, throw an IOException
            throw new IOException("Error writing to the file.", e);
        }
    }

    @Override
    public String getRootPath() {
        // Return the root path where the search starts
        return this.rootPath;  // Assuming rootPath is an instance variable that holds the root path
    }

    @Override
    public void setRootPath(String rootPath) {
        // Set the root path where the search operation will begin
        if (rootPath == null || rootPath.trim().isEmpty()) {
            // If the provided root path is null or empty, throw an IllegalArgumentException
            throw new IllegalArgumentException("The root path cannot be null or empty.");
        }
        this.rootPath = rootPath;  // Store the provided root path in the instance variable
    }

    @Override
    public String getRegex() {
        // Return the regular expression used for searching
        return this.regex;  // Assuming regex is an instance variable that stores the regular expression pattern
    }

    @Override
    public void setRegex(String regex) {
        // Check if the provided regex is valid
        if (regex == null || regex.trim().isEmpty()) {
            // If the regex is null or empty, throw an IllegalArgumentException
            throw new IllegalArgumentException("The regex cannot be null or empty.");
        }
        // Set the provided regex to the instance variable
        this.regex = regex;
    }

    @Override
    public String getOutFile() {
        // Return the output file path where the results will be written
        return this.outFile;  // Assuming outFile is an instance variable that stores the output file path
    }

    @Override
    public void setOutFile(String outFile) {
        // Check if the provided output file path is valid
        if (outFile == null || outFile.trim().isEmpty()) {
            // If the output file path is null or empty, throw an IllegalArgumentException
            throw new IllegalArgumentException("The output file path cannot be null or empty.");
        }
        // Set the provided output file path to the instance variable
        this.outFile = outFile;
    }

    // Main method
    public static void main(String[] args) {
        // Check if exactly 3 arguments are passed
        if (args.length != 3) {
            System.out.println("Usage: JavaGrepImpl <regex> <rootPath> <outFile>");
            System.exit(1);
        }

        // Create an instance of JavaGrepImpl
        JavaGrepImpl javaGrep = new JavaGrepImpl();

        // Assign the arguments to instance variables
        javaGrep.setRegex(args[0]);  // Set the regular expression
        javaGrep.setRootPath(args[1]);  // Set the root directory path
        javaGrep.setOutFile(args[2]);  // Set the output file path

        // Try to execute the grep process
        try {
            javaGrep.process();  // Execute the grep operation
            System.out.println("Grep operation completed successfully.");
        } catch (IOException e) {
            // If an error occurs, print the exception
            e.printStackTrace();
            System.out.println("Error during grep operation.");
        }
    }
}
