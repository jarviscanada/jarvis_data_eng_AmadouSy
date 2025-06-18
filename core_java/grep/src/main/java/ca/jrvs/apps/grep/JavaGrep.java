package ca.jrvs.apps.grep;

import java.io.File;
import java.io.IOException;
import java.util.List;

public interface JavaGrep {

    /**
     * Top-level method for processing the grep operation.
     * This method should orchestrate the steps of listing files, reading lines,
     * matching patterns, and writing the results to the output file.
     * @throws IOException if there are input/output issues
     */
    void process() throws IOException;

    /**
     * Traverse a given directory and return all files found.
     *
     * @param rootDir the input directory to start searching from
     * @return a list of files found under the rootDir
     */
    List<File> listFiles(String rootDir);

    /**
     * Read the content of a file and return all lines as a list.
     *
     * @param inputFile the file to read from
     * @return a list of lines read from the file
     * @throws IllegalArgumentException if the given file is not valid
     * @throws IOException if there are issues reading the file
     */
    List<String> readLines(File inputFile) throws IllegalArgumentException, IOException;

    /**
     * Check if a given line matches the regular expression pattern.
     *
     * @param line the line to check
     * @return true if the line contains the pattern, false otherwise
     */
    boolean containsPattern(String line);

    /**
     * Write the list of matched lines to the output file.
     *
     * @param lines the list of lines to write
     * @throws IOException if there are issues writing to the output file
     */
    void writeToFile(List<String> lines) throws IOException;

    /**
     * Get the root path where the search starts.
     *
     * @return the root path
     */
    String getRootPath();

    /**
     * Set the root path for the search operation.
     *
     * @param rootPath the root path to set
     */
    void setRootPath(String rootPath);

    /**
     * Get the regular expression pattern used for searching.
     *
     * @return the regular expression
     */
    String getRegex();

    /**
     * Set the regular expression pattern used for searching.
     *
     * @param regex the regular expression to set
     */
    void setRegex(String regex);

    /**
     * Get the output file path where the results will be written.
     *
     * @return the output file path
     */
    String getOutFile();

    /**
     * Set the output file path where the results will be written.
     *
     * @param outFile the output file path to set
     */
    void setOutFile(String outFile);
}
