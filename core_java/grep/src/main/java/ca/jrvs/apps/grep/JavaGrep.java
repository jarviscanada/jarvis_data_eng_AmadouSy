package ca.jrvs.apps.grep;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Top level search workflow
 * @throws IOException
 */
public interface JavaGrep {

    /**
     * Perform the search process
     * @throws IOException
     */
    void process() throws IOException;

    /**
     * Traverse a given directory and return all files
     * @param rootDir input directory
     * @return files under the rootDir
     */
    List<File> listFiles(String rootDir);

    /**
     * Read a file and return all the lines
     * @param inputFile file to be read
     * @return lines in the file
     * @throws IllegalArgumentException if inputFile is not a file
     * @throws IOException if an IO error occurs
     */
    List<String> readLines(File inputFile) throws IllegalArgumentException, IOException;

    /**
     * Check if a line contains the regex pattern (passed by user)
     * @param line input string
     * @return true if there is a match
     */
    boolean containsPattern(String line);

    /**
     * Write lines to a file
     * @param lines matched lines
     * @throws IOException if write failed
     */
    void writeToFile(List<String> lines) throws IOException;

    // Getters and Setters for root path, regex, output file, etc.
    String getRootPath();
    void setRootPath(String rootPath);

    String getRegex();
    void setRegex(String regex);

    String getOutFile();
    void setOutFile(String outFile);
}
