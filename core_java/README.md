# Core Java Apps
This project category consists of the two Java applications below

1. [Java Grep App](./grep)

# Introduction

This project implements a simple **Java Grep** application that searches through files in a directory to find lines that match a given regular expression (regex). The design of the application is based on core **Java** concepts, including **IO operations** (reading and writing files), **regular expressions**, and **file traversal**. It uses a **Docker** container for easy deployment, ensuring portability across systems. The project also leverages **lambda expressions** and **streams** for efficient data processing. The application is developed using an **IDE** (e.g., **IntelliJ IDEA**), and packaged into a **JAR file** for distribution.

# Quick Start

To use the **Java Grep** application, follow these steps:

1. **Build the project**:
   - Ensure you have **Maven** installed.
   - Navigate to the project directory and run the following command to compile the project:
     ```bash
     mvn clean install
     ```

2. **Run the application**:
   - Use the command below to run the application:
     ```bash
     java -cp target/grep-1.0-SNAPSHOT.jar ca.jrvs.apps.grep.JavaGrepImpl "<regex>" "<rootDir>" "<outFile>"
     ```
     - Replace `<regex>` with your regular expression pattern.
     - Replace `<rootDir>` with the directory you want to search in.
     - Replace `<outFile>` with the path where the matched lines will be saved.

3. **Example**:
   ```bash
   java -cp target/grep-1.0-SNAPSHOT.jar ca.jrvs.apps.grep.JavaGrepImpl ".*Romeo.*Juliet.*" ./data/txt ./out/grep.txt
   
4. **Docker Setup**
  - You can also run the app inside a Docker container. First, build the Docker image:
  ```bash
  docker build -t <your-docker-id>/grep .
  ```
  - Then, run the application using Docker:
  ```bash
  docker run --rm -v $(pwd)/data:/data -v $(pwd)/log:/log <your-docker-id>/grep "<regex>" /data/txt /log/grep.out
  ```

## Implementation

### Pseudocode

```java
method process():
    # Step 1: List all files in the root directory
    files = listFiles(rootPath)

    # Step 2: Loop through each file
    for each file in files:
        # Step 3: Read all lines from the current file
        lines = readLines(file)

        # Step 4: Loop through each line and check if it matches the pattern
        for each line in lines:
            if containsPattern(line):
                # Step 5: Write the matched lines to the output file
                writeToFile(line)
```

This pseudocode outlines the core logic of the `process` method:

1. It lists all the files in the given directory (`listFiles`).
2. Reads each file's content (`readLines`).
3. Checks each line against a given regular expression pattern (`containsPattern`).
4. Writes matching lines to the output file (`writeToFile`).


2. [Stock Quote App](./stockquote)
