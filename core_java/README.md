# Core Java Apps
This project category consists of the two Java applications below

# Java Grep App

## Introduction

This project implements a simple **Java Grep** application that searches through files in a directory to find lines that match a given regular expression (regex). The design of the application is based on core **Java** concepts, including **IO operations** (reading and writing files), **regular expressions**, and **file traversal**. It uses a **Docker** container for easy deployment, ensuring portability across systems. The project also leverages **lambda expressions** and **streams** for efficient data processing. The application is developed using an **IDE** (e.g., **IntelliJ IDEA**), and packaged into a **JAR file** for distribution.

## Quick Start

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

### Performance Issue

The `process` method processes large files, which can cause high memory usage, especially when reading and storing the contents of entire files in memory. To mitigate this, the program could process the files line by line, rather than loading the entire file into memory. Additionally, using streams and lazy evaluation could help reduce memory consumption.

## Test

To test the application manually, I prepared sample text files containing various sentences, including some with matching patterns (e.g., "Romeo" and "Juliet"). I ran the application using different regular expressions to verify that only lines containing the specified patterns were written to the output file. I also compared the output file with the expected results to ensure accuracy. Additionally, I tested with both small and large files to assess the application's performance and memory usage.

## Deployment

To dockerize the application, I created a `Dockerfile` that includes the necessary setup for the app to run inside a Docker container. The Dockerfile starts with an official `openjdk:8-alpine` base image, which is lightweight and ideal for running Java applications. The JAR file is copied into the container, and the `ENTRYPOINT` is set to run the application with the provided arguments. The Docker image can then be built and pushed to Docker Hub for easy distribution.

The steps to build and run the Docker container are as follows:

1. Build the Docker image:
   ```bash
   docker build -t myusername/grep .
   ```

2. Run the container
To run the Docker container with your application, use the following command:
   ```bash
   docker run --rm -v $(pwd)/data:/data -v $(pwd)/log:/log myusername/grep .*Romeo.*Juliet.* /data/txt /log/grep.out

## Improvement

Here are three simple improvements for the project:

1. **Add Input Validation for Regex**  
   Before using the regex pattern, we could add a validation step to ensure the regex is correctly formatted, improving robustness.

2. **Allow Custom Output Formatting**  
   The app currently writes plain matching lines. Adding options to customize the output format (e.g., adding line numbers, file names) would make it more flexible.

3. **Improve Logging**  
   Adding more detailed logging for different steps of the process (e.g., file reading, matching, writing) could help with debugging and monitoring the app's behavior.




# Stock Quote Application  

## Introduction  
The **Stock Quote Application** is a Java-based application designed to retrieve and manage real-time stock market data. It fetches stock information from the **Alpha Vantage API** and stores it in a **PostgreSQL database**.  

This application follows a **modular architecture**, using **JDBC, PostgreSQL, Maven, OkHttp, and Docker** to ensure scalability and maintainability. The main functionalities include:  
- Fetching live stock quotes  
- Storing stock data in a relational database  
- Managing user stock positions (buying and selling stocks)  
- Running inside a Docker container for easy deployment  

This project aims to provide **real-time financial insights** while demonstrating best practices in **database interaction, API integration, and software design patterns**.  

## Implementation  

### ER Diagram  
The **ER (Entity-Relationship) Diagram** represents how the data is structured in the database. It includes tables for storing stock quotes, user transactions, and portfolio details.  

#### **Key Entities**:  
1. **Quote Table** - Stores stock price information such as symbol, open price, high price, low price, and volume.  
2. **Position Table** - Tracks the stocks owned by a user, including the number of shares and total investment.  
3. **Transaction Table** - Records buy and sell transactions, including stock symbol, transaction type, price, and timestamp.  

Below is the ER diagram of the system:  

*(Insert ER diagram image here)*  

The database is managed using **PostgreSQL**, and data is inserted or retrieved using **JDBC**.  




