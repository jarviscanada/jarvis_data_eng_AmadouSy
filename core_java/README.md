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
  -Then, run the application using Docker:
  ```bash
  docker run --rm -v $(pwd)/data:/data -v $(pwd)/log:/log <your-docker-id>/grep "<regex>" /data/txt /log/grep.out
  ```

2. [Stock Quote App](./stockquote)
