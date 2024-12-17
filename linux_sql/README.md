# Project Design

## What does this project do?
This project sets up a PostgreSQL database inside a Docker container. It provides a simple way to deploy PostgreSQL with:
- **Persistent Data**: Data is stored in Docker volumes, so it's safe even if the container is removed.
- **Remote Access**: You can connect to the database via the default port (5432).
- **Configurable Environment Variables**: You can set the database password and other settings easily using environment variables.

## Who are the users?
This project is useful for:
- **Developers** who need a quick and isolated PostgreSQL setup for testing or development.
- **System administrators** who want to deploy PostgreSQL in a Docker container.
- **DevOps teams** looking for an easy way to manage PostgreSQL in different environments (like staging or production).

## Technologies Used:
- **Bash**: To automate the setup and Docker commands.
- **Docker**: For containerizing PostgreSQL.
- **PostgreSQL**: The database management system used.
- **Git**: For version control and managing changes in the project.

This project makes it easy to manage PostgreSQL in a containerized environment with minimal setup and configuration.

## Quick Start

Follow these steps to quickly set up and use the project:

### 1. Start a PostgreSQL instance using `psql_docker.sh`

- Run the following command to start a PostgreSQL container:
  ```bash
  ./psql_docker.sh create <db_username> <db_password>

- Replace <db_username> and <db_password> with the appropriate database username and password.

### 2. Create tables using ddl.sql

- After the container is up and running, create the necessary tables by running the following command:
  ``` bash
  psql -h localhost -U postgres -d host_agent -f sql/ddl.sql

- This command executes the ddl.sql file to create the required tables in the PostgreSQL database.

### 3. Insert hardware specs data into the database using host_info.sh

- Run the following command to insert the machine's hardware specifications data:
``` bash
./host_info.sh localhost 5432 host_agent postgres <db_password>

- Replace <db_password> with the database password you provided earlier.

### 4. Insert hardware usage data into the database using host_usage.sh

- To collect and insert real-time machine usage data every minute, run:

`./host_usage.sh localhost 5432 host_agent postgres <db_password>`

- Replace <db_password> with the password for your PostgreSQL instance.

### 5. Set up cron for periodic data collection

- Add the host_usage.sh script to crontab to run every minute:

`crontab -e` 

- Add the following line to your crontab file to schedule it:

`* * * * * /path/to/host_usage.sh localhost 5432 host_agent postgres <db_password>`

- Replace /path/to/host_usage.sh with the correct path to your host_usage.sh script and <db_password> with your database password.

## Implementation

This project is implemented using a combination of Bash scripts, Docker, PostgreSQL, and Linux utilities. Here's an overview of the key components and how they were implemented:

### 1. **Database Setup**
- A PostgreSQL database is deployed using Docker for efficient and isolated data storage. 
- The database schema is created using `ddl.sql`, which defines tables for hardware specifications (`host_info`) and real-time usage data (`host_usage`).

### 2. **Hardware Specification Collection**
- The `host_info.sh` script collects hardware information such as CPU, memory, and disk specifications using Linux commands like `vmstat`, `lscpu`, and `df`. 
- This data is inserted into the `host_info` table in the PostgreSQL database.

### 3. **Real-Time Usage Data Collection**
- The `host_usage.sh` script collects real-time metrics like CPU idle time, memory usage, and disk I/O using Linux commands like `vmstat` and `df`.
- This script inserts the collected metrics into the `host_usage` table in the database.

### 4. **Automating Data Collection**
- A cron job is set up to run the `host_usage.sh` script every minute. This ensures continuous data collection without manual intervention.

### 5. **Integration with Docker**
- Docker ensures that PostgreSQL is isolated and runs in a controlled environment.
- The `psql_docker.sh` script manages the creation, starting, and stopping of the PostgreSQL Docker container, providing flexibility and simplicity in deployment.

### 6. **Version Control**
- Git is used to manage the project files and maintain a history of changes. Branches were used for different features, and merging ensured a clean and tested main branch.

### 7. **Error Handling and Logging**
- The Bash scripts include error handling to validate inputs, check database connectivity, and ensure reliable operation.
- Logs are generated to debug and track the scripts' execution.

By combining these components, the project collects, stores, and manages system hardware and usage data efficiently in a structured manner.

