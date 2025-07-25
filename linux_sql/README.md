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

## Architecture

The project architecture consists of three Linux hosts, each running monitoring agents that collect system usage data. The agents store the collected data into a centralized PostgreSQL database.

- **Linux Hosts:** Machines where the monitoring agents (`host_info.sh` and `host_usage.sh`) are executed to collect hardware and usage data.
- **Monitoring Agents:** Shell scripts that gather and send system metrics to the database.
- **PostgreSQL Database:** Centralized storage for all collected hardware specifications and usage data.

The diagram is saved in the `assets` directory as `cluster_architecture.png`.

## Scripts

### 1. psql_docker.sh
This script manages a PostgreSQL database instance using Docker. It can create, start, or stop a container.

**Usage:**

  - Create a PostgreSQL container

  `./scripts/psql_docker.sh create db_username db_password`

  - Start the PostgreSQL container

  `./scripts/psql_docker.sh start`

  - Stop the PostgreSQL container

  `./scripts/psql_docker.sh stop`

### 2. host_info.sh
This script collects the hardware specifications of the host machine (e.g., CPU, memory, etc.) and inserts the data into the `host_info` table in the database.

**Usage:**

- Insert hardware information into the database

`./scripts/host_info.sh psql_host psql_port db_name psql_user psql_password`


### 3. host_usage.sh
This script collects real-time system usage data (e.g., memory usage, CPU idle time) and inserts the data into the `host_usage` table in the database. It is designed to be run periodically, such as through crontab.

**Usage:**

- Insert system usage data into the database

`./scripts/host_usage.sh psql_host psql_port db_name psql_user psql_password`

### 4. crontab
`crontab` is used to automate the execution of `host_usage.sh` every minute. This ensures continuous monitoring of the system's resource usage.

**Setup:**

- Open crontab editor

`crontab -e`

- Add the following line to execute host_usage.sh every minute

`* * * * * bash /path/to/scripts/host_usage.sh psql_host psql_port db_name psql_user psql_password`

### 5. queries.sql
This script contains SQL queries to analyze the collected data and address specific business problems. For example, it can identify machines with high CPU idle time or low memory availability.

**Example Business Problem:**  
*"Which machines consistently have the highest CPU idle time or are underutilized?"*

**Usage:**

`-- Query to find machines with high CPU idle time
SELECT hostname, AVG(cpu_idle) AS avg_cpu_idle
FROM host_usage
INNER JOIN host_info ON host_usage.host_id = host_info.id
GROUP BY hostname
ORDER BY avg_cpu_idle DESC;`

## Data Modeling

### 1. `host_info` Table Schema

| Column Name  | Data Type      | Description                                    |
|--------------|----------------|------------------------------------------------|
| `id`         | INT            | Primary key, unique identifier for each host   |
| `hostname`   | VARCHAR(255)    | Host machine name or identifier                |
| `cpu_count`  | INT            | Number of CPUs on the host                     |
| `memory`     | INT            | Total memory (in MB) of the host               |
| `disk_space` | INT            | Total disk space (in MB) of the host           |
| `os_version` | VARCHAR(255)    | The operating system version of the host       |
| `created_at` | TIMESTAMP      | Timestamp when the entry was created           |

### 2. `host_usage` Table Schema

| Column Name  | Data Type      | Description                                     |
|--------------|----------------|-------------------------------------------------|
| `id`         | INT            | Primary key, unique identifier for each usage record |
| `host_id`    | INT            | Foreign key, references `id` in `host_info` table  |
| `timestamp`  | TIMESTAMP      | Timestamp when the usage data was collected     |
| `cpu_idle`   | INT            | Percentage of CPU idle time                     |
| `cpu_kernel` | INT            | Percentage of CPU used by the kernel            |
| `memory_free`| INT            | Amount of free memory (in MB)                   |
| `disk_io`    | INT            | Disk input/output activity                      |
| `disk_available` | INT        | Available disk space (in MB)                    |

## Test

To test the Bash scripts and DDL (Data Definition Language) scripts, the following steps were taken:

1. **Testing `psql_docker.sh`:**
   - **Objective:** Verify that the script correctly creates, starts, and stops the PostgreSQL container.
   - **Testing Method:** 
     - Ran `psql_docker.sh create` with a valid database username and password.
     - Used `psql_docker.sh start` to start the container and `psql_docker.sh stop` to stop it.
   - **Result:** The script successfully created the container, started it, and stopped it without any issues.

2. **Testing `host_info.sh`:**
   - **Objective:** Ensure that the script collects the host hardware specifications and inserts them into the database.
   - **Testing Method:** 
     - Ran the script with correct database parameters to insert hardware info into the `host_info` table.
   - **Result:** Data was successfully inserted into the database, including CPU count, memory, disk space, etc.

3. **Testing `host_usage.sh`:**
   - **Objective:** Ensure that the script collects real-time system usage data and inserts it into the database.
   - **Testing Method:** 
     - Ran the script with correct database parameters to insert real-time system usage data into the `host_usage` table.
   - **Result:** Data was successfully inserted and included metrics like CPU idle time, memory usage, disk I/O, etc.

4. **Testing `crontab` Setup:**
   - **Objective:** Verify that the `host_usage.sh` script runs periodically as per the crontab configuration.
   - **Testing Method:** 
     - Set up crontab to run `host_usage.sh` every minute.
     - Checked the `host_usage` table to see if new records were inserted every minute.
   - **Result:** Data was successfully recorded every minute as expected.

5. **Testing DDL:**
   - **Objective:** Validate the correct creation of database tables and relationships.
   - **Testing Method:** 
     - Ran the DDL script to create the `host_info` and `host_usage` tables.
   - **Result:** Tables were successfully created without errors, and relationships between tables were correctly established.

These tests ensured the functionality of the scripts and DDL in managing the database and automating data collection.

## Deployment

To deploy the application, the following steps were followed:

1. **GitHub:**
   - The project and scripts were stored in a GitHub repository.
   - GitHub was used for version control, making it easy to collaborate, track changes, and roll back if necessary.

2. **Docker:**
   - Docker was used to containerize the PostgreSQL database (`psql_docker.sh`), ensuring that the database could be easily deployed across different environments.
   - A Docker container was created and configured with the necessary environment variables for PostgreSQL (e.g., password, port).

3. **Crontab:**
   - `crontab` was used to automate the execution of the `host_usage.sh` script. This ensures that the system's resource usage is monitored continuously without manual intervention.
   - The script was set to run every minute, inserting system usage data into the database.

4. **Manual Testing:**
   - After deployment, scripts were tested manually to ensure that they worked as expected and collected the necessary data into the database.
   
5. **Environment Variables:**
   - Necessary environment variables (e.g., database credentials) were configured to allow the scripts to connect to the database.
   
By combining GitHub, Docker, and crontab, the application was effectively deployed and automated for continuous monitoring of system performance and hardware specifications.

# Improvements

1. **Handle hardware updates:**
   - Currently, the system assumes hardware specifications remain unchanged. An improvement would be to regularly update and verify hardware details, ensuring that the database reflects any hardware changes or upgrades automatically.

2. **Optimize resource usage collection:**
   - The current `host_usage.sh` script collects system usage data every minute. Optimizing this by adjusting the frequency or improving the efficiency of data collection could reduce resource consumption on the monitored system.

3. **Error handling and notifications:**
   - Enhance the scripts with better error handling and logging to capture failures in script execution. Additionally, setting up notifications (e.g., email alerts or Slack messages) would be beneficial if a container fails to start or if there is a problem inserting data into the database.

