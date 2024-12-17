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

# Quick Start

Follow these steps to quickly set up and use the project:

### 1. Start a PostgreSQL instance using `psql_docker.sh`

- Run the following command to start a PostgreSQL container:
  ```bash
  ./psql_docker.sh create <db_username> <db_password>

- Replace <db_username> and <db_password> with the appropriate database username and password.

### 2. Create tables using ddl.sql

- After the container is up and running, create the necessary tables by running the following command:
  `psql -h localhost -U postgres -d host_agent -f sql/ddl.sql`

- This command executes the ddl.sql file to create the required tables in the PostgreSQL database.

### 3. Insert hardware specs data into the database using host_info.sh

- Run the following command to insert the machine's hardware specifications data:
`./host_info.sh localhost 5432 host_agent postgres <db_password>`

- Replace <db_password> with the database password you provided earlier.

### 4. Insert hardware usage data into the database using host_usage.sh

- To collect and insert real-time machine usage data every minute, run:

`./host_usage.sh localhost 5432 host_agent postgres <db_password>`

- Replace <db_password> with the password for your PostgreSQL instance.

### 5. Set up cron for periodic data collection

- Add the host_usage.sh script to crontab to run every minute:

`crontab -e` 

- Add the following line to your crontab file to schedule it:

* * * * * /path/to/host_usage.sh localhost 5432 host_agent postgres <db_password>

- Replace /path/to/host_usage.sh with the correct path to your host_usage.sh script and <db_password> with your database password.
