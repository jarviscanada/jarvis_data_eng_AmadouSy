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

