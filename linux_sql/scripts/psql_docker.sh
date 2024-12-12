#!/bin/sh

# Capture CLI arguments
cmd=$1
db_username=$2
db_password=$3

# Start Docker service
# Use "||" to start Docker if it's not running
sudo systemctl status docker || sudo systemctl start docker

# Check container status
# Inspect the container to see if it exists
docker container inspect jrvs-psql > /dev/null 2>&1
container_status=$?

# Use switch-case to handle create|stop|start options
case $cmd in 
  create)
    # Check if the container already exists
    if [ $container_status -eq 0 ]; then
      echo "Container already exists"
      exit 1
    fi

    # Check the number of CLI arguments
    if [ $# -ne 3 ]; then
      echo "Create requires username and password"
      exit 1
    fi

    # Create a Docker volume for persistent data storage
    docker volume create jrvs-psql-data

    # Create and run the PostgreSQL container
    docker run --name jrvs-psql \
      -e POSTGRES_USER=$db_username \
      -e POSTGRES_PASSWORD=$db_password \
      -d \
      -v jrvs-psql-data:/var/lib/postgresql/data \
      -p 5432:5432 \
      postgres:latest

    # Check if the container was created successfully
    exit $?
    ;;

  start|stop)
    # Check if the container has been created
    if [ $container_status -ne 0 ]; then
      echo "Container has not been created"
      exit 1
    fi

    # Start or stop the container based on the command
    docker container $cmd jrvs-psql
    exit $?
    ;;	

  *)
    # Handle invalid commands
    echo "Illegal command"
    echo "Commands: start|stop|create"
    exit 1
    ;;
esac

