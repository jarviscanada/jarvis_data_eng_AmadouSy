#!/bin/bash

# Validate the number of CLI arguments
if [ "$#" -ne 5 ]; then
  echo "Usage: $0 psql_host psql_port db_name psql_user psql_password"
  exit 1
fi

# Assign CLI arguments to variables
psql_host=$1
psql_port=$2
db_name=$3
psql_user=$4
psql_password=$5

# Export the PostgreSQL password to avoid manual entry
export PGPASSWORD=$psql_password

# Collect hardware specifications
# Get the fully qualified hostname
hostname=$(hostname -f)

# Get the number of CPUs
cpu_number=$(lscpu | grep "^CPU(s):" | awk '{print $2}')

# Get the CPU architecture (e.g., x86_64)
cpu_architecture=$(lscpu | grep "Architecture" | awk '{print $2}')

# Get the CPU model name
cpu_model=$(lscpu | grep "Model name" | cut -d':' -f2 | xargs)

# Get the CPU frequency in MHz
cpu_mhz=$(lscpu | grep "CPU MHz" | awk '{print $3}')

# Get the L2 cache size in KB, removing the "K" suffix
L2_cache=$(lscpu | grep "L2 cache" | awk '{print $3}' | sed 's/K//')

# Get the total memory in KB
total_mem=$(cat /proc/meminfo | grep "MemTotal" | awk '{print $2}')

# Get the current timestamp in UTC format
timestamp=$(date -u '+%Y-%m-%d %H:%M:%S')

# Construct the SQL INSERT statement
insert_stmt="INSERT INTO host_info (hostname, cpu_number, cpu_architecture, cpu_model, cpu_mhz, L2_cache, total_mem, timestamp) \
VALUES ('$hostname', $cpu_number, '$cpu_architecture', '$cpu_model', $cpu_mhz, $L2_cache, $total_mem, '$timestamp');"

# Execute the SQL INSERT statement using psql
psql -h $psql_host -p $psql_port -U $psql_user -d $db_name -c "$insert_stmt"

# Check if the psql command was successful
if [ $? -ne 0 ]; then
  echo "Error: Failed to insert data into the database"
  exit 1
fi

# Output a success message
echo "Data successfully inserted into the host_info table"
exit 0

