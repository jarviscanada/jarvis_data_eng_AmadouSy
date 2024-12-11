#!/bin/bash

# Assign CLI arguments to variables
psql_host=$1
psql_port=$2
db_name=$3
psql_user=$4
psql_password=$5

# Validate the number of arguments
if [ "$#" -ne 5 ]; then
    echo "Illegal number of parameters. Usage: $0 psql_host psql_port db_name psql_user psql_password"
    exit 1
fi

# Save machine statistics in MB and current hostname to variables
vmstat_mb=$(vmstat --unit M)
hostname=$(hostname -f)

# Retrieve hardware specification variables
# Free memory (in MB)
memory_free=$(echo "$vmstat_mb" | awk '{print $4}' | tail -n1 | xargs)

# CPU idle percentage
cpu_idle=$(echo "$vmstat_mb" | awk '{print $15}' | tail -n1 | xargs)

# CPU kernel usage percentage
cpu_kernel=$(echo "$vmstat_mb" | awk '{print $14}' | tail -n1 | xargs)

# Number of disk I/O operations
disk_io=$(vmstat -d | awk '{print $10}' | tail -n1 | xargs)

# Available disk space on root directory (in MB)
disk_available=$(df -BM / | tail -n1 | awk '{print $4}' | sed 's/M//' | xargs)

# Current timestamp in UTC format
timestamp=$(date -u '+%Y-%m-%d %H:%M:%S')

# Subquery to find the matching host ID in the host_info table
host_id="(SELECT id FROM host_info WHERE hostname='$hostname')"

# Construct the SQL INSERT statement
insert_stmt="INSERT INTO host_usage (timestamp, host_id, memory_free, cpu_idle, cpu_kernel, disk_io, disk_available)
VALUES ('$timestamp', $host_id, $memory_free, $cpu_idle, $cpu_kernel, $disk_io, $disk_available);"

# Export PostgreSQL password for non-interactive psql
export PGPASSWORD=$psql_password 

# Execute the SQL INSERT statement
psql -h $psql_host -p $psql_port -d $db_name -U $psql_user -c "$insert_stmt"

# Exit with the status of the psql command
exit $?

