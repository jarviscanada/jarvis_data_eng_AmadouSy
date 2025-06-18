CREATE TABLE IF NOT EXISTS host_info (
    id SERIAL PRIMARY KEY,
    hostname VARCHAR NOT NULL UNIQUE,
    cpu_number SMALLINT NOT NULL,
    cpu_architecture VARCHAR NOT NULL,
    cpu_model VARCHAR NOT NULL,
    cpu_mhz DOUBLE PRECISION NOT NULL,
    l2_cache INTEGER NOT NULL,
    "timestamp" TIMESTAMP NOT NULL,
    total_mem INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS host_usage (
    id SERIAL PRIMARY KEY,
    hostname VARCHAR NOT NULL,
    memory_used INTEGER NOT NULL,
    disk_used INTEGER NOT NULL,
    cpu_usage DECIMAL(5, 2) NOT NULL,
    "timestamp" TIMESTAMP NOT NULL,
    FOREIGN KEY (hostname) REFERENCES host_info(hostname) ON DELETE CASCADE
);
