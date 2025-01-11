# Introduction
This project focuses on building expertise in SQL and relational database design by solving practical queries and understanding database structures. It allows users to learn and practice CRUD operations, data modeling, normalization, and advanced SQL techniques such as JOINs, aggregations, and window functions. The project is designed as a hands-on learning experience, combining theoretical understanding with practical implementation.

The primary users of this project are aspiring data professionals, including data engineers, analysts, and developers, who aim to strengthen their database management and querying skills.

The project utilizes a variety of technologies to enhance the learning experience, including PostgreSQL for database management, Docker for containerized database instances, Git for version control, and Bash for automating tasks. Tools like pgAdmin and DBeaver are recommended for database interaction, while SQL formatters are used to maintain clean and readable query structures.

# SQL Queries

###### Table Setup (DDL)

```sql
insert into cd.facilities
    (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance)
    values (9, 'Spa', 20, 30, 100000, 800);
```
Explanation: This query adds a new facility with facid = 9 named "Spa" to the cd.facilities table, with associated costs and maintenance fees.

###### Question 1: Show all members 

### Create schema `cd`

```sql
CREATE SCHEMA cd;

CREATE TABLE cd.members (
    memid SERIAL PRIMARY KEY,
    surname VARCHAR(200) NOT NULL,
    firstname VARCHAR(200) NOT NULL,
    address VARCHAR(300),
    zipcode INTEGER,
    telephone VARCHAR(20),
    recommendedby INTEGER,
    joindate TIMESTAMP
);

CREATE TABLE cd.bookings (
    bookid SERIAL PRIMARY KEY,
    facid INTEGER NOT NULL,
    memid INTEGER NOT NULL,
    starttime TIMESTAMP NOT NULL,
    slots INTEGER NOT NULL,
    FOREIGN KEY (facid) REFERENCES cd.facilities(facid),
    FOREIGN KEY (memid) REFERENCES cd.members(memid)
);

CREATE TABLE cd.facilities (
    facid SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    membercost NUMERIC NOT NULL,
    guestcost NUMERIC NOT NULL,
    initialoutlay NUMERIC NOT NULL,
    monthlymaintenance NUMERIC NOT NULL
);

```




###### Questions 2: Lorem ipsum...



