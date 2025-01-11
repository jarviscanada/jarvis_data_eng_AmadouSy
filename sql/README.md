# Introduction
This project focuses on building expertise in SQL and relational database design by solving practical queries and understanding database structures. It allows users to learn and practice CRUD operations, data modeling, normalization, and advanced SQL techniques such as JOINs, aggregations, and window functions. The project is designed as a hands-on learning experience, combining theoretical understanding with practical implementation.

The primary users of this project are aspiring data professionals, including data engineers, analysts, and developers, who aim to strengthen their database management and querying skills.

The project utilizes a variety of technologies to enhance the learning experience, including PostgreSQL for database management, Docker for containerized database instances, Git for version control, and Bash for automating tasks. Tools like pgAdmin and DBeaver are recommended for database interaction, while SQL formatters are used to maintain clean and readable query structures.

# SQL Queries

###### Table Setup (DDL)

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

###### Question 1: Insert a new facility (Spa)

```sql
insert into cd.facilities
    (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance)
    values (9, 'Spa', 20, 30, 100000, 800);
```
This query adds a new facility with facid = 9 named "Spa" to the cd.facilities table, with associated costs and maintenance fees.


###### Question 2: Dynamically insert a new facility

```sql
insert into cd.facilities
    (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance)
    select (select max(facid) from cd.facilities)+1, 'Spa', 20, 30, 100000, 800;
```
This query updates the initialoutlay value to 10000 for the facility with facid = 1.


###### Question 3: Update the initial outlay of a facility

```sql
update cd.facilities
    set initialoutlay = 10000
    where facid = 1;
```
This query updates the initialoutlay value to 10000 for the facility with facid = 1.

###### Question 4: Update costs dynamically

```sql
update cd.facilities
set 
    membercost = (select membercost * 1.1 from cd.facilities where facid = 0),
    guestcost = (select guestcost * 1.1 from cd.facilities where facid = 0)
where facid = 1;
```
This query increases the membercost and guestcost of the facility with facid = 1 by 10%, using values derived from the facility with facid = 0.

###### Question 5: Delete all bookings

```sql
delete from cd.bookings;
```
This query deletes all records from the cd.bookings table, effectively clearing all booking data.

###### Question 6: Delete a specific member

```sql
delete from cd.members
    where memid = 37;
```
This query deletes the member with memid = 37 from the cd.members table

###### Question 7: Find facilities with low member costs

```sql
select facid, name, membercost, monthlymaintenance 
    from cd.facilities 
    where 
        membercost > 0 and 
        (membercost < monthlymaintenance/50.0);
```
This query selects facilities where the membercost is greater than 0 but less than 1/50th of the monthlymaintenance.

###### Question 8: Search facilities by name

```sql
select *
    from cd.facilities 
    where 
        name like '%Tennis%';
```
This query retrieves all facilities where the name contains the word "Tennis".

###### Question 9: Filter facilities by specific IDs

```sql
select *
    from cd.facilities 
    where 
        facid in (1,5);
```
This query selects facilities with facid values of 1 or 5.

###### Question 10: Find members who joined after a specific date

```sql
select memid, surname, firstname, joindate
    from cd.members
    where joindate >= '2012-09-01';
```
This query retrieves members who joined on or after September 1, 2012.

###### Question 11: Combine member and facility names

```sql
select surname 
    from cd.members
union
select name 
    from cd.facilities;
```
This query combines and lists unique names from both cd.members and cd.facilities.

###### Question 12: Find bookings for a specific member

```sql
select bks.starttime 
    from 
        cd.bookings bks
        inner join cd.members mems
            on mems.memid = bks.memid
    where 
        mems.firstname='David'
        and mems.surname='Farrell';
```
This query retrieves the starttime of bookings made by the member named "David Farrell".

###### Question 13: Find bookings for specific tennis courts on a specific day

```sql
select bks.starttime as start, facs.name as name
    from 
        cd.facilities facs
        inner join cd.bookings bks
            on facs.facid = bks.facid
    where 
        facs.name in ('Tennis Court 2','Tennis Court 1') and
        bks.starttime >= '2012-09-21' and
        bks.starttime < '2012-09-22'
order by bks.starttime;      
```
This query retrieves bookings for "Tennis Court 1" and "Tennis Court 2" on September 21, 2012, sorted by start time.

###### Question 14: Find members and their recommenders

```sql
select mems.firstname as memfname, mems.surname as memsname, recs.firstname as recfname, recs.surname as recsname
    from 
        cd.members mems
        left outer join cd.members recs
            on recs.memid = mems.recommendedby
order by memsname, memfname;
```
This query retrieves members' names along with the names of the people who recommended them. If no recommender exists, it shows NULL. Results are sorted by members' surnames and first names.

###### Question 15: Find distinct recommenders

```sql
select distinct recs.firstname as firstname, recs.surname as surname
    from 
        cd.members mems
        inner join cd.members recs
            on recs.memid = mems.recommendedby
order by surname, firstname;
```
This query lists distinct recommenders' names, sorted by surname and first name.

###### Question 16: List members and their recommenders in a single column

```sql
select distinct mems.firstname || ' ' ||  mems.surname as member,
    (select recs.firstname || ' ' || recs.surname as recommender 
        from cd.members recs 
        where recs.memid = mems.recommendedby
    )
    from 
        cd.members mems
order by member;
```
This query lists members and their recommenders as concatenated strings. If no recommender exists, the field is NULL. The result is sorted by member names.

###### Question 17: Count members by recommender

```sql
select recommendedby, count(*) 
    from cd.members
    where recommendedby is not null
    group by recommendedby
order by recommendedby;
```
This query counts the number of members recommended by each recommender. Only non-NULL recommenders are included, and the results are grouped and sorted by recommendedby.


###### Question 18: Calculate total slots by facility

```sql
select facid, sum(slots) as "Total Slots"
    from cd.bookings
    group by facid
order by facid;
```
This query calculates the total slots booked for each facility, grouping by facid and sorting by facility ID.

###### Question 19: Calculate total slots for a specific month

```sql
select facid, sum(slots) as "Total Slots"
    from cd.bookings
    where
        starttime >= '2012-09-01'
        and starttime < '2012-10-01'
    group by facid
order by sum(slots); 
```
This query calculates total slots for each facility during September 2012. Results are grouped by facility and sorted by the total slots in ascending order.

###### Question 20: Monthly slot usage by facility

```sql
select facid, extract(month from starttime) as month, sum(slots) as "Total Slots"
    from cd.bookings
    where extract(year from starttime) = 2012
    group by facid, month
order by facid, month;    
```
This query calculates the total slots booked per facility for each month in 2012. Results are grouped by facid and month and sorted accordingly

###### Question 21: Count distinct members who made bookings

```sql
select count(distinct memid) from cd.bookings;
```
This query counts the number of unique members (memid) who have made bookings in the cd.bookings table.

###### Question 22: Find first booking date per member

```sql
select mems.surname, mems.firstname, mems.memid, min(bks.starttime) as starttime
    from cd.bookings bks
    inner join cd.members mems on
        mems.memid = bks.memid
    where starttime >= '2012-09-01'
    group by mems.surname, mems.firstname, mems.memid
order by mems.memid;
```
This query retrieves the earliest booking date (min(starttime)) for each member who made bookings on or after September 1, 2012. Results are grouped by member information and sorted by memid.

###### Question 23: Count total members with a window function

```sql
select count(*) over(), firstname, surname
    from cd.members
order by joindate;
```
This query retrieves all members along with a total count of all rows (using the count() window function). Results are ordered by joindate.

###### Question 24: Assign row numbers to members by join date

```sql
select row_number() over(order by joindate), firstname, surname
    from cd.members
order by joindate;
```
This query assigns a unique row number to each member based on their joindate, using the row_number() window function. Results are sorted by joindate

###### Question 25: Find the facility with the highest slot usage

```sql
select facid, total from (
    select facid, sum(slots) total, rank() over (order by sum(slots) desc) rank
        from cd.bookings
        group by facid
    ) as ranked
    where rank = 1;
```
This query calculates the total slot usage for each facility and ranks them in descending order. It selects the facility with the highest usage (rank = 1).

###### Question 26: Format member names

```sql
select surname || ', ' || firstname as name from cd.members;
```
This query concatenates members' surnames and first names into the format "Surname, Firstname".

###### Question 27: Find members with special characters in telephone numbers

```sql
select memid, telephone 
    from cd.members
    where telephone ~ '[()]';
```
This query selects members whose telephone field contains parentheses using a regular expression match.

###### Question 28: Count members by first letter of their surname

```sql
select substr(mems.surname,1,1) as letter, count(*) as count 
    from cd.members mems
    group by letter
    order by letter;
```
This query counts the number of members grouped by the first letter of their surname. Results are sorted alphabetically by the letter.

