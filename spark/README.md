# Spark Retail Data Analysis Project

## Project Description

This project uses Apache Spark on Azure Databricks to perform data analysis on a retail dataset. The goal is to apply the ETL process, explore the data, and extract business insights using Spark DataFrames and SQL.

## Technologies Used

- Apache Spark  
- Azure Databricks  
- PySpark (Python API for Spark)  
- Databricks File System (DBFS)  
- Git & GitHub  

## Data Source

The data comes from a CSV file named `online_retail_ii.csv`, stored in Azure Blob Storage, and imported into Databricks via DBFS.

## Key Steps

1. Load the data into a Spark DataFrame  
2. Clean the data by removing nulls and fixing data types  
3. Add a new column `TotalPrice = Quantity * Price`  
4. Perform aggregations:
   - Total sales by country
   - Top-selling products
   - Monthly sales trends
   - Best customers  
5. Cache/persist data for better performance  
6. Export results or visualize them with graphs  

## How to Run

1. Open the Databricks notebook:  
   `spark/notebook/retail_analysis.ipynb`

2. Make sure your cluster is running and the file `online_retail_ii.csv` is loaded in `/FileStore/tables/`

3. Run the notebook cell by cell

## Authors

- Amadou Sy  
- Project done as part of Jarvis training
