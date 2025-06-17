# Scala Retail Data Analytics Project

## Project Description

This project uses Apache Spark with Scala to perform data analysis on a retail dataset. The goal is to clean and process the data using the Spark Scala API, perform transformations and aggregations, and extract key business insights.

## Technologies Used

- Apache Spark  
- Scala  
- Almond (Scala Jupyter kernel)  
- Jupyter Notebook  
- Databricks (for comparison and testing)  
- Git & GitHub  

## Data Source

The dataset used is `online_retail_ii.csv`, which contains e-commerce transactions. The file is locally imported and processed using Spark DataFrames in Scala.

## Key Steps

1. Load the data into a Spark DataFrame  
2. Clean the data (remove nulls, remove rows with Quantity ≤ 0)  
3. Add a new column `TotalPrice = Quantity * Price`  
4. Perform aggregations:
   - Total sales by country  
   - Top-selling products by revenue  
   - Top-selling products by quantity  
   - Monthly sales trends  
5. Export data for visualization in Python with Pandas/Matplotlib  

## How to Run

1. Open the Scala Jupyter notebook with Almond kernel  
2. Make sure Spark is properly initialized  
3. Place the CSV file in the accessible path (e.g., `online_retail_ii.csv`)  
4. Run each cell in order to process and analyze the data  
5. Optionally, switch to a Python notebook for plotting with Pandas/Matplotlib  

## Authors

- Amadou Sy  
- Project done as part of the Jarvis training program
