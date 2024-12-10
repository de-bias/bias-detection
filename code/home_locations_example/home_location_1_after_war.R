# Load libraries
library(arrow)        # For working with Apache Arrow
library(sparklyr)    # For connecting to Spark
library(dplyr)       # For data manipulation

# Set the working directory
working_directory = "/Volumes/rdm08/UKRAINESDK/UKRAINE_SDK_2022/"
setwd(working_directory)

relevant_months_after_war <-  c(
  "02_5",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08"
)


for (relevant_month in relevant_months_after_war) {
  
  # Configure Spark
  config <- spark_config()
  config$`sparklyr.shell.driver-memory` <- '9G'
  
  # Connect to Spark
  sc <- spark_connect(master = "local", config = config)
  
  # Define the data parent folder and file pattern
  data_parent_folder <- file.path(working_directory, "refined_data", "after_war", relevant_month)
  file_pattern <- file.path(data_parent_folder, "*", "*.csv.gz")
  
  # Read CSV files into Spark DataFrame
  df <- spark_read_csv(
    sc,
    path = file_pattern,
    memory = TRUE
  )
  
  # Create a new column 'combo_location' by concatenating 'NAME_2' and 'settlement_type'
  rdd_1 <- df %>% 
    mutate(
      combo_location = paste(
        NAME_1,
        "-",
        NAME_2,
        "(",
        settlement_type,
        ")"
      )
    )
  
  # Extract month, day, and hour from 'date' and 'time' columns
  rdd_2 <- rdd_1 %>% 
    mutate(
      month = month(date),
      day = day(date),
      hour = hour(time)
    )
  
  rdd_5 <- rdd_2 %>% 
    select(device_aid, combo_location, month, day) %>% 
    group_by(device_aid, combo_location, month, day) %>% 
    spark_dataframe() %>% 
    invoke(
      "dropDuplicates",
      list("device_aid", "combo_location", "month", "day")
    ) %>% 
    sdf_register()
  
  # Group by 'device_aid', 'combo_location', and 'month', and calculate the total count
  rdd_6 <- rdd_5 %>% 
    group_by(device_aid, combo_location, month) %>% 
    summarise(total_count = n(), .groups = "drop") %>% 
    arrange(device_aid, combo_location, month) %>% 
    sdf_repartition(partitions = 1)
  
  # Set the save location for the output CSV file
  save_location <- file.path(working_directory, "home_location_new_1", "after_war_new_2", relevant_month)
  
  # Write the final DataFrame to CSV
  spark_write_csv(rdd_6, save_location)
  
  # Disconnect from Spark
  spark_disconnect(sc)
}