# Load libraries
library(arrow)        # For working with Apache Arrow
library(sparklyr)    # For connecting to Spark
library(dplyr)       # For data manipulation

# Set the working directory
working_directory = "/Volumes/rdm08/UKRAINESDK/UKRAINE_SDK_2022"
setwd(working_directory)

# Configure Spark
config <- spark_config()
config$`sparklyr.shell.driver-memory` <- '8G'

# Connect to Spark
sc <- spark_connect(master = "local", config = config)

# Define the data parent folder and file pattern
data_parent_folder <- file.path(working_directory, "home_location_new_2", "before_war_new_2")
file_pattern <- file.path(data_parent_folder, "*.csv")

# Read CSV files into Spark DataFrame
df <- spark_read_csv(
  sc,
  path = file_pattern,
  memory = TRUE
)

# Determine if a location is considered home based on the percentage
rdd_3 <- df %>%
  mutate(is_home = case_when(
    percent > 50 ~ TRUE,
    TRUE ~ FALSE
  ))

# Filter for rows where 'is_home' is TRUE and repartition the DataFrame
rdd_4 <- rdd_3 %>%
  filter(is_home == TRUE) %>%
  sdf_repartition(partitions = 1)

save_location <- file.path(working_directory, "home_location_new_3", "before_war_new_2")

# Write the final DataFrame to CSV
spark_write_csv(rdd_4, save_location)

# Disconnect from Spark
spark_disconnect(sc)
