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
data_parent_folder <- file.path(working_directory, "home_location_new_1", "after_war_new_2")
file_pattern <- file.path(data_parent_folder, "*", "*.csv")

# Read CSV files into Spark DataFrame
df <- spark_read_csv(
  sc,
  path = file_pattern,
  memory = TRUE
)

# Group by 'device_aid' and 'combo_location', and calculate the sum of 'total_count'
rdd_1 <- df %>% 
  group_by(device_aid, combo_location) %>% 
  summarise(new_total_count = sum(total_count), .groups = "drop") %>% 
  arrange(device_aid, combo_location)

# Group by 'device_aid', and calculate the percentage based on 'new_total_count'
rdd_2 <- rdd_1 %>% 
  group_by(device_aid) %>% 
  mutate(percent = 100 * (new_total_count / sum(new_total_count))) %>% 
  ungroup() %>% 
  arrange(device_aid, combo_location)

save_location <- file.path(working_directory, "home_location_new_2", "after_war_new_2")

# Write the final DataFrame to CSV
spark_write_csv(rdd_2, save_location)

# Disconnect from Spark
spark_disconnect(sc)
