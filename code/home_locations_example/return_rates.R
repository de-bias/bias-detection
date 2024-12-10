# Load libraries
library(arrow)        # For working with Apache Arrow
library(sparklyr)    # For connecting to Spark
library(dplyr)       # For data manipulation

# Set the working directory
working_directory = "/Volumes/UKRAINE_SDK_2022"
setwd(working_directory)

# Configure Spark
config <- spark_config()
config$`sparklyr.shell.driver-memory` <- '9G'

# Connect to Spark
sc <- spark_connect(master = "local", config = config)

# Define the data parent folder and file pattern for users_movements_3
data_parent_folder_2 <- file.path(working_directory, "home_location_new_3", "before_war_new_2")
file_pattern_2 <- file.path(data_parent_folder_2, "*.csv")

after_war_folder <- file.path(working_directory, "home_location_new_2", "after_war_new_2")
after_war_file_pattern <- file.path(after_war_folder, "*.csv")

# Read CSV files into Spark DataFrame for users_movements_3
home_location_new_3_df <- spark_read_csv(
  sc,
  path = file_pattern_2,
  memory = TRUE
)

after_war_home_location_2 <- spark_read_csv(
  sc,
  path = after_war_file_pattern,
  memory = T
)

ids_after_war_that_have_home_locs_before_war <- semi_join(
  after_war_home_location_2,
  home_location_new_3_df,
  by=c("device_aid")
)

ids_after_war_that_have_home_locs_before_war %>% select(device_aid) %>% distinct() %>% count()

spark_disconnect(sc)
