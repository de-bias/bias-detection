# Load libraries
library(arrow)        # For working with Apache Arrow
library(sparklyr)    # For connecting to Spark
library(dplyr)       # For data manipulation

# Set the working directory
working_directory = "/Volumes/rdm08/UKRAINESDK/UKRAINE_SDK_2022"
setwd(working_directory)

# Configure Spark
config <- spark_config()
config$`sparklyr.shell.driver-memory` <- '9G'

# Connect to Spark
sc <- spark_connect(master = "local", config = config)

# Define the data parent folder and file pattern for users_movements_3
data_parent_folder_2 <- file.path(working_directory, "home_location_new_3", "before_war_new_2")
file_pattern_2 <- file.path(data_parent_folder_2, "*.csv")

# Read CSV files into Spark DataFrame for users_movements_3
home_location_new_3_df <- spark_read_csv(
  sc,
  path = file_pattern_2,
  memory = TRUE
)

home_location_new_3_df_1 <- home_location_new_3_df %>% 
  ft_regex_tokenizer(
    input_col = "combo_location",
    output_col = "combo_location_2",
    pattern = "\\s*\\(\\s*|\\s*\\)\\s*"
  ) %>% 
  sdf_separate_column(
    "combo_location_2",
    into =  c("loc_NAME_1_and_2", "loc_Settlement_Type")
  ) %>% 
  select(
    -c("combo_location_2")
  )

home_location_new_3_df_2 <- home_location_new_3_df_1 %>% 
  ft_regex_tokenizer(
    input_col = "loc_NAME_1_and_2",
    output_col = "loc_NAME_1_and_2_2",
    pattern = "\\s*-\\s*",
  ) %>% 
  sdf_separate_column(
    "loc_NAME_1_and_2_2",
    into =  c("loc_NAME_1", "loc_NAME_2")
  ) %>%
  select(
    -c("loc_NAME_1_and_2_2")
  )

home_location_new_3_df_3 <- home_location_new_3_df_2 %>% 
  group_by(loc_NAME_1, loc_NAME_2) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  arrange(loc_NAME_1, loc_NAME_2)

save_home_location_summary <- file.path(working_directory, "home_location_new_4")

spark_write_csv(
  home_location_new_3_df_3, 
  save_home_location_summary
)

spark_disconnect(sc)
