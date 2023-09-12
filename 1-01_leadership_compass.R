### ############################################################################
### ############################################################################
###                                                                          ###
### LEADERSHIP COMPASS                                                             ###
###                                                                          ###
### ############################################################################
### ############################################################################






# Setup and Configuration ------------------------------------------------------

source(
  "0-00_setup_and_configuration.R",
  echo = TRUE,
  max.deparse.length = 1e4
)





# Data Loading -----------------------------------------------------------------

LEADERSHIP_REFERENCE <- read_xlsx(here(
  "raw_data",
  "leadership_compass.xlsx"
  ),
  sheet = "Reference")

# Get names of all sheets and remove reference and example
sheet_names <- excel_sheets(here(
  "raw_data",
  "leadership_compass.xlsx"
))
sheet_names <- sheet_names[!sheet_names %in% c("Reference", "Example")]

# Read and combine sheets into a single data frame
RAW_LEADERSHIP <- map_df(sheet_names, ~{
  sheet_name = .x
  read_xlsx(here(
    "raw_data",
    "leadership_compass.xlsx"
  ),
  sheet = sheet_name) %>% 
    mutate(sheet_name = sheet_name)
})

# Data Cleaning -----------------------------------------------------------

CLEAN_LEADERSHIP <- RAW_LEADERSHIP %>% 
  
  # Remove NA and replace with 0
  mutate(across(!statement, ~ifelse(is.na(.), 0, .)))



# Data Linkage ----------------------------------------------------------





# Data Analysis -----------------------------------------------------------





# Data Visualisation ------------------------------------------------------





