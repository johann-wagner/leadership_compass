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

LINKED_LEADERSHIP <- CLEAN_LEADERSHIP %>% 
  
  left_join(
    LEADERSHIP_REFERENCE,
    join_by(statement)
  )



# Data Analysis -----------------------------------------------------------

ANALYSED_LEADERSHIP <- LINKED_LEADERSHIP %>% 
  mutate(
    movement_x = case_when(
      strongly_disagree == 1 ~ -2*value_x,
      disagree          == 1 ~ -value_x,
      neutral           == 1 ~ 0,
      agree             == 1 ~ value_x,
      strongly_agree    == 1 ~ 2*value_x
    ),
    
    movement_y = case_when(
      strongly_disagree == 1 ~ -2*value_y,
      disagree          == 1 ~ -value_y,
      neutral           == 1 ~ 0,
      agree             == 1 ~ value_y,
      strongly_agree    == 1 ~ 2*value_y
    )
  )



# Data Visualisation ------------------------------------------------------





