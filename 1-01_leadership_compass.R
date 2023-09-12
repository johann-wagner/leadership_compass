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
    mutate(person_name = sheet_name)
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

FINAL_LEADERSHIP_POSITION <- ANALYSED_LEADERSHIP %>% 
  
  group_by(person_name) %>% 
  
  summarise(
    total_x = sum(movement_x),
    total_y = sum(movement_y)
  )



# Data Visualisation ------------------------------------------------------

FINAL_LEADERSHIP_POSITION %>% 
  
  ggplot(aes(
    x = total_x,
    y = total_y,
    colour = person_name
  )) +
  
  geom_vline(
    aes(xintercept = 0),
    linewidth = 1.5,
    color = "firebrick",
    linetype = "dashed"
    ) +
  geom_hline(
    aes(yintercept = 0),
    linewidth = 1.5,
    color = "firebrick",
    linetype = "dashed"
  ) +
  geom_point(
    size = 8
  ) +
  
  scale_x_continuous(
    limits = c(-40, 40),
    sec.axis = sec_axis(~., name = "North - Mobilisers")
  ) +
  scale_y_continuous(
    limits = c(-40, 40),
    sec.axis = sec_axis(~., name = "West - Teachers")
  ) +
  scale_colour_viridis_d() +
  
  labs(
    title  = "Leadership Compass Results",
    x      = "South - Nurturer",
    y      = "East - Visionaries",
    colour = "Person"
  ) +
  
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10), size = 15),
    axis.title.y = element_text(margin = margin(r = 10), size = 15),
    axis.text.x  = element_blank(), #remove x axis labels
    axis.ticks.x = element_blank(), #remove x axis ticks
    axis.text.y  = element_blank(),  #remove y axis labels
    axis.ticks.y = element_blank(),  #remove y axis ticks
    plot.title   = element_text(face = "bold",
                              margin = margin(10, 0, 10, 0),
                              size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
    )



