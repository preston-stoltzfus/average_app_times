install.packages("tidyverse")
install.packages("skimr")
install.packages("kableExtra")
library(tidyverse)
library(skimr)
library(kableExtra)

# importing data
app_trends <- read_csv("app_trends_2023.csv")

# selecting out rows needed for analysis
s_app_trends <- app_trends %>% 
  select(patientid, description, staffid, datein, dateout, census_reason_txt, display_id)
View(s_app_trends)


# cleaning begins

# renaming column "census_reason_txt" to "visit_details"
s_app_trends <- s_app_trends %>% 
  rename(visit_details = census_reason_txt)

# renaming column "display_id" to "location"
s_app_trends <- s_app_trends %>% 
  rename(location = display_id)

# Convert character columns to lowercase and remove leading/trailing whitespace
s_app_trends <- s_app_trends %>%
  mutate_if(is.character, tolower) %>%
  mutate_if(is.character, str_trim)

# creating new columns needed for analysis later. The app_type column will label different types of appointments
# and the app_subtype column will help differentiate different tech app categories
s_app_trends$app_type <- NA
s_app_trends$app_subtype <- NA


# fixing "datein" and "dateout" columns & creating "duration_min" column for total time(minutes) patient spent in hospital

# Convert "datein" and "dateout" columns to proper date/time format
s_app_trends$datein <- as.POSIXct(s_app_trends$datein, format = "%m/%d/%Y %H:%M:%S")
s_app_trends$dateout <- as.POSIXct(s_app_trends$dateout, format = "%m/%d/%Y %H:%M:%S")

# making a column of duration in minutes with the "datein" and "dateout" columns and rounding the result. 
# If you want to calculate the duration in hours, use units = "hours" instead
s_app_trends$duration_min <- as.numeric(difftime(s_app_trends$dateout, s_app_trends$datein, units = "mins"))
s_app_trends$duration_min <- round(s_app_trends$duration_min, 0)

# removing clients who were not properly checked out. Although impossible to find every error, the hospital opens 7am
# on monday and closes at 2pm on saturday, they do not keep patients past that time on sunday. That amount of time
# is 8256 minutes. If I remove any patients that are checked in over that time that will at least remove the extreme errors.
# I will also remove any duration 1 minute and under for the same reason, that's to fast for the appointment to happen
s_app_trends <- subset(s_app_trends, duration_min > 1 & duration_min <= 8256)


# cleaning the "staffid" column

# Each number in "staffid" correlates to a specific doctor, need to remove the "L" and "M" in front of numbers, these 
# signify location of where the doctor is at but we have a separate column for that and just want one ID for the doctor 
s_app_trends$staffid <- gsub("[A-Za-z](?=[0-9])", "", s_app_trends$staffid, perl = TRUE)

# finding instances of "ltk", "mtk", and "tktk" and changing it to "tk" for the same reason listed above
s_app_trends$staffid <- gsub("l|mtk|tktk", "tk", s_app_trends$staffid)

# there are 5 rows that correlate to assistants/technicians but those rows are errors/and or used for in house things
# 0 & 22 also need removed, 22 is a relief vet where management doesn't want those values messing things up and 0 is in house stuff
s_app_trends <- s_app_trends[!(s_app_trends$staffid %in% c("ceb", "ed", "mc", "stkk", "vc", "0", "22")), ]


# cleaning the "description" column

# change na values to unknown
s_app_trends$description <- ifelse(is.na(s_app_trends$description), "unknown", s_app_trends$description)

# changing different names for the same type of appointment to a consistent name
s_app_trends <- s_app_trends %>%
  mutate(description = ifelse(description %in% c("exam", "exam with vaccines"),
                              "preventative care exam", description))
s_app_trends <- s_app_trends %>%
  mutate(description = ifelse(description %in% c("not wellness"),
                              "sick", description))


# creating data in "app_type" column

# adding data to the new app_type. I have 6 different categories to make; doctor, tech_app, admit, other, doctor/admit, and na.
s_app_trends <- s_app_trends %>%
  mutate(app_type = case_when(
    description %in% c("acupuncture", "behavior consultation", "emergency", "euthanasia", "exotic exam", "new client", 
                       "new pet", "pre surgical exam", "preventative care exam", "quality of life", "recheck", 
                       "senior exam", "sick", "travel health certificate", "suture removal" ) ~ "doctor",
    description %in% c("cytopoint injection", "diabetic consultation", "discharge", "lab work", "laser", 
                       "technician appointment", "tnt" ) ~ "tech_app",
    description %in% c("dental", "drop off - admit", "feline castrate", "glucose curve", "jss surgery", 
                       "laparascopic surgery", "short procedure", "surgery"  ) ~ "admit",
    description %in% c("boarding", "with dr approval", "scheduling notes") ~ "other",
    description %in% c("ultrasound") ~ "doctor/admit",
    TRUE ~ NA_character_  # If none of the conditions match, set the value as NA
  ))
# "consultation" & "radiology" are words not categorized because they can be either doctor, admit, or tech_apps. 
# In order to classify those words, I look in the "staffid" column. It's listed as either "tk" for a tech_app or
# with a doctors number if it's a doctors number the appointment type could be a range of different things so 
# will set to doctor/admit
s_app_trends$app_type <- ifelse(is.na(s_app_trends$app_type) & s_app_trends$staffid == "tk", "tech_app",
                                ifelse(is.na(s_app_trends$app_type), "doctor/admit", s_app_trends$app_type))

# creating data in "app_subtype" column. Really I just need to asses which appointments are diabetic consults, 
# unfortunately it's not scheduled the same way all the time and some are doctor appointments we we need to select out just
# the tech appointment diabetic consults. 

# creating "diabetic consult" in "app_subtype" only if it's a tech_app
s_app_trends <- s_app_trends %>%
  mutate(
    app_subtype = ifelse(
      (grepl("diabetic", description, ignore.case = TRUE) | 
         grepl("diabetic", visit_details, ignore.case = TRUE)) &
        app_type == "tech_app",
      "diabetic consult",
      app_subtype)
     )

# create a column called "details". This is so we can categorize things (in particular tech appointments) in a clearer more
# error free way. I am also selecting out the only appointment types that are needed for analysis
s_app_trends <- s_app_trends %>%
  mutate(details_placeholder = NA_character_) %>%
  mutate(details = case_when(
    app_subtype == "diabetic consult" ~ "diabetic consult",
    description %in% c("sick", "preventative care exam", "recheck", 
                       "senior exam", "new pet", "new client", "emergency", "exotic exam", 
                       "suture removal", "quality of life", "tnt", "lab work", "laser") ~ description,
    app_type == "tech_app" & is.na(details_placeholder) ~ "misc tech appointment",
    TRUE ~ NA_character_
  )) %>%
  select(-details_placeholder)  # Remove the placeholder column


# analysis and visuals begin

# Finding the average duraiton. Create a new data frame for the results. Using median to protect from large outliers that exist in data. 
median_appointment_times <- s_app_trends %>%
  filter(!is.na(details)) %>%  # Exclude rows with NA in details column
  mutate(location_column = location) %>%
  group_by(details, app_type, location_column) %>%
  summarise(median_duration = median(duration_min)) %>%
  pivot_wider(names_from = location_column, values_from = median_duration)

# Melt the data for plotting
melted_data <- median_appointment_times %>%
  gather(location, average_time, mpv:met) %>%
  filter(!is.na(average_time))  # Exclude NA values

# Calculate the date range from now to 2 years ago
current_year <- year(Sys.Date())
date_range_start <- as.Date(paste0(current_year - 2, "-01-01"))
date_range_end <- Sys.Date()

# Format the date range for the file names
formatted_date_range <- paste(format(date_range_start, "%Y"), format(date_range_end, "%Y"), sep = "-")

# Create the title with the years
title_text <- paste("Average Appointment Times by Category and Location",
                    formatted_date_range, sep = " ")

# Create a grouped bar plot with centered title and modified y-axis
bar_plot <- ggplot(melted_data, aes(x = details, y = average_time, fill = location)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = title_text,
       x = "Appointment Type", y = "Average Appointment Time (Minutes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Centered title
  scale_y_continuous(breaks = seq(0, ceiling(max(melted_data$average_time)), 5))  # Modify y-axis breaks


# exporting the data

# Create a folder if it doesn't exist
if (!dir.exists("project_results")) {
  dir.create("project_results")
}

# Export the cleaned s_app_trends w/ year info in title
write.csv(s_app_trends, file = paste("project_results/cleaned_appointment_times_", formatted_date_range, ".csv", sep = ""), row.names = FALSE)

# Export the median_appointment_times w/ year info in title
write.csv(median_appointment_times, file = paste("project_results/average_appointment_times_", formatted_date_range, ".csv", sep = ""), row.names = FALSE)

# Set the file path for saving the bar plot
image_path <- file.path("project_results", paste("bar_plot_average_appointment_times_", formatted_date_range, ".png", sep = ""))
# Save the bar plot as a high quality image w/ year info in title
ggsave(filename = image_path, plot = bar_plot, width = 10, height = 6, dpi = 400, bg = "white")

# Export median_appointment_times as an image w/ year info in title
webshot::install_phantomjs()

formatted_table <- median_appointment_times %>%
  mutate(across(starts_with("20"), ~ paste0(round(.), " min"), .names = "new_{.col}"))

table_image_path <- file.path("project_results", paste("average_appointment_times_", formatted_date_range, ".png", sep = ""))

kable(formatted_table, "html") %>%
  kable_styling() %>%
  as_image(file = table_image_path)

