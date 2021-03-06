#=======================================================================================
#
# File:        IncidentLogAnalysis.R
# Author:      Dave Langer
# Description: This code illustrates using R for event log analysis. Code was written
#              to support the "Introduction to Event Log Mining with R" Meetup dated
#              01/04/2017. More details on the Meetup are available at:
#
#                 https://www.meetup.com/data-science-dojo/events/235913034/
#
#              The code in this file leverages data from the 2014 Business Processing
#              Intelligence Challenge (BPIC). More information/details are available
#              at:
#
#                 http://www.win.tue.nl/bpi/doku.php?id=2014:challenge
#
# NOTE - This file is provided "As-Is" and no warranty regardings its contents are
#        offered nor implied. USE AT YOUR OWN RISK!
#
#=======================================================================================


# Install the following packages if needed.
#install.packages("edeaR", repos = "http://cran.us.r-project.org")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("ggplot2")


# Load packages
library(edeaR)
library(lubridate)
library(dplyr)
library(ggplot2)


#
# Load incident records log - assumes CSV is in current working directory!
#
incident.data <- read.csv("Detail Incident Activity.csv", sep = ";", stringsAsFactors = FALSE)


# What's the structure of the change log data?
str(incident.data)




#=======================================================================================
#
# Perform initial data munging
#
#=======================================================================================


# Convert date strings using lubridate.
incident.data$DateStamp <- dmy_hms(incident.data$DateStamp, tz = "UTC")


# Setup all the factors.
incident.data$Incident.ID <- as.factor(incident.data$Incident.ID)
incident.data$IncidentActivity_Number <- as.factor(incident.data$IncidentActivity_Number)
incident.data$IncidentActivity_Type <- as.factor(incident.data$IncidentActivity_Type)
incident.data$Assignment.Group <- as.factor(incident.data$Assignment.Group)
incident.data$KM.number <- as.factor(incident.data$KM.number)
incident.data$Interaction.ID <- as.factor(incident.data$Interaction.ID)
incident.data$Lifecycle <- as.factor("Start")


# Create a data frame to assign a Month/Year to each case based on the
# earliest record in the log
incidents.month.year <- incident.data %>%
  group_by(Incident.ID) %>%
  arrange(Incident.ID, DateStamp) %>%
  filter(row_number() == 1) %>%
  mutate(Month.Year = floor_date(DateStamp, "month")) %>%
  select(Incident.ID, Month.Year)


# Add the Month/Year to the log
incident.data <- incident.data %>%
  left_join(incidents.month.year, by = "Incident.ID") %>%
  arrange(Incident.ID, DateStamp)


# Check the data
head(incident.data, 10)


#=======================================================================================
#
# Build Event Log
#
#=======================================================================================


incidents.log <- eventlog(eventlog = incident.data,
                          case_id = "Incident.ID",
                          activity_id = "IncidentActivity_Type",
                          activity_instance_id = "IncidentActivity_Number",
                          lifecycle_id = "Lifecycle",
                          resource_id = "Assignment.Group",
                          timestamp = "DateStamp")




#=======================================================================================
#
# Explore the Event Log
#
#=======================================================================================


# In fine R fashion, summarize the event log!
incidents.log %>% summary


# What about case counts over time?
case.counts <- incidents.log %>%
  distinct(Month.Year, Incident.ID) %>%
  group_by(Month.Year) %>%
  summarize(Case.Count = n()) %>%
  arrange(Month.Year)

ggplot(case.counts, aes(x = Month.Year, y = Case.Count)) +
  theme_bw() +
  geom_line(size = 0.75) +
  labs(x = "Month & Year", y = "Count of Cases",
       title = "Case Counts for Complete Log File")


# What about average case throughput over time?
throughput.times <- throughput_time(incidents.log, "case") 

throughput.averages <- incidents.log %>%
  distinct(Month.Year, Incident.ID) %>%
  left_join(throughput.times, by = "Incident.ID") %>%
  group_by(Month.Year) %>%
  summarize(Throughput.Avg = mean(throughput_time)) %>%
  arrange(Month.Year)

ggplot(throughput.averages, aes(x = Month.Year, y = Throughput.Avg)) +
  theme_bw() +
  geom_line(size = 0.75) +
  labs(x = "Month & Year", y = "Average Case Throughput in Days",
       title = "Case Throughput Averages for Complete Log File")

head(throughput.averages, nrow(throughput.averages))


# Let's take a closer look at the case counts over time
case.counts


# Susbet data for the incidents starting in October 2013 and later
incidents.late <- incidents.log %>%
  filter(Month.Year >= ymd("2013-10-01 UTC")) %>%
  distinct(Incident.ID)

# NOTE - You have to use edeaR functions on eventlog objects!
incidents.late <- incidents.log %>%
  filter_case(cases = incidents.late$Incident.ID)


# Check again to ensure we have the right counts for October and beyond.
incidents.late %>%
  distinct(Month.Year, Incident.ID) %>%
  group_by(Month.Year) %>%
  summarize(Case.Count = n()) %>%
  arrange(Month.Year)


# OK, what's the case throughput distribution?
throughput.times <- throughput_time(incidents.late, "case")
summary(throughput.times$throughput_time)
quantile(throughput.times$throughput_time, probs = seq(0.1, 1, 0.1))


# Split on 9.287 days!
incidents.late.short <- filter_throughput_time(incidents.late, upper_threshold = 9.287)
incidents.late.long <- filter_throughput_time(incidents.late, lower_threshold = 9.2871)


# Diplay summary stats for the groups via utility function
display.stats <- function(header, short.value, long.value) {
  cat(paste("\n", header, " for each group:",
            "\n     Short:  ", short.value, 
            "\n     Long:   ", long.value, 
            sep = "")) 
}

display.stats("# of cases", n_cases(incidents.late.short), 
                            n_cases(incidents.late.long))

display.stats("Avg # of activities per case", 
              mean(trace_length(incidents.late.short, "case")$trace_length), 
              mean(trace_length(incidents.late.long, "case")$trace_length))

display.stats("# of traces", n_traces(incidents.late.short), 
                             n_traces(incidents.late.long))


# First question we have for the data - what activities are most frequent?
early.activity.freq <- incidents.late.short %>%
  activity_frequency("activity") %>%
  arrange(desc(absolute))
head(early.activity.freq, 10)

late.activity.freq <- incidents.late.long %>%
  activity_frequency("activity") %>%
  arrange(desc(absolute))
head(late.activity.freq, 10)


# Start building intuition regarding structure of cases
early.activity.presence <- incidents.late.short %>%
  activity_presence()
head(early.activity.presence, 10)

late.activity.presence <- incidents.late.long %>%
  activity_presence()
head(late.activity.presence, 10)




#=======================================================================================
#
# Filter logs and export
#
#=======================================================================================


# Filter complet logs to the activities that account for 90% of the total
short.filtered <- incidents.late.short %>%
  filter_activity_frequency(percentile_cut_off = .90)

long.filtered <- incidents.late.long %>%
  filter_activity_frequency(percentile_cut_off = .90)


# OK, we know that some of the activities have the same DateStamp, fix cases
# so that Open/Closed activities are always the earliest/latest

# Utility functions
adjust.open.closed <- function(row) {
  activity <- row["IncidentActivity_Type"]
  value <- row["DateStamp"]
  
  if (activity == "Open") {
    value <- row["Min.DateStamp"]
  } else if (activity == "Closed") {
    value <- row["Max.DateStamp"]
  }
  
  return(value)
}

fix.open.closed <- function(incidents) {
  # Get data frame of min/max DateStamps by Incident.ID
  min.max <- incidents %>%
    group_by(Incident.ID) %>%
    summarize(Min.DateStamp = min(DateStamp),         
              Max.DateStamp = max(DateStamp))
  
  # Adjust min/max values to ensure earliest/latest
  min.max$Min.DateStamp <- min.max$Min.DateStamp - minutes(1)
  min.max$Max.DateStamp <- min.max$Max.DateStamp + minutes(1)

  # Left outer join min/max values
  incidents <- incidents %>%
    left_join(min.max, by = "Incident.ID")
  
  # Use apply() vectorized processing
  incidents$DateStamp <- apply(incidents, 1, adjust.open.closed)  
  
  return(incidents)
}

short.filtered <- fix.open.closed(short.filtered)
long.filtered <- fix.open.closed(long.filtered)


# Subset columns to minimum needed for import into ProM
cols <- c("Incident.ID", "IncidentActivity_Type", "DateStamp", "Assignment.Group")

short.filtered <- short.filtered[, cols]
long.filtered <- long.filtered[, cols]


# Rename Assignment.Group column for ease import into ProM
res.name <- "org:resource"

names(short.filtered)[4] <- res.name
names(long.filtered)[4] <- res.name


# Write complete event log CSV files for import into ProM
write.csv(short.filtered, file = "ShortFiltered.csv", row.names = FALSE)
write.csv(long.filtered, file = "LongFiltered.csv", row.names = FALSE)

