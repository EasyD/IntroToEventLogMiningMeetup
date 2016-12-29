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
#install.packages("edeaR")
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
incident.data$DateStamp <- dmy_hms(incident.data$DateStamp)


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



#=======================================================================================
#
# Build Event Log
#
#=======================================================================================


incidents.log <- eventlog(eventlog = incident.data,
                         case_id = "Interaction.ID",
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
  distinct(Month.Year, Interaction.ID) %>%
  group_by(Month.Year) %>%
  summarize(Case.Count = n()) %>%
  arrange(Month.Year)

ggplot(case.counts, aes(x = Month.Year, y = Case.Count)) +
  theme_bw() +
  geom_line(size = 0.75) +
  labs(x = "Month & Year", y = "Count of Cases",
       title = "Case Counts for Complete Log File")


# What about activity counts over time?
activity.counts <- incidents.log %>%
  distinct(Month.Year, IncidentActivity_Number) %>%
  group_by(Month.Year) %>%
  summarize(Activity.Count = n()) %>%
  arrange(Month.Year)

ggplot(activity.counts, aes(x = Month.Year, y = Activity.Count)) +
  theme_bw() +
  geom_line(size = 0.75) +
  labs(x = "Month & Year", y = "Count of Activities",
       title = "Activity Counts for Complete Log File")


# What about average case throughput over time?
throughput.times <- throughput_time(incidents.log, "case") 

throughput.averages <- incidents.log %>%
  distinct(Month.Year, Interaction.ID) %>%
  left_join(throughput.times, by = "Interaction.ID") %>%
  group_by(Month.Year) %>%
  summarize(Throughput.Avg = mean(throughput_time)) %>%
  arrange(Month.Year)

ggplot(throughput.averages, aes(x = Month.Year, y = Throughput.Avg)) +
  theme_bw() +
  geom_line(size = 0.75) +
  labs(x = "Month & Year", y = "Average Case Throughput in Days",
       title = "Case Throughput Averages for Complete Log File")

head(throughput.averages, nrow(throughput.averages))


# Susbet data for the incidents earlier than October 2013
incidents.early <- incidents.log %>%
  filter(Month.Year < '2013-10-01') %>%
  distinct(Interaction.ID)

# NOTE - You have to use edeaR functions on eventlog objects!
incidents.early <- incidents.log %>%
  filter_case(cases = incidents.early$Interaction.ID)

# Susbet data for the incidents starting in October 2013 and later
incidents.late <- incidents.log %>%
  filter(Month.Year >= '2013-10-01') %>%
  distinct(Interaction.ID)

# NOTE - You have to use edeaR functions on eventlog objects!
incidents.late <- incidents.log %>%
  filter_case(cases = incidents.late$Interaction.ID)


# Whoa! We have a problem
nrow(incidents.early) + nrow(incidents.late)

bad.ID <- intersect(incidents.early$Interaction.ID, incidents.late$Interaction.ID)

bad.obs <- incidents.log %>%
  filter(Interaction.ID == bad.ID) %>%
  arrange(Interaction.ID, DateStamp)


# Remove bad observations
incidents.early <- incidents.early %>%
  filter_case(case = bad.ID, reverse = TRUE)

incidents.late <- incidents.late %>%
  filter_case(case = bad.ID, reverse = TRUE)


# Diplay summary stats for the groups via utility function
display.stats <- function(header, early.value, late.value) {
  cat(paste("\n", header, " for each group:",
            "\n     Early:  ", early.value, 
            "\n     Late:   ", late.value, 
            sep = "")) 
}

display.stats("# of cases", n_cases(incidents.early), 
                            n_cases(incidents.late))

display.stats("Avg # of activities per case", 
              mean(trace_length(incidents.early, "case")$trace_length), 
              mean(trace_length(incidents.late, "case")$trace_length))

display.stats("# of traces", n_traces(incidents.early), 
                             n_traces(incidents.late))


# First question we have for the data - what activities are most frequent?
early.activity.freq <- incidents.early %>%
  activity_frequency("activity") %>%
  arrange(desc(absolute))
head(early.activity.freq, 10)

late.activity.freq <- incidents.late %>%
  activity_frequency("activity") %>%
  arrange(desc(absolute))
head(late.activity.freq, 10)


# Start building intuition regarding structure of cases
early.activity.presence <- incidents.early %>%
  activity_presence()
head(early.activity.presence, 10)

late.activity.presence <- incidents.late %>%
  activity_presence()
head(late.activity.presence, 10)


# Compare the resources between each group
early.resources <- incidents.early %>%
  resource_involvement("resource") %>%
  arrange(desc(absolute))

late.resources <- incidents.late %>%
  resource_involvement("resource") %>%
  arrange(desc(absolute))

# Plot differences in top 10 resources
early.resources$group <- "Early"
late.resources$group <- "Late"

cols <- c("group", "Assignment.Group", "absolute")
resources.df <- rbind(early.resources[1:10, cols],
                      late.resources[1:10, cols])
resources.df$group <- factor(resources.df$group,
                             levels = c("Early", "Late"))

ggplot(resources.df, aes(x = reorder(Assignment.Group, -absolute), y = absolute)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  facet_grid(~ group) + 
  labs(x = "Resource/Team",
       y = "Resource/Team Activity Count",
       title = "Top 10 Resources/Teams by Throughput Grouping") +
  theme(axis.text.x = element_text(angle = -90))


# Compare the counts of self-loops in cases
early.self.loops <- incidents.early %>%
  number_of_selfloops("repeat", "case") %>%
  arrange(desc(absolute))
summary(early.self.loops$absolute)

late.self.loops <- incidents.late %>%
  number_of_selfloops("repeat", "case") %>%
  arrange(desc(absolute))
summary(late.self.loops$absolute)


#=======================================================================================
#
# Filter logs and export
#
#=======================================================================================


# Filter logs to the activities that account for 80% of the total
early.filtered <- incidents.early %>%
  filter_activity_frequency(percentile_cut_off = 0.8)

late.filtered <- incidents.late %>%
  filter_activity_frequency(percentile_cut_off = 0.8)


# Write XES-files
write_xes(early.filtered)
write_xes(late.filtered)
