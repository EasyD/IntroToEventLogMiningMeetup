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




#=======================================================================================
#
# Build Event Log
#
#=======================================================================================


incident.log <- eventlog(eventlog = incident.data,
                         case_id = "Interaction.ID",
                         activity_id = "IncidentActivity_Type",
                         activity_instance_id = "IncidentActivity_Number",
                         lifecycle_id = "Lifecycle",
                         resource_id = "Assignment.Group",
                         timestamp = "DateStamp")
incident.log %>% summary




#=======================================================================================
#
# Explore the Event Log
#
#=======================================================================================


# Given the business problem a prime thing we're interested in - how are interactions 
# counts changing over time?
interaction.counts <- incident.log %>%
  mutate(Month.Year = floor_date(DateStamp, "month")) %>%
  group_by(Month.Year) %>%
  summarize(Interaction.Count = n()) %>%
  arrange(Month.Year)

# Plot it!
ggplot(interaction.counts, aes(x = Month.Year, y = Interaction.Count)) +
  theme_bw() +
  geom_line() +
  labs(x = "Year & Month",
       y = "Count of Interactions")



# Another thing we're interested in is how long do interactions take to resolve?
interaction.resolution <- throughput_time(incident.log, "case")

summary(interaction.resolution$throughput_time)
quantile(interaction.resolution$throughput_time,
         probs = seq(0.1, 1, 0.1))


# OK, 90% of interactions take less than 11 days. Filter the log down.
incident.log.filtered <- filter_throughput_time(incident.log,
                                                lower_threshold = 0,
                                                upper_threshold = 11)

# Get the throughputs from filtered log
interaction.resolution <- throughput_time(incident.log.filtered, "case")

# Plot it!
ggplot(interaction.resolution, aes(x = throughput_time)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(x = "Interaction Duration in Days",
       y = "Count of Interactions")
  