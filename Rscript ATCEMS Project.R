remotes::install_github("ianmoran11/mmtable2")
install.packages("gt")
install.packages("tidyverse")
library(mmtable2)
library(gt)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)

#Reformatting Data
University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2 %<>%
  mutate(Time_PhonePickUp_Date= as.Date(Time_PhonePickUp_Date, format= "%m/%d/%Y"))
University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2$year <- 
  format(as.Date(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2$Time_PhonePickUp_Date, format="%Y-%m-%d"),"%Y")
University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2 <- 
  rename(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2, Assigned = Time_First_Unit_Assigned)
University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2 <- 
  rename(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2, Enroute = Time_First_Unit_Enroute)
University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2 <- 
  rename(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2, Arrived = Time_First_Unit_Arrived)

#Data Filters
mod_ATCEMS_data <-filter(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2, 
                         Problem %in% c("Respiratory", "Sick", "Fall"))
by_year_data <-group_by(mod_ATCEMS_data, Problem, year)
meandata <- summarize(by_year_data,
                     mean_assigned = mean(Assigned, na.rm = TRUE),
                     mean_enroute = mean(Enroute, na.rm = TRUE),
                     mean_arrived = mean(Arrived, na.rm = TRUE))

#Problem in ATCEMS overall 2019-2020
ggplot(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2, aes(x = Problem)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black",
           width = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25, size = 6.5))+
  labs(x = "Problem", 
       y = "Frequency", 
       title = "Problem in ATCEMS Service Area",)

#Problem in ATCEMS overall, broken down by year
ggplot(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2, 
       aes(x = Problem,
           fill = year)) + 
  geom_bar(color="black",
           position = "dodge",
           width = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.25, size = 6.5))+
  labs(x = "Problem", 
       y = "Frequency", 
       title = "Problem in ATCEMS Service Area")

#Total Amount of Calls, broken down by year
ggplot(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2, aes(x = year)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black",
           width = 0.5) +
  labs(x = "Year", 
       y = "Amount of Emergency Calls", 
       title = "Amount of Emergency Calls in ATCEMS",
       subtitle = "Broken Down by Year, 2019 vs 2020")

#Problem in ATCEMS, top 3 calls
ggplot(mod_ATCEMS_data, 
       aes(x = Problem,
           fill = year)) +
  geom_bar(color = "black",
           position = "dodge") +
  labs(title = "Problem in ATCEMS Service Area",
       subtitle = "Broken Down by 3 Most Frequent Calls",
       x = "Problem",
       y = "Frequency")

#Frequency of Dispatches Graph
ggplot(University_of_Texas_Good_Systems_and_ATCEMS_incidents_data2,
       aes(x = Priority_Number,
           fill = year)) +
  geom_bar(color = "black",
           position = "dodge") +
  scale_x_continuous(breaks = seq(1, 15, 1)) +
  labs(title = "Frequency of Priority Dispatches in ATCEMS",
       subtitle = "By Year (2019 vs 2020)",
       x = "Priority Dispatch Number",
       y = "Frequency") 

#Chief 3 complaint response times, organized by year (2019 v 2020)
ggplot(meandata, aes(x = year, y = mean_enroute))+
  geom_bar(stat = "identity",
           fill = "cornflowerblue",
           color = "black",
           width = 0.5)+
  facet_wrap(~Problem, ncol = 1) +
  labs(title = "Mean Response Time by Year in ATCEMS", 
       subtitle = "2019 vs 2020",
       x = "Year",
       y = "Mean Response Time in Minutes (En Route Time)")

#Table
data_wrangled <- mod_ATCEMS_data %>%
  group_by(Problem, year) %>%
  summarise(across(.cols = c(Assigned, Enroute, Arrived), .fns = mean)) %>%
  ungroup() %>%
  pivot_longer(cols = c(Assigned, Enroute, Arrived), names_to = "Part_of_Response", values_to = "Mean_Time")

data_wrangled

main_table <- data_wrangled %>%
  mutate(Mean_Time = round(Mean_Time, 2)) %>%
  mmtable(cells = Mean_Time, table_name = "Mean Time") +
  header_left(Problem) +
  header_left_top(year) +
  header_top(Part_of_Response) +
  header_format(Part_of_Response, list(cell_text(transform = "capitalize"))) +
  header_format(Problem, list(cell_text(transform = "capitalize"))) +
  table_format(
    locations = list(
      cells_body(rows = c(2,5))
    ),
    style = list(
      cell_borders(sides = "top", color = "grey")
    )
  )

main_table %>%
  gt::tab_header( 
      title = "Mean Response Times for 3 Most Common Problems in ATCEMS, 2019 vs 2020",
      subtitle = "Broken Down by Part of Response")