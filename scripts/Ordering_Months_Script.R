#== Ordering Months: R Walkthrough ==#

# Loading in the appropriate libraries====
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Loading the data into the environment====
Patients <- read_csv("data/Patients.csv")

#Let's look at the data set===
Patients

#We see that we have months here and a count for each of them. We decide we just want to make a simple bar chart of these counts.#

# Making a ggplot with the data "as-is"====
Patientgraph <- ggplot(Patients, aes(x = Month, y = `New Patients`)) +
  geom_bar(stat = "identity", fill = "#693ead")

# Let's view the graph===
Patientgraph

#The months are organized alphabetically. Ggplot does this automatically but we want this in chronological order.#

# We could fix this by manually setting the factors. To do this, we'd need to know the months we have in the dataset. This is done with the "unique" function===
unique(Patients$Month)

#It also helps to confirm the varibale type that you're dealing with. We want factors, not characters.#
class(Patients$Month)

#We can see the order is (feb, may, aug, sept, dec).#
#We can do this manually. Doing it this way is cumbersome though because the spelling has to be exact, (capitalization, spelling, etc.)#

# Let's make a new data set called "patients2" that has this change====
Patients2 <- Patients %>%
  mutate(Month = factor(Month, levels = c("February","May","August","September","December")))

# Let's plot it again now===
Patientgraph2 <- ggplot(Patients2, aes(x = Month, y = `New Patients`)) +
  geom_bar(stat = "identity", fill= "#693ead")

# Let's view the graph now===
Patientgraph2

#This is exactly what we're looking for! But there's a better way to do this without having to write it out manually.#
#We can use the "month.name" constant that comes with base R to help us with this. This will only work if our months are formatted in the same way and spelled correctly. If your months are abbreviated, you can use the "month.abb" constant instead, as long as the formatting of your months match.# 

# Let's look at the "month.name" constant first before using it====
month.name

#This returns a vector of 12 strings. Each month of the year. Because this is already programmed into base R, this will help us to avoid cumbersome coding. If you notice, the months here are ordered chronologically instead of alphabetically which is what we want.#

#We want R to compare the months that we have present in our dataset to the months within the "month.name" constant. We'd like for R to order it this way as well (chronologically). We can do so by subsetting out relevant months that are present within our "Months" column in our data set. We do this with the %in% operator.#

# We'll do this with a new dataset called "Patients3"====
Patients3 <- Patients %>%
  mutate(Month = factor(Month, levels = month.name[month.name %in% unique(Month)]))

# Let's plot it again ===
Patientgraph3 <- ggplot(Patients3, aes(x = Month, y = `New Patients`)) +
  geom_bar(stat = "identity", fill= "#693ead")

# And view it===
Patientgraph3 

#Perfect! This would come in handy and save us a bit of time when trying to reorder our months. The code above tells R to only use the months in the "month.name" constant that are already present within our data set. We do this by subsetting (use of the square brackets) This signals R to look inside the month.name constant and only pull out the months that we need.#

#So what if you have a dataset that has every month in it already? Technically, your code can be even simpler than this. Let's have a look.#

# We'll load in a new dataset that has the complete year of records (12 Months) in it and name it "Patients_complete" ====
Patients_complete <- read_csv("data/Patients_complete.csv")

# Let's look at the dataset we have now===
Patients_complete

#We can see all of the 12 months are accounted for, capitalized, and spelled correctly. Let's graph it.#

# Plotting Patients_complete====
Patientgraph4 <- ggplot(Patients_complete, aes(x = Month, y = `New Patients`)) +
  geom_bar(stat = "identity", fill= "#693ead")

# Let's view it===
Patientgraph4

#All of our months are present, but again, ggplot ordered them alphabetically. We fix this by factoring again, but this time, because we have every month present and accounted for we can shorten our code a bit.#

# Refactoring for "Patients_complete" and storing it in a new dataset called "Patients4"====
Patients4 <- Patients_complete %>%
  mutate(Month = factor(Month, levels = month.name))

# Let's plot it now===
Patientgraph5 <- ggplot(Patients4, aes(x = Month, y = `New Patients`)) +
  geom_bar(stat = "identity", fill= "#693ead")

# And view it===
Patientgraph5

#Perfect! You can definitely use that code instead if you know that all of your months will be presented in your data set and spelled, capitalized properly, however, I would caution against this if you have a dataset that might change. Using the first version of factoring protects your program from throwing an error if all the months are not present in your data.

#So finally, what if you have a dataset that doesn't have all the months, but you want to show all of the months anyway (i.e, show zeros for months with no counts?). Let's go back to our first dataset "Patients".#

#Previously, we saw that the "Patients" dataset was missing the months January, April, March, June, July, October, and November.#
#To get these months on to the graph we have to create them. They currently don't exist in the data set. We should still consider these months valid data points for this purpose, but only with a count of zero. We can do this while storing the result into a new dataset called "Patients_modified" with the following code.#

# Adding missing months to the Patients_modified set by turning month.name constant into a tibble, joining it to the exiting data frame and filling in the "New Patients variable with "0"====
Patients_modified <- left_join(tibble("Month" = month.name),Patients, by = "Month") 
Patients_modified <- Patients_modified %>%
  mutate(`New Patients` = ifelse(is.na(`New Patients`),0,`New Patients`)) %>%
  mutate(Month = factor(Month, levels = month.name))

# Let's plot it now===
Patientgraph6 <- ggplot(Patients_modified, aes(x = Month, y = `New Patients`)) +
  geom_bar(stat = "identity", fill= "#693ead") 

#And view it===
Patientgraph6