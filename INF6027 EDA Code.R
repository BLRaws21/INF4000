#install these packages before running the code
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")

#Load in the packages every time you restart R Studio 
library(ggplot2)
library(dplyr)
library(readxl)

#Initial Datasets
total_neetdata <- read_excel("clean_NEET Dataset.xlsx", sheet = "People - SA")
men_neetdata <- read_excel("clean_NEET Dataset.xlsx", sheet = "Men - SA")
women_neetdata <- read_excel("clean_NEET Dataset.xlsx", sheet = "Women - SA")

#re-scales columns 2-5 to represent thousands by unit
total_neetdata[,2:5] <- ceiling(total_neetdata[,2:5] / 1000)
men_neetdata[,2:5] <- ceiling(men_neetdata[,2:5] / 1000)
women_neetdata[,2:5] <- ceiling(women_neetdata[,2:5] / 1000)

#merge process for three selected datasets
merged_data <- merge(total_neetdata, men_neetdata, by = "seasonal_periods", sort = FALSE)
merged_data <- merge(merged_data, women_neetdata, by = "seasonal_periods", sort = FALSE)

#data frame for initial gender pie for the 3rd Quarter of 2024
gdon_2024Q3 <- data.frame(
  Gender = factor(c("Male", "Female")),
  Value = c(merged_data$men_total[92], merged_data$women_total[92])
)

# Using percentage formula and rounding to two decimal places
gdon_2024Q3$Percentage <- round(gdon_2024Q3$Value / sum(gdon_2024Q3$Value) * 100, 2) 

#setting colourblind friendly colour palette for donut chart
colour_palette <- c("purple","green")

#creating donut chart for NEETs by Gender 2024 Q3 
gdon_chart1 <- ggplot(gdon_2024Q3, aes(x = 2, y = Value, fill = Gender)) + 
  geom_col(width = 1) +
  xlim(0.5, 2.5) + #this line creates the centre, remove to make a pie chart.
  geom_text(aes(label = paste0(gdon_2024Q3$Percentage, "%")),
            position = position_stack(vjust = 0.5), #adjusts text to be central to colour on chart.
            size = 5, fontface = "bold", color = "black") + #controls font, colour and size of text.
  coord_polar(theta="y") + #creates a pie_chart from bar chart.
  scale_fill_manual(values = colour_palette) + #uses colours from colour palette to colour the chart.
  theme_void() + #removes the degrees found when using a normal pie chart, making chart more readable.
  labs(title="NEETs by Gender 2024 Q3 (Jul-Sep)",
       caption = "Young people not in education, employment or training (NEET) Dataset")

#print the donut chart
print(gdon_chart1)

#setup a colour scheme for the stacked bar chart
colour_palette <- c("red", "cyan")

attach(merged_data) #attaches the dataset 
gbar01_24 <- data.frame(
  Gender = factor(rep(c("Male","Female"), times = 3)), #repeats factors 3 times
  Quarter = factor(c("2024 Q1", "2024 Q2", "2024 Q3")),
  Value = c(men_total[90], women_total[90], men_total[91], women_total[91], men_total[92],
            women_total[92])
  )

#Detaches the merged_data dataframe to allow for another dataframe to be attached 
detach(merged_data)

attach(gbar01_24)

#2024 Quarter as stacked bar chart with labels in bold
gbar_chart1 <- ggplot(gbar01_24, aes(x = Quarter, y = Value, fill = Gender, order = FALSE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Value)),
            position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold") +
  scale_fill_manual(values = colour_palette) +
  labs(title = "NEETs by Gender and Quarter 2024",
       caption = "Young people not in education, employment or training (NEET) Dataset",
       x = "2024 Quarter",
       y = "NEETs (in thousands)")

detach(gbar01_24)

#prints NEETs by Gender and Quarter 2024 Bar Chart
print(gbar_chart1)

#Summarises the dataset using descriptive statistics
summary(merged_data)

#create the lten_years dataframe
lten_years <- data.frame(
  Quarter = merged_data$seasonal_periods[50:92],
  Neet_Rate = merged_data$total_total[50:92],
  men_total = merged_data$men_total[50:92],
  women_total = merged_data$women_total[50:92]
)
#attaches the dataframe to make referring to the variables simpler
attach(lten_years)

#creates a line chart for Total Neet Rate progression between 2014-2024
neetline_graph <- ggplot(lten_years, aes(x = Quarter, y = Neet_Rate, group=1)) +
  geom_line(size = 1) + #Changes the size of the line to be thicker
  theme_minimal() +
  scale_x_discrete(breaks = Quarter[seq(1, length(Quarter), by = 4)],
                   limits = unique(Quarter)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Neet Rate Progression 2014-2024 (Quarterly)",
       caption = "Young people not in education, employment or training (NEET) Dataset",
       x = "Quarter", y = "Total NEETs (in thousands)")

#prints Total Neet Rate Progression 2014-2024 (Quarterly) Line Chart
print(neetline_graph)

#creates the gender line graph, where males are blue and females are gold
genline_graph <- ggplot(lten_years, aes(x = Quarter, group=1)) +
  theme_minimal() +
  geom_line(aes(y = men_total, col = "Male")) +
  geom_line(aes(y = women_total, col = "Female")) +
  scale_colour_manual(name = "Legend",
                      values = c("Male"="blue",
                                 "Female" = "gold")) +
  scale_x_discrete(breaks = Quarter[seq(1, length(Quarter), by = 4)],
                   limits = unique(Quarter)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Quarter", y="Total NEETs (in thousands)",
       title="NEET Progression by Gender 2014-2024 (Quarterly)",
       caption="Young people not in education, employment or training (NEET) Dataset")

#prints the NEET Progression by Gender 2014-2024 (Quarterly) line graph
print(genline_graph)

