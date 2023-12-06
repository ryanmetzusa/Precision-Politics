library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
options(scipen = 999)
data <- read_excel("VoterFile.xlsx")

summary(data)

#Changing variables to factors/date format


data$Voters_CalculatedRegDate <- as.Date(data$Voters_CalculatedRegDate, format = "%A, %B %d, %Y")
data$Voters_OfficialRegDate <- as.Date(data$Voters_OfficialRegDate, format = "%A, %B %d, %Y")
data$FECDonors_LastDonationDate <- as.Date(data$FECDonors_LastDonationDate, format = "%A, %B %d, %Y")
char_columns <- sapply(data, is.character)
data[char_columns] <- lapply(data[char_columns], factor)

summary(data)


#EDA

unique(data$Parties_Description)



ggplot(data, aes(x = Parties_Description, y = Vote_Frequency)) +
  geom_boxplot() +
  labs(title = "Scatterplot of Parties_Description vs Vote_Frequency",
       x = "Parties_Description",
       y = "Vote_Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




ggplot(data, aes(x = Voters_OfficialRegDate, y = FECDonors_LastDonationDate
)) +
  geom_point() +
  labs(title = "Scatterplot of Voters_OfficialRegDate vs FECDonors_LastDonationDate",
       x = "Voters_OfficialRegDate",
       y = "FECDonors_LastDonationDate
")

ggplot(data, aes(x = Voters_OfficialRegDate)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Voters_OfficialRegDate",
       x = "Voters_OfficialRegDate",
       y = "Frequency")

ggplot(data, aes(x = Voters_OfficialRegDate)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ Parties_Description, scales = "free_x", ncol = 2) +
  labs(title = "Histogram of Voters_OfficialRegDate by Parties_Description",
       x = "Voters_OfficialRegDate",
       y = "Frequency")

ggplot(data, aes(x = Voters_OfficialRegDate)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ EthnicGroups_EthnicGroup1Desc
, scales = "free_x", ncol = 2) +
  labs(title = "Histogram of Voters_OfficialRegDate by EthnicGroups_EthnicGroup1Desc",
       x = "Voters_OfficialRegDate",
       y = "Frequency")

######################

filtered_data <- data %>%
  group_by(EthnicGroups_EthnicGroup1Desc) %>%
  filter(CommercialData_EstHomeValue >= quantile(CommercialData_EstHomeValue, 0.25, na.rm = TRUE) - 1.5 * IQR(CommercialData_EstHomeValue, na.rm = TRUE),
         CommercialData_EstHomeValue <= quantile(CommercialData_EstHomeValue, 0.75, na.rm = TRUE) + 1.5 * IQR(CommercialData_EstHomeValue, na.rm = TRUE))

# Get the y-axis limits without outliers
y_limits <- range(filtered_data$CommercialData_EstHomeValue, na.rm = TRUE)

# Create a boxplot with ggplot2 using the filtered data and manually set y-axis limits
ggplot(filtered_data, aes(x = EthnicGroups_EthnicGroup1Desc, y = CommercialData_EstHomeValue)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of EthnicGroups_EthnicGroup1Desc by CommercialData_EstHomeValue",
       x = "EthnicGroups_EthnicGroup1Desc",
       y = "CommercialData_EstHomeValue") +
  ylim(y_limits)
###############################
ggplot(data, aes(x = factor(Vote_Frequency), fill = Parties_Description)) +
  geom_bar(position = 'fill', stat = 'count') +
  labs(title = 'Party Affiliate Voter Frequency',
       x = 'Vote Frequency',
       y = 'Share of Voting Pool',
       fill = 'Party Affiliation') +  # This line changes the legend title
  theme_minimal()

###########################

data$Year <- format(data$Voters_OfficialRegDate, "%Y")

# Create a bar chart
# Assuming you have a dataframe named 'data'

# Sample Dataframe
# Load the required libraries
library(ggplot2)

# Convert Voters_OfficialRegDate to a Date object
data$Voters_OfficialRegDate <- as.Date(data$Voters_OfficialRegDate)

# Extract the year from Voters_OfficialRegDate
data$Year <- format(data$Voters_OfficialRegDate, "%Y")

# Create separate charts for each level of Vote_Frequency with fill color representing Parties_Description
breaks <- seq(min(data$Year), max(data$Year), by = 5)
labels <- ifelse(as.numeric(as.character(breaks)) %% 5 == 0, as.character(breaks), '')

# Create separate charts for each level of Vote_Frequency with fill color representing Parties_Description
ggplot(data, aes(x = Year, fill = Parties_Description)) +
  geom_bar(position = 'fill', stat = 'count') +
  labs(title = 'Party Affiliation Voter Frequency Breakdown',
       x = 'Year',
       y = 'Party Affiliation Share percentage (normalized)',
       fill = 'Party Affiliation') +
  facet_grid(Vote_Frequency ~ ., scales = 'free_y', space = 'free_y', switch = 'y', drop = TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),  # Remove x-axis ticks for better readability
        strip.text = element_text(size = 9),
        axis.title.y.right = element_text(margin = margin(t = 100, r = 15))) +  # Adjust right Y-axis position
  scale_x_discrete(breaks = breaks, labels = labels) +  # Set custom x-axis breaks and labels
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Vote Frequency")) +  # Secondary Y-axis
  guides(fill = guide_legend(title = 'Party Affiliation'),  # Customize legend title
         fill = guide_legend(order = 2))  # Set legend order for better placement

########


data95 <- data %>%
  filter(data$Year > 1995)


democratic_summary <- data95 %>%
  filter(Parties_Description == 'Democratic') %>%
  group_by(Vote_Frequency) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Print or visualize the results
print(democratic_summary)

democratic_total_count <- data95 %>%
  filter(Parties_Description == 'Democratic') %>%
  summarise(total_count = n())

# Print or visualize the results
print(democratic_total_count)


ggplot(data95, aes(x = Year, y = after_stat(count/sum(count)), group = Parties_Description, color = Parties_Description)) +
  geom_line(stat = 'count') +
  labs(title = 'Party Affiliation Voter Frequency Breakdown (Years after 1995)',
       x = 'Registered Year',
       y = 'Party Affiliation Share % (normalized)',
       color = 'Party Affiliation') +
  facet_grid(Vote_Frequency ~ ., scales = 'free_y', space = 'free_y', switch = 'y', drop = TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),  # Remove x-axis ticks for better readability
        strip.text = element_text(size = 9),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 15))) +  # Adjust right Y-axis position
  scale_x_discrete(breaks = breaks, labels = labels) +  # Set custom x-axis breaks and labels
  scale_y_continuous(sec.axis = sec_axis(~ . * sum(data$Vote_Frequency), name = "Vote Frequency")) +  # Secondary Y-axis
  guides(color = guide_legend(title = 'Party Affiliation'),  # Customize legend title
         color = guide_legend(order = 2))  # Set legend order for better placement


####

dataD <- data %>%
  mutate(Donates_Y = ifelse(CommercialData_DonatesEnvironmentCauseInHome == 'Y' | CommercialData_DonatesToCharity == 'Y', 'Donates', 'Does not Donate'))


donation_summary <- dataD %>%
  filter(Parties_Description == 'Democratic' & !is.na(Donates_Y)) %>%
  group_by(Donates_Y) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count, na.rm = TRUE))

# Print or visualize the results
print(donation_summary)
# Create a bar chart for the count of 'Y' occurrences
ggplot(dataD, aes(x = Parties_Description, fill = factor(Donates_Y))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = 'Donation Comparison by Party',
       x = 'Political Party',
       y = 'Number of Voters',
       fill = 'Donation or Not') +
  theme_minimal()

####

count_data <- data %>%
  group_by(Parties_Description, CommercialData_PresenceOfChildrenCode) %>%
  summarise(count = n())


ggplot(count_data, aes(x = Parties_Description, fill = CommercialData_PresenceOfChildrenCode, y = count)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = 'Presence of Children by Party',
       x = 'Parties Description',
       y = 'Count',
       fill = 'Presence of Children') +
  theme_minimal()


#####

filtered_dataC <- data %>%
  filter(CommercialData_DonatesEnvironmentCauseInHome == 'Y' | CommercialData_DonatesToCharity == 'Y')


count_dataC <- filtered_dataC %>%
  group_by(Parties_Description, CommercialData_PresenceOfChildrenCode) %>%
  summarise(count = n())

# Create a bar chart
ggplot(count_dataC, aes(x = Parties_Description, fill = CommercialData_PresenceOfChildrenCode, y = count)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = 'Presence of Children by Party (filtered by donations)',
       x = 'Parties Description',
       y = 'Count',
       fill = 'Presence of Children') +
  theme_minimal()


###########
income_count_data <- filtered_data %>%
  group_by(Parties_Description, CommercialData_EstimatedIncome) %>%
  summarise(count = n())

income_count_data1 <- filtered_data %>%
  filter(Parties_Description %in% c("Democratic", "Registered Independent")) %>%
  group_by(Parties_Description, CommercialData_Education) %>%
  summarise(count = n()) %>%
  group_by(Parties_Description) %>%
  summarise(total_count = sum(count))




filtered_dataE <- data %>%
  filter(!is.na(CommercialData_Education),
         CommercialData_DonatesEnvironmentCauseInHome == 'Y' | CommercialData_DonatesToCharity == 'Y')

# Create a bar chart for CommercialData_Education
education_count_data <- filtered_dataE %>%
  group_by(Parties_Description, CommercialData_Education) %>%
  summarise(count = n())

education_count_data1 <- filtered_dataE %>%
  filter(Parties_Description %in% c("Democratic", "Registered Independent")) %>%
  group_by(Parties_Description, CommercialData_Education) %>%
  summarise(count = n()) %>%
  group_by(Parties_Description) %>%
  summarise(total_count = sum(count))

educationprop <- prop.table(table(education_count_data1$Parties_Description, education_count_data1$total_count), margin = 1)
print(educationprop)

ggplot(income_count_data, aes(x = Parties_Description, fill = CommercialData_EstimatedIncome, y = count)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = 'Estimated Income by Party',
       x = 'Political Party',
       y = 'Number of Voters',
       fill = 'Estimated Income') +
  theme_minimal()

# Plot for CommercialData_Education
ggplot(education_count_data, aes(x = Parties_Description, fill = CommercialData_Education, y = count)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = 'Education Level by Party',
       x = 'Political Party',
       y = 'Number of Voters',
       fill = 'Education Level') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

resultE <- filtered_dataE %>%
  group_by(Vote_Frequency, CommercialData_Education) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

# Print the result
print(resultE)

#### 
filtered_dataIHV <- data %>%
  filter(Parties_Description %in% c("Registered Independent", "Democratic"))

# Create a scatter plot
ggplot(filtered_dataIHV, aes(x = CommercialData_EstimatedMedianIncome, y = CommercialData_EstHomeValue, color = Parties_Description)) +
  geom_point() +
  labs(title = 'Scatter Plot of Estimated Median Income vs. Estimated Home Value',
       x = 'Estimated Median Income',
       y = 'Estimated Home Value',
       color = 'Parties Description') +
  theme_minimal()


#####

filtered_dataDR <- data %>%
  filter(!is.na(FECDonors_TotalDonationsAmt_Range),Parties_Description %in% c("Democratic"))

# Create a scatter plot grouped by Parties_Description
ggplot(filtered_dataDR, aes(x = FECDonors_LastDonationDate, y = Voters_OfficialRegDate, color = Parties_Description, size = FECDonors_TotalDonationsAmt_Range)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2, height = 0.2)) +  # Adjust alpha and jitter
  scale_size_discrete(name = 'Total Donations Amount Range') +  # Use scale_size_discrete
  labs(title = 'Bubble Chart of Last Donation Date vs. Official Registration Date (Grouped by Parties)',
       x = 'Last Donation Date',
       y = 'Official Registration Date',
       color = 'Parties Description') +
  theme_minimal()


ggplot(filtered_dataDR, aes(x = FECDonors_LastDonationDate, y = Voters_OfficialRegDate, fill = Parties_Description)) +
  geom_density_2d(alpha = 0.5) +  # Adjust alpha for transparency
  labs(title = 'Density Curve of Last Donation Date vs. Official Registration Date (Grouped by Parties)',
       x = 'Last Donation Date',
       y = 'Official Registration Date',
       fill = 'Parties Description') +
  theme_minimal()

ggplot(filtered_dataDR, aes(x = FECDonors_TotalDonationsAmt_Range, fill = Parties_Description)) +
  geom_density(alpha = 0.5) +  # Adjust alpha for transparency
  labs(title = 'Density Curve of Last Donation Date (Grouped by Parties)',
       x = 'Last Donation Date',
       fill = 'Parties Description') +
  theme_minimal()


#####

filtered_dataG <- data[data$Parties_Description == "Democratic", ]
filtered_dataG$Vote_Frequency <- factor(filtered_dataG$Vote_Frequency)

# Create a bar chart
ggplot(filtered_dataG, aes(x = Mailing_HHGender_Description, fill = Vote_Frequency)) +
  geom_bar(position = "dodge") +
  labs(title = 'Gender Distribution of Voters',
       x = 'Household Gender Description',
       y = 'Number of Voters',
       fill = 'Vote Frequency') +
  theme_minimal()



ggplot(filtered_dataG, aes(x = Mailing_HHGender_Description, fill = Vote_Frequency)) +
  geom_bar(position = 'dodge') +
  facet_grid(~Vote_Frequency, scales = 'free_x', switch = 'x') +
  labs(title = 'Gender Distribution of Voters for Democrats',
       x = 'Household Gender Description',
       y = 'Number of Voters',
       fill = 'Voting Participation') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


resultG <- filtered_dataG %>%
  group_by(Vote_Frequency, Mailing_HHGender_Description) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

resultG1 <- filtered_dataG %>%
  filter(!is.na(Vote_Frequency)) %>%
  group_by(Vote_Frequency) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Print the result
print(resultG)

print(resultG1)


democratic_total_count <- data95 %>%
  filter(Parties_Description == 'Democratic') %>%
  summarise(total_count = n())

# Print or visualize the results
print(democratic_total_count)

resultG <- filtered_dataG %>%
  filter(Parties_Description == 'Democratic') %>%
  summarise(count = n())

print (resultG)


 