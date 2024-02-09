#loading the libraries:

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(ggplot2)

#importing/reading all the files:

######################## data related to Activity #######################

activityDaily <- read.csv("./Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv", stringsAsFactors = F)

####################### data related to MET #############################

#MET <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv", stringsAsFactors = F)

######################## data related to Calories #######################

#calorieDaily <- read.csv("./Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv", stringsAsFactors = F)
calorieHourly <- read.csv("./Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv", stringsAsFactors = F)
#calorieMinute_narrow <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv", stringsAsFactors = F)
#calorieMinute_wide <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteCaloriesWide_merged.csv", stringsAsFactors = F)

######################## data related to Intensity ######################

#intensityDaily <- read.csv("./Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv", stringsAsFactors = F)
intensityHourly <- read.csv("./Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv", stringsAsFactors = F)
#intensityMinute_narrow <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv", stringsAsFactors = F)
#intensityMinute_wide <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteIntensitiesWide_merged.csv", stringsAsFactors = F)

######################## data related to Steps ##########################

#stepsDaily <- read.csv("./Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv", stringsAsFactors = F)
stepsHourly <- read.csv("./Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv", stringsAsFactors = F)
#stepsMinute_narrow <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv", stringsAsFactors = F)
#stepsMinute_wide <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteIntensitiesWide_merged.csv", stringsAsFactors = F)

######################## data related to Sleep ##########################

sleepDaily <- read.csv("./Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv", stringsAsFactors = F)
#sleepMinute <- read.csv("./Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv", stringsAsFactors = F)

######################## data related to Heartrate #######################

#heartrate <- read.csv("./Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv", stringsAsFactors = F)

######################## data related to Weight ##########################

#weightLog <- read.csv("./Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv", stringsAsFactors = F)


#determining if the sample size provided is enough for the analysis or not

n_distinct(activityDaily$Id)
n_distinct(sleepDaily$Id)
n_distinct(intensityHourly$Id)
n_distinct(calorieHourly$Id)
n_distinct(stepsHourly$Id)

#n_distinct(MET$Id)
#n_distinct(calorieDaily$Id)
#n_distinct(calorieMinute_narrow$Id)
#n_distinct(calorieMinute_wide$Id)
#n_distinct(intensityDaily$Id)
#n_distinct(intensityMinute_narrow$Id)
#n_distinct(intensityMinute_wide$Id)
#n_distinct(stepsDaily$Id)
#n_distinct(stepsMinute_narrow$Id)
#n_distinct(stepsMinute_wide$Id)
#n_distinct(sleepMinute$Id)
#n_distinct(heartrate$Id)
#n_distinct(weightLog$Id)

glimpse(activityDaily)
glimpse(sleepDaily)
glimpse(intensityHourly)
glimpse(calorieHourly)
glimpse(stepsHourly)

#glimpse(MET)
#glimpse(calorieDaily)
#glimpse(calorieMinute_narrow)
#glimpse(calorieMinute_wide)
#glimpse(intensityDaily)
#glimpse(intensityMinute_narrow)
#glimpse(intensityMinute_wide)
#glimpse(stepsDaily)
#glimpse(stepsMinute_narrow)
#glimpse(stepsMinute_wide)
#glimpse(sleepMinute)
#glimpse(heartrate)
#glimpse(weightLog)



#checking the existing NA values:

sum(is.na(activityDaily))
sum(is.na(sleepDaily))
sum(is.na(intensityHourly))
sum(is.na(calorieHourly))
sum(is.na(stepsHourly))

#checking the empty cells :

sum(apply(activityDaily, MARGIN=c(1,2), function(x) (x=="")))
sum(apply(sleepDaily, MARGIN=c(1,2), function(x) (x=="")))
sum(apply(intensityHourly, MARGIN=c(1,2), function(x) (x=="")))
sum(apply(calorieHourly, MARGIN=c(1,2), function(x) (x=="")))
sum(apply(stepsHourly, MARGIN=c(1,2), function(x) (x=="")))

#checking the duplicated values :

sum(duplicated(activityDaily))
sum(duplicated(sleepDaily))
sum(duplicated(intensityHourly))
sum(duplicated(calorieHourly))
sum(duplicated(stepsHourly))

#removing the duplicated values from sleepDaily:

sleepDaily <- sleepDaily %>% 
  distinct() %>% 
  drop_na()
sum(duplicated(sleepDaily))  

#modifying sleep data and activity data by sorting Date and Time columns and adding Weekdays:

activityDaily$TotalActiveMinutes <- activityDaily$VeryActiveMinutes+activityDaily$FairlyActiveMinutes+activityDaily$LightlyActiveMinutes
activityDaily$Date <- mdy(activityDaily$ActivityDate)
activityDaily$Weekday <- weekdays(activityDaily$Date)
activityDaily <- activityDaily %>% 
  select(Id,Date,Weekday,TotalSteps,TotalDistance,Calories,SedentaryMinutes,TotalActiveMinutes,VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes,VeryActiveDistance,ModeratelyActiveDistance,LightActiveDistance,SedentaryActiveDistance,TrackerDistance)
head(activityDaily)

sleepDaily$SleepDay <- sub("0:00$", "12:00:00 AM", sleepDaily$SleepDay)
sleepDaily$SleepDay_parsed <- mdy_hms(sleepDaily$SleepDay)
sleepDaily$Date <- as.Date(sleepDaily$SleepDay_parsed)
sleepDaily$Time <- format(sleepDaily$SleepDay_parsed, format = "%H:%M:%S")
sleepDaily$Weekday <- weekdays(sleepDaily$Date)
head(sleepDaily)

#merging sleep data and activity data:

dailyData <- activityDaily %>% 
  select(Id,Date,Weekday,TotalSteps,Calories,SedentaryMinutes,TotalActiveMinutes)

sleep_activity_merged<-merge(sleepDaily, dailyData, by=c('Id','Date','Weekday'))

SleepActivity_merged <- sleep_activity_merged %>% 
  select(Id,Date,Weekday,Time,TotalMinutesAsleep,TotalTimeInBed,TotalSteps,Calories,SedentaryMinutes,TotalActiveMinutes)
head(SleepActivity_merged)

#merging different hourly files into one

Hourly_data <- data.frame(calorieHourly$Id,calorieHourly$ActivityHour,calorieHourly$Calories,intensityHourly$TotalIntensity,stepsHourly$StepTotal)
colnames(Hourly_data) <- c("Id","ActivityHour","CaloriesBurnt","TotalIntensity","TotalSteps")

Hourly_data$ActivityHour_parsed <- mdy_hms(Hourly_data$ActivityHour)
Hourly_data$Date <- as.Date(Hourly_data$ActivityHour_parsed)
Hourly_data$Time <- format(Hourly_data$ActivityHour_parsed, format = "%H:%M:%S")

hourlyData <- Hourly_data %>% 
  select(Id,Date,Time,CaloriesBurnt,TotalIntensity,TotalSteps)

#important data we are going to work with:

head(activityDaily)
head(sleepDaily)
head(intensityHourly)
head(calorieHourly)
head(stepsHourly)
head(dailyData)
head(hourlyData)
head(SleepActivity_merged)



#summary stats about calories:
activityDaily %>% 
  select(Calories) %>% 
  summary()

#histogram showing skewness
totalCalories <- activityDaily %>% 
  select(Id,Date,Calories)

totalCalories_histogram <- ggplot(totalCalories, aes(x=Calories))+
  geom_histogram(binwidth=50, color="pink",fill="pink",alpha = 0.5)+
  theme_light()+
  ggtitle("Calorie Intake Spectrum in the Sample")+
  labs(x="Calorie Intake(kcal)", y="Number of Participants")

totalCalories_histogram


#summary stats about total steps 
activityDaily %>% 
  select(TotalSteps) %>% 
  summary()

#histogram showing skewness
totalSteps <- activityDaily %>% 
  select(Id,Date,TotalSteps)

totalSteps_histogram <- ggplot(totalSteps, aes(x=TotalSteps))+
  geom_histogram(binwidth=500, color="pink",fill="pink",alpha = 0.5)+
  theme_light()+
  ggtitle("Variation in Step Counts Across the Population")+
  labs(x="Total Steps", y="Number of Participants")

totalSteps_histogram

# 
# #### **Combined Daily Steps:** *Initial insights from the summary statistics and the histogram*
# > *  Skewness : Positive or Right Skew.
# > *  Average Steps: On average, individuals take approximately 7638 steps per day, providing a central measure for the distribution.
# > *  Dominant Step Range: The majority of people fall within the range of 1000 to 15000 steps per day, indicating a concentration of typical daily physical activity.
# > *  Noteworthy Outliers: Notable are outliers above 20,000 steps, signifying a subset of individuals who exhibit exceptionally high levels of daily physical activity.
# > *  Prolonged Sedentary Behavior: A considerable number of people are characterized by extended periods of sitting, suggesting a sedentary lifestyle for a significant portion of the population.



#summary stats about total distance covered 
activityDaily %>% 
  select(TotalDistance) %>% 
  summary()

#histogram showing skewness
totalDistance <- activityDaily %>% 
  select(Id,Date,TotalDistance)

totalDistance_histogram <- ggplot(totalDistance, aes(x=TotalDistance))+
  geom_histogram(binwidth=1, color="pink",fill="pink",alpha = 0.5)+
  ggtitle("Total Distance Traveled by Individuals")+
  labs(x="Total Distance(kms)", y="Number of Participants")+
  theme_light()

totalDistance_histogram

sum(activityDaily$TotalDistance)
sum(activityDaily$TrackerDistance)

activityDaily %>% 
  select(TotalDistance,TrackerDistance) %>% 
  summary()


#summary stats about minutes:

activityDaily %>% 
  select(VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes,TotalActiveMinutes,SedentaryMinutes) %>% 
  summary()


#supporting histograms:

#VeryActiveMinutes:Sample breakdown
VeryActiveMinutes <- activityDaily %>% 
  select(Id,Date,VeryActiveMinutes)

VeryActiveMinutes_histogram <- ggplot(VeryActiveMinutes, aes(x=VeryActiveMinutes))+
  geom_histogram(binwidth=4, color="lightblue",fill="lightblue",alpha = 0.5)+
  ggtitle("Exploring Very Active Time by Participant Count")+
  labs(x="Very Active Time(minutes)", y="Number of Participants")+
  theme_light()

#SedentaryMinutes:Sample breakdown
SedentaryMinutes <- activityDaily %>% 
  select(Id,Date,SedentaryMinutes)

SedentaryMinutes_histogram <- ggplot(SedentaryMinutes, aes(x=SedentaryMinutes))+
  geom_histogram(binwidth=10, color="lightblue",fill="lightblue",alpha = 0.5)+
  ggtitle("Exploring Sedentary Time by Participant Count")+
  labs(x="Sedentary Time(minutes)", y="Number of Participants")+
  theme_light()

#ActiveMinutes:Sample breakdown
ActiveMinutes <- activityDaily %>%
  select(Id,Date,TotalActiveMinutes)

ActiveMinutes_histogram <- ggplot(ActiveMinutes, aes(x=TotalActiveMinutes))+
  geom_histogram(binwidth=10, color="lightblue",fill="lightblue",alpha = 0.5)+
  ggtitle("Exploring Total Active Time by Participant Count")+
  labs(x="Total Active Time(minutes)", y="Number of Participants")+
  theme_light()

VeryActiveMinutes_histogram
SedentaryMinutes_histogram
ActiveMinutes_histogram

#Pie chart
Very_Active <- sum(activityDaily$VeryActiveMinutes)
Fairly_Active <- sum(activityDaily$FairlyActiveMinutes)
Lightly_Active <- sum(activityDaily$LightlyActiveMinutes)
Sedentary <- sum(activityDaily$SedentaryMinutes)
Total <- Very_Active+Fairly_Active+Lightly_Active+Sedentary

Physical_State_of_Individuals <- c("Very Active","Fairly Active","Lightly Active","Sedentary")
Activeness <- c(Very_Active,Fairly_Active,Lightly_Active,Sedentary)
PercentageDistribution <- Activeness/Total*100

Activeness_data <- data.frame(Physical_State_of_Individuals,Activeness,PercentageDistribution)

custom_colors <- c("#a6d854", "#fc8d62", "#66c2a5", "#8da0cb" )
ggplot(Activeness_data, aes(x = "", y = PercentageDistribution, fill = Physical_State_of_Individuals)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  ggtitle("Distribution of Activity Levels Throughout the Day") +
  theme_void()+
  scale_fill_manual(values = custom_colors)+
  theme(legend.position = "left")
View(Activeness_data)



#summary stats about sleep
sleepDaily %>% 
  select(TotalMinutesAsleep,TotalTimeInBed) %>% 
  summary()

#supporting density graphs
sleep_data <- sleepDaily %>% 
  select(Id,Date,TotalMinutesAsleep,TotalTimeInBed) 

ggplot(sleep_data, aes(x = TotalMinutesAsleep)) +
  geom_density(color="blue")+
  labs(x="Total time asleep (minutes)", y="Number of Participants", title="Total Time asleep vs. Participant Count")+
  theme_light()

ggplot(sleep_data, aes(x = TotalTimeInBed)) +
  geom_density(color="red")+
  labs(x="Total time in bed (minutes)", y="Number of Participants", title="Total Time in bed vs. Participant Count")+
  theme_light()


TotalTime_InBed <- sum(sleepDaily$TotalTimeInBed)
TotalTime_Asleep <- sum(sleepDaily$TotalMinutesAsleep)
total <- TotalTime_InBed+TotalTime_Asleep
Sleep_Profile <- c("Total Time In Bed","Total Time Asleep")
Sleep <- c(TotalTime_InBed,TotalTime_Asleep)
percentageDist <- Sleep/total*100
Sleep_data <- data.frame(Sleep_Profile,Sleep,percentageDist)

View(Sleep_data)


intensityHourly %>% 
  select(TotalIntensity) %>% 
  summary()


ggplot(hourlyData, aes(x=CaloriesBurnt,y=TotalIntensity))+
  geom_point(color="#FFB6C1",alpha=0.4, size = 3)+
  theme_light()+
  labs(x = "Calories Burnt",y = "Intensity of the Activity",title = "Relationship Between Calories Burnt and Intensity")+ 
  geom_smooth(method="lm",color = "black")

ggplot(hourlyData, aes(x=CaloriesBurnt,y=TotalIntensity))+
  geom_point(color="#FFB6C1",alpha=0.4, size = 3)+
  theme_light()+
  labs(x = "Calories Burnt",y = "Intensity of the Activity",title = "Relationship Between Calories Burnt and Intensity")+ 
  geom_smooth(color = "black")


ggplot(hourlyData, aes(x=TotalIntensity,y=TotalSteps))+
  geom_point(color="#ADD8E6",alpha=0.4, size = 3)+
  theme_light()+
  labs(x = "Total Intensity",y = "Total Steps",title = "Relationship Between Total Intensity and Total Steps")+ 
  geom_smooth(method="lm",color = "black")

ggplot(hourlyData, aes(x=TotalIntensity,y=TotalSteps))+
  geom_point(color="#ADD8E6",alpha=0.4, size = 3)+
  theme_light()+
  labs(x = "Total Intensity",y = "Total Steps",title = "Relationship Between Total Intensity and Total Steps")+ 
  geom_smooth(color = "black")

ggplot(activityDaily, aes(x=Calories,y=TotalSteps))+
  geom_point(color="#E0BBE4",alpha=0.4, size = 3)+
  theme_light()+
  labs(x = "Total Calories Burnt Daily",y = "Total Steps Walked Everyday",title = "Relationship Between Total Calories and Total Steps: Daily")+ 
  geom_smooth(color = "black")

ggplot(sleep_data, aes(x=TotalMinutesAsleep,y=TotalTimeInBed))+
  geom_point(color="#FFD700",alpha=0.4, size = 3)+
  theme_minimal()+
  labs(x = "Total Time Asleep",y = "Total Time In Bed",title = "Relationship Between Total Minutes Asleep and Total Time In Bed")+ 
  geom_smooth(method="lm",color = "black")

ggplot(sleep_data, aes(x=TotalMinutesAsleep,y=TotalTimeInBed))+
  geom_point(color="#FFD700",alpha=0.4, size = 3)+
  theme_minimal()+
  labs(x = "Total Time Asleep",y = "Total Time In Bed",title = "Relationship Between Total Minutes Asleep and Total Time In Bed")+ 
  geom_smooth(color = "black")


ggplot(SleepActivity_merged, aes(x=TotalMinutesAsleep,y=Calories))+
  geom_point(color="#98FB98",alpha=0.4, size = 3)+
  theme_minimal()+
  labs(x = "Total Time Asleep",y = "Calories",title = "Relationship Between Total Minutes Asleep and Calories")+ 
  geom_smooth(color = "black")


ggplot(SleepActivity_merged, aes(x=TotalMinutesAsleep,y=SedentaryMinutes))+
  geom_point(color="magenta",alpha=0.2, size = 3)+
  theme_minimal()+
  labs(x = "Total Time Asleep",y = "Sedentary Minutes",title = "Relationship between Total Time Spent Sleeping & Time Spent in Sedentary state")+ 
  geom_smooth(method="lm",color = "black")


ggplot(SleepActivity_merged, aes(x=TotalMinutesAsleep,y=TotalActiveMinutes))+
  geom_point(color="purple",alpha=0.2, size = 3)+
  theme_minimal()+
  labs(x = "Total Minutes Asleep",y = "Active Minutes",title = "Relationship between Total Time Spent Sleeping & Time Spent in Active state")+ 
  geom_smooth(color = "black")


ggplot(hourlyData, aes(x=Time,y=CaloriesBurnt))+
  geom_bar(stat = "identity", fill = "#B2DFDB") +
  labs(x = "Hours", y = "Calories Burnt",title="Relationship between Calories burnt and Hour of the day") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(hourlyData, aes(x=Time,y=TotalIntensity))+
  geom_bar(stat = "identity", fill = "#C8A2C8") +
  labs(x = "Hours", y = "Intensity of the Activity",title="Relationship between Intensity of an activity and Hour of the day") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(hourlyData, aes(x=Time,y=TotalSteps))+
  geom_bar(stat = "identity", fill = "#FFB6C1") +
  labs(x = "Hours", y = "Total Steps",title="Relationship between Total Steps and Hour of the day") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(dailyData, aes(x=TotalActiveMinutes,y=TotalSteps))+
  geom_point(color="#FF7F50",alpha=0.4, size = 3)+
  theme_minimal()+
  labs(x = "Total Active Minutes",y = "Total Steps Walked Hourly",title = "Relationship Between Active Minutes and Total Steps")+ 
  geom_smooth(method="lm",color = "black")

ggplot(dailyData, aes(x=SedentaryMinutes,y=TotalSteps))+
  geom_point(color="#FF7F50",alpha=0.4, size = 3)+
  theme_minimal()+
  labs(x = "Sedentary Minutes",y = "Total Steps Walked Hourly",title = "Relationship Between Sedentary Minutes and Total Steps")+ 
  geom_smooth(method="lm",color = "black")


ggplot(dailyData, aes(x=Weekday,y=Calories))+
  geom_bar(stat = "identity", fill = "#E6E6FA") +
  labs(x = "Weekday", y = "Calories burnt",title="Relationship between Calories and Days of the week") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(dailyData, aes(x=Weekday,y=SedentaryMinutes))+
  geom_bar(stat = "identity", fill = "#ADD8E6") +
  labs(x = "Weekday", y = "Sedentary Minutes",title="Relationship between Sedentary Physical State and Days of the week") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(dailyData, aes(x=Weekday,y=TotalActiveMinutes))+
  geom_bar(stat = "identity", fill = "#E0BBE4") +
  labs(x = "Weekday", y = "Total Active Minutes",title="Relationship between Active Physical State and Days of the week") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(dailyData, aes(x=Weekday,y=TotalSteps))+
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Weekday", y = "Total Steps",title="Relationship between Total Steps and Days of the week") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(SleepActivity_merged, aes(x=Weekday,y=TotalMinutesAsleep))+
  geom_bar(stat = "identity", fill = "#FFA07A") +
  labs(x = "Weekday",y = "Total Minutes Asleep",title = "Relationship between Total Minutes Asleep and Weekdays") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(SleepActivity_merged, aes(x=Calories,y=SedentaryMinutes))+
  geom_point(color="brown",alpha=0.4, size = 3)+
  theme_minimal()+
  labs(x = "Calories Burnt",y = "Sedentary Minutes",title = "Relationship between Calories Burnt and Time Spend in Sedentary state")+
  geom_smooth(color = "black")