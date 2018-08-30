# METHODOLOGY FOR THIS PROJECT

# 1. Defining the goal of the project
# 2. Preparing the data and data exploration
# 3. Training and testing the model 
# 4. Evaluating the performance of the model
# 5. Documentation and presentation

# GOAL OF THE PROJECT

# Our goal is to understand patterns and conditions in order to predict possible client's status in the next visit

# Some of the Insights and questions to look at are:

# 1. How long is PHQ administration and Recieving Time in days, how was it in relation to the condition of the clients
# 2. what is the session number distribution, is it decremental from session1 to session 12 as expected or we have left some clients of time
# 3. which day(s) are our field officers on average busy in the field, which day(s) can we utilize them well at the office
# 4. How is the distribution of; No_pleasure_doing_anything,Feeling_sad__depressed_or_hopeless,Trouble_sleeping,Feeling_tired_or_little_energy, Poor_appetite_or_over_eating, self_guilt, Trouble_concentrating, Moving_or_speaking_slowly, Sucide_Thoughts
# 5. How is our response variable distributed 
# 6. Much other insights could further be got but that is it for  now

# Loading Packages to use in the project
library(dplyr)
library(ggplot2)
library(magrittr)
library(RColorBrewer)
library(flipTime) # To install it first install devtools, or use library(devtools) if you already installed it
# run install_github("Displayr/flipTime")


# Loading the data
phq9_data <- read.csv("data/PHQ_9 FORM.csv")

# knowning what the data contains
str(phq9_data)
head(phq9_data, 3)
tail(phq9_data, 3)

# starting with the difference in days

# First lets remove the timestamp from the date in Recieved_At column
# Just removed time zone using grab sub string method, had no better way of doing it
phq9_data$Received_At <- gsub(x=phq9_data$Received_At,pattern=" +00:00",replacement="",fixed=T)

# Now lets rmove the time
# lets first convert our two dates columns to Date since they are factors as seen from str()
phq9_data$Received_At <-AsDateTime(phq9_data$Received_At) # using our flipTime package
phq9_data$PHQ_9_Administration <-AsDateTime(phq9_data$PHQ_9_Administration)

# Since all our time formats are in POSIXct, lets use as.Date trick to get only date for Recieved at and transform PHQ_Administration to Date
phq9_data$Received_At <-as.Date(phq9_data$Received_At)
phq9_data$PHQ_9_Administration <-as.Date(phq9_data$PHQ_9_Administration)

# IF we just tranformed it directly minus changing to POSIXct it would return NA

# Getting days between PHQ_Administration and Recieved_at
phq9_data$days_in_between <- phq9_data$Received_At- phq9_data$PHQ_9_Administration
head(phq9_data, 10)

# Plot of How long is PHQ administration and Recieving Time in days

days_unsorted <- phq9_data %>% 
  group_by(days_in_between) %>%
  summarise(Total = n())

days_between <- transform(days_unsorted, 
                          days_in_between  = reorder(days_in_between, Total))

colourCount = length(unique(days_between$days_in_between))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = days_between, aes(x = days_in_between, y = Total,
                                fill=getPalette(colourCount),
                                label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Days in between PHQ Administration and Recieving"))+
  labs(x = "Days In Between", y = "Number Times it Happened")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold",color="#5D5055", 
                                   size=5, angle=0),
        axis.text.y = element_text(face="bold", color="#5D5055", 
                                   size=10, angle=0))+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# Looks like there some outliers in the data, we can't have negative days between PHQ_Administration and Recieving_at
# Lets drop those rows where the difference between Recieving and PHQ_Administration is less than 0
phq9_data <- phq9_data[with(phq9_data,days_in_between>=0),]

#There is a possibility of Duplicated rows also, lets remove them
phq9_data <- unique(phq9_data[ , ]) # Because of confidentiality, some helful columns were removed

# Re-Visualization of the above will now not contain negatives 

# Visualization of the sessions
sessions <- phq9_data %>%
  group_by(Session_Number) %>%
  summarise(Total = n())

new_sessions <- transform(sessions, 
                          Session_Number = reorder(Session_Number, Total)) 

colourCount = length(unique(new_sessions$Total))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(data = new_sessions, aes(x = Session_Number, y = Total,
                                fill=getPalette(colourCount),
                                label=Total))+
  geom_bar(stat = "identity"
  )+
  geom_text(hjust = 0.8, vjust = 0,color = "black", 
            fontface = "bold",size = 2)+
  coord_flip()+
  ggtitle(paste("Distribution of Sessions"))+
  labs(x = "Session Name", y = "Number of Times of that Session")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", color="#5D5055", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="bold", color="#5D5055", 
                                   size=10, angle=0))+
  theme(axis.text.x = element_text(angle=75, hjust=1))+
  theme(axis.text.y = element_text(angle=45, hjust=1))

# This is interesting and it ties with our hyposesis

# Meeting Day Distribution
meeting_day <- phq9_data %>%
  group_by(Meeting_Day) %>%
  summarise(Total = n())

new_meeting_day <- transform(meeting_day, 
                             Meeting_Day = reorder(Meeting_Day, Total)) 

colourCount = length(unique(new_meeting_day$Total))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(data = new_meeting_day, aes(x = Meeting_Day, y = Total,
                                   fill=getPalette(colourCount),
                                   label=Total))+
  geom_bar(stat = "identity"
  )+
  geom_text(hjust = 0.8, vjust = 0,color = "black", 
            fontface = "bold",size = 2)+
  coord_flip()+
  ggtitle(paste("Distribution of Meeting Days By Field Officers"))+
  labs(x = "Meeting Day Name", y = "Number of Meetings on that WeekDay")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", color="#5D5055", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="bold", color="#5D5055", 
                                   size=10, angle=0))+
  theme(axis.text.x = element_text(angle=75, hjust=1))+
  theme(axis.text.y = element_text(angle=45, hjust=1))

# Friday is the day most of the field officers arent so much heading to the field, they can be utilized well. 
# No big event which requires all staff should be placed on Wednesday because most of the field officers are not in the office

# No_pleasure_doing_anything Distribution
no_pleasure_unsorted <- phq9_data %>% 
  group_by(No_pleasure_doing_anything) %>%
  summarise(Total = n())

new_no_pleasure <- transform(no_pleasure_unsorted, 
                             No_pleasure_doing_anything  = reorder(No_pleasure_doing_anything, Total))

colourCount = length(unique(new_no_pleasure$No_pleasure_doing_anything))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_no_pleasure, aes(x = No_pleasure_doing_anything, y = Total,
                                   fill=getPalette(colourCount),
                                   label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution of No Pleasure Doing Things"))+
  labs(x = "Degree of Feeling", y = "Number Times this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# There is really a slight differnce between (0,1 and 2). its 3 which is below 2000

# Feeling_sad__depressed_or_hopeless Distribution
depression_unsorted <- phq9_data %>% 
  group_by(Feeling_sad__depressed_or_hopeless) %>%
  summarise(Total = n())

new_depression <- transform(depression_unsorted, 
                            Feeling_sad__depressed_or_hopeless  = reorder(Feeling_sad__depressed_or_hopeless, Total))

colourCount = length(unique(new_depression$Feeling_sad__depressed_or_hopeless))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_depression, aes(x = Feeling_sad__depressed_or_hopeless, y = Total,
                                  fill=getPalette(colourCount),
                                  label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Depression"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# The pattern is almost like No_pleasure_doing_anything only that response on is greater than 2 and also there is a slight difference in (0,1 and 2) as compared to No_pleasure_doing_anything
# Also 3 is greater than 2000 responses, might be a serious issue to look deeper

# Trouble_sleeping Distribution
sleep_unsorted <- phq9_data %>% 
  group_by(Trouble_sleeping) %>%
  summarise(Total = n())

new_sleep <- transform(sleep_unsorted, 
                       Trouble_sleeping  = reorder(Trouble_sleeping, Total))

colourCount = length(unique(Trouble_sleeping))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_sleep, aes(x = Trouble_sleeping, y = Total,
                             fill=getPalette(colourCount),
                             label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Trouble Sleeping"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# Seems like Trouble_sleeping is not a very big issue

# Feeling_tired_or_little_energy Distribution
tired_unsorted <- phq9_data %>% 
  group_by(Feeling_tired_or_little_energy) %>%
  summarise(Total = n())

new_tired <- transform(tired_unsorted, 
                       Feeling_tired_or_little_energy  = reorder(Feeling_tired_or_little_energy, Total))

colourCount = length(unique(Feeling_tired_or_little_energy))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_tired, aes(x = Feeling_tired_or_little_energy, y = Total,
                             fill=getPalette(colourCount),
                             label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Feeling tired or little energy"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# 3 is moreless 1000
# Pretty not a scaring factor, looks like a lowest risk with our clients but will confirm it 

# Poor_appetite_or_over_eating Distribution
eating_issues_unsorted <- phq9_data %>% 
  group_by(Poor_appetite_or_over_eating) %>%
  summarise(Total = n())

new_eating_issues <- transform(eating_issues_unsorted, 
                               Poor_appetite_or_over_eating  = reorder(Poor_appetite_or_over_eating, Total))

colourCount = length(unique(Poor_appetite_or_over_eating))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_eating_issues, aes(x = Poor_appetite_or_over_eating, y = Total,
                                     fill=getPalette(colourCount),
                                     label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Poor appetite or over eating"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# Very few clients had a problem with eating

# self_guilt Distribution
guilt_unsorted <- phq9_data %>% 
  group_by(self_guilt) %>%
  summarise(Total = n())

new_guilt <- transform(guilt_unsorted, 
                       self_guilt  = reorder(self_guilt, Total))

colourCount = length(unique(self_guilt))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_guilt, aes(x = self_guilt, y = Total,
                             fill=getPalette(colourCount),
                             label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Self Guilt"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# Self guilt isnt so much a big issue with the clients

# Trouble_concentrating Distribution
concentrating_unsorted <- phq9_data %>% 
  group_by(Trouble_concentrating) %>%
  summarise(Total = n())

new_concentrating <- transform(concentrating_unsorted, 
                               Trouble_concentrating  = reorder(Trouble_concentrating, Total))

colourCount = length(unique(Trouble_concentrating))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_concentrating, aes(x = Trouble_concentrating, y = Total,
                                     fill=getPalette(colourCount),
                                     label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Trouble Concentrating"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))


# Moving_or_speaking_slowly Distribution
snail_unsorted <- phq9_data %>% 
  group_by(Moving_or_speaking_slowly) %>%
  summarise(Total = n())

new_snail <- transform(snail_unsorted, 
                       Moving_or_speaking_slowly  = reorder(Moving_or_speaking_slowly, Total))

colourCount = length(unique(Moving_or_speaking_slowly))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_snail, aes(x = Moving_or_speaking_slowly, y = Total,
                             fill=getPalette(colourCount),
                             label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Moving or speaking slowly"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# Might be the least


# Sucide_Thoughts Distribution
suicide_unsorted <- phq9_data %>% 
  group_by(Sucide_Thoughts) %>%
  summarise(Total = n())

new_suicide <- transform(suicide_unsorted, 
                         Sucide_Thoughts  = reorder(Sucide_Thoughts, Total))

colourCount = length(unique(Sucide_Thoughts))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = new_suicide, aes(x = Sucide_Thoughts, y = Total,
                               fill=getPalette(colourCount),
                               label=Total))+
  geom_bar(stat = "identity"
  )+
  ggtitle(paste("Plot of Distribution on Sucide Thoughts"))+
  labs(x = "Degree of Feeling", y = "Number Times for this Response")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1))

# Nearly no suicide thoughts among clients

# Conclusion is that Feeling_sad__depressed_or_hopeless, No_pleasure_doing_anything, and Trouble_sleeping are extreme problems with most of our clients

# Distribution of our response variable

sessions <- phq9_data %>%
  group_by(status) %>%
  summarise(Total = n())

colourCount = length(unique(sessions$Total))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(data = sessions, aes(x = status, y = Total,
                            fill=getPalette(colourCount),
                            label=Total))+
  geom_bar(stat = "identity"
  )+
  geom_text(hjust = 0.8, vjust = 0,color = "black", 
            fontface = "bold",size = 2)+
  coord_flip()+
  ggtitle(paste("Distribution of status of Clients"))+
  labs(x = "Condition", y = "Number of clients")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", color="#5D5055", 
                                   size=10, angle=0),
        axis.text.y = element_text(face="bold", color="#5D5055", 
                                   size=10, angle=0))+
  theme(axis.text.x = element_text(angle=75, hjust=1))+
  theme(axis.text.y = element_text(angle=45, hjust=1))



# so since classification takes two factors lets transform our sessions into two levels, 1 - difficuilt(extremely difficult and very difficult), 0 - (somewhat difficult and Not difficult at all)

# changing status column as character not factor
phq9_data$status <- as.character(phq9_data$status)

for (each_val in phq9_data[ ,15]) {
  if(each_val == "Not Difficult at All")  gsub("Not Difficult at All" , 0 , phq9_data[,15] )
}