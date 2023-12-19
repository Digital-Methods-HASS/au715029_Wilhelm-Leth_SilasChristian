#Starting my script i need a base to work with
dir.create("Ros_data")
install.packages("tidyverse")
library(tidyverse)
install.packages("here")
library(ggplot2)

read.csv("Ros_data/RosTableNoComments.csv", sep = ";")
RosTable <- read.csv("Ros_data/RosTableNoComments.csv", sep = ";")

ggplot(data = RosTable,
       mapping = aes(x="Enrollment.year", 
                     y = "Death.year"))+
  geom_point(size=5)+
  geom_line(color = "red")

ggplot(data = RosTable,
       mapping = aes(x = Enrollment.year, 
                     y = Death.year))+
  geom_point(size=5)+
  geom_line(color = "red")

ggplot(data = RosTable,
       mapping = aes(x = Enrollment.year, 
                     y = Married))+
  geom_point(size=2)

ggplot(data = RosTable,
       mapping = aes(x = Enrollment.year, 
                     y = Place.type))+
  geom_point(size=4)

#Conversion to dotplot for a more useful graph

ggplot(data = RosTable,
       mapping = aes(x = Enrollment.year, fill = factor(Place.type)))+
                     geom_dotplot(size=4)

#Now a more complicated one

ggplot(data = RosTable,
       mapping = aes(x = Enrollment.year, fill = factor(Place.type)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")

#This one is good but it would be perfect if I can choose what the y-axis shows, ideally the actual numbers enrolled

ggplot(data = RosTable,
       mapping = aes(x = Enrollment.year, fill = factor(Place.type)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


?ggplot

#Going to try something similar but with whether or not the different types were married
#This is a overall overview of enrollment-years and eventual marriage status

ggplot(data = RosTable, mapping = aes(x = Enrollment.year, fill = factor(Married)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#Now we create the same but with the types separated 

ggplot(data = RosTable, mapping = aes(x = Enrollment.year, fill = factor(Married)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  facet_wrap(~Place.type)

#aes(shape=Place.type), this would be good for shapes instead of separation, but it is not compatible with dotplot

#Lets create one without time as an element, comparing totals

ggplot(data = RosTable, mapping = aes(x = Place.type, fill = factor(Married)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 0.03, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#Calculating the "levealder" (age at death), and more
RosTable %>% 
  mutate(Birthyear = as.numeric(Birthyear)) %>% 
  mutate(Death.year = as.numeric(Death.year)) %>% 
  mutate(Marriage.year = as.numeric(Marriage.year)) %>% 
  mutate(Enrollment.year = as.numeric(Enrollment.year)) %>%
  mutate(AgeAtEnrollment = Enrollment.year-Birthyear) %>% 
  mutate(AgeAtMarriage = Marriage.year-Birthyear) %>% 
  mutate(YearsEnrolledUntilMarriage = Marriage.year-Enrollment.year) %>% 
  mutate(AgeAtDeath = Death.year-Birthyear) -> RosTableAge

#Let's save the table just in case - when the Rstu/Rosda/ is not typed before the requested 
# name ("RostableAge.csv" is the name chosen), it will save in the repository, "Home" 

write_csv(RosTableAge, "RosKlosterRFiles/Ros_data/RosTableAge.csv")

#New plots and graphs making use of the improved table

ggplot(data = RosTableAge, mapping = aes(x = Marriage.year, y = factor(AgeAtMarriage), fill = factor(Place.type)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")
#Does not seem to work, y axis not in use, just a consequence of dotplot i guess - I will try with other types

ggplot(data = RosTableAge, 
       mapping = aes(x = Marriage.year,
                     y = AgeAtMarriage, shape = factor(Place.type), stackgroups = TRUE))+
  geom_point(aes(color = factor(Place.type), size=5))
  
#Again but different test

ggplot(data = RosTableAge, 
       mapping = aes(x = Marriage.year,
                     y = AgeAtMarriage, stackgroups = TRUE))+
  geom_point(aes(color = factor(Place.type), shape = factor(Place.type), size=5))

#I like it but we can do better, particularly making the points not block eachother out

ggplot(data = RosTableAge, 
       mapping = aes(x = Marriage.year,
                     y = AgeAtMarriage, stackgroups = TRUE))+
  geom_point(aes(color = factor(Place.type), shape = factor(Place.type), alpha = 0.9, size=5))

#The next step will be changing the shapes, as the Statute 8 shape is too thin / not clearly visible

ggplot(data = RosTableAge, 
       mapping = aes(x = Marriage.year,
                     y = AgeAtMarriage, stackgroups = TRUE))+
  geom_point(aes(color = factor(Place.type), shape = factor(Place.type), alpha = 0.9, size = 1))+
  scale_shape_manual(values = c("\u2605", "\u26CA", "\u25A0", "\u25B2"))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  scale_size_manual(values = c(1000,1000,1000,1000))+
  theme(legend.position = "right")

#Getting different errors when using custom stuff, so lets go boring

ggplot(data = RosTableAge, 
       mapping = aes(x = Marriage.year,
                     y = AgeAtMarriage, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))

#This works and the colors are how I want them, though symbols do not work 
#also note that alpha channel and size now work

#New day back to it - to warm up, lets create a graph showing years enrolled before marriage, later we will figure out how to show the married 
#jomfruer with unknown marriage years
#YearsEnrolledAtMarriage graph

ggplot(data = RosTableAge, 
       mapping = aes(x = Marriage.year,
                     y = YearsEnrolledAtMarriage, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))

#variation

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = YearsEnrolledAtMarriage, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))

#Plot of age at death

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtDeath, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Married), shape = factor(Place.type)))+
  scale_shape_manual(values = c(18, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C'))

#Lets clean up the levealder-plot 

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtDeath, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Married), shape = factor(Married)))+
  scale_shape_manual(values = c(17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C'))

#same one but with jitter

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtDeath, stackgroups = TRUE))+
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha = 0.7, size = 10, aes(color = factor(Married), shape = factor(Married)))+
  scale_shape_manual(values = c(17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C'))

#Rethinking the levealder-plot
#nr1 rethinking - dotplot

ggplot(data = RosTableAge,
       mapping = aes(x = AgeAtDeath, fill = factor(Married)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#nr2 rethinking - dotplot place.types instead of marriage

ggplot(data = RosTableAge,
       mapping = aes(x = AgeAtDeath, fill = factor(Place.type), ))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#nr3 rethinking - dotplot Enrollmentyear and if ever married

ggplot(data = RosTableAge,
       mapping = aes(x = Enrollment.year, fill = factor(Married)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#nr4 rethinking - dotplot AgeAtDeath, Marriage and Place.type in one

ggplot(data = RosTableAge,
       mapping = aes(x = AgeAtDeath, fill = factor(Married)))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  facet_wrap(~Place.type)

#nr5 rethinking - dotplot AgeAtDeath in relation to Enrollment year, showing only place type as factor

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtDeath))+
  geom_point(position = position_jitter(h=0.2, w=0.2), alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  geom_smooth()

#nr5 rethinking - dotplot AgeAtDeath in relation to birthyear

ggplot(data = RosTableAge, 
       mapping = aes(x = Birthyear,
                     y = AgeAtDeath))+
  geom_point(position = position_jitter(h=0.2, w=0.2), alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  geom_smooth()+
  facet_wrap(~Place.type)

#messing about, experimenting

ggplot(data = RosTableAge,
       mapping = aes(x = Place.type))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")

#...

RosTableAgeObject <- ggplot(RosTableAge, aes(Place.type))

RosTableAgeObject + geom_bar()

RTAObjectMarriedStatus <- ggplot(RosTableAge, aes(Married))

RTAObjectMarriedStatus + geom_bar()

ggplot(RosTableAge, aes(x = Enrollment.year, y = AgeAtDeath))+
  geom_line()

#curvy line
ggplot(RosTableAge, aes(x = Enrollment.year, y = AgeAtDeath))+
  geom_point()+
  geom_smooth()
#straight line
ggplot(RosTableAge, aes(x = Enrollment.year, y = AgeAtDeath))+
  geom_point()+
  geom_smooth(method = "lm")

#Research and possible tests of different graph/plots in ggplot2 
#base detailed plot of enrollment age across the years
ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtEnrollment))+
  geom_point(position=position_jitter(h=0.2, w=0.2), alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))

#histogram visualization

ggplot(RosTableAge, aes(AgeAtEnrollment))+
  geom_histogram()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggplot(RosTableAge, aes(AgeAtEnrollment))+
  geom_bar()+
  scale_x_binned()

#Enrollment age and place type totals
ggplot(RosTableAge, aes(AgeAtEnrollment, fill = Place.type))+
  geom_histogram()

ggplot(RosTableAge, aes(Enrollment.year, after_stat(density), color = Place.type))+
  geom_freqpoly()

#Smooth/Lines attempt, put method = "lm" into the () to get straight line

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtEnrollment))+
  geom_point(position=position_jitter(h=0.2, w=0.2), alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  geom_smooth()

#separated

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtEnrollment))+
  geom_point(position=position_jitter(h=0.2, w=0.2), alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  geom_smooth(method = "lm")+
  facet_wrap(~Place.type)

#When reopening script

library(tidyverse)
library(ggplot2)

#New themes relating to age:

ggplot(data = RosTableAge,
       mapping = aes(x = Birthyear, fill = factor(Place.type), ))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggplot(data = RosTableAge,
       mapping = aes(x = AgeAtDeath, fill = factor(Married), ))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggplot(data = RosTableAge,
       mapping = aes(x = AgeAtEnrollment, fill = factor(Place.type), ))+
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggplot(data = RosTableAge, 
       mapping = aes(x = Marriage.year,
                     y = AgeAtMarriage, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  geom_smooth()

#Figure 14 - Age at marriage and birth year

ggplot(data = RosTableAge, 
       mapping = aes(x = Birthyear,
                     y = AgeAtMarriage, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  geom_smooth()

#Figure 15 - Enrollment age, enrollment year, and place type

ggplot(data = RosTableAge, 
       mapping = aes(x = Enrollment.year,
                     y = AgeAtEnrollment, stackgroups = TRUE))+
  geom_point(alpha = 0.7, size = 10, aes(color = factor(Place.type), shape = factor(Place.type)))+
  scale_shape_manual(values = c(17, 17, 15, 19))+
  scale_color_manual(values = c('#3D56E1', '#DD1313', '#14BE5C', '#A03DE1'))+
  geom_smooth()
