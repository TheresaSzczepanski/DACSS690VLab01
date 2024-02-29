
# clean memory ------------------------------------------------------------
rm(list = ls())

# load libraries -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(purrr)
library(plotly)
library(dplyr)
library(RColorBrewer)

# read in data ------------------------------------------------------------
#set working directory

filename="theFile.csv"
mydata=read.csv(filename)

filename2 = "_data/PrivProtectWinterGrowthAddress.csv"
mydata2 <- read.csv(filename2)
#view(mydata2)
mydata2<- mydata2%>%
  mutate(`Test 2 Benchmark Category` = recode_factor(`Test.2.Benchmark.Category`,
                                                     "At/Above Benchmark" = "At/Above Benchmark",
                                                     "On Watch" = "On Watch",
                                                     "Intervention" = "Intervention",
                                                     "Urgent Intervention"= "Urgent Intervention",
                                                     .ordered = TRUE))%>%
  mutate (`Test 2 Benchmark Category` = na_if(`Test.2.Benchmark.Category`, "-"))%>%
  mutate(`Test 1 Benchmark Category` = recode_factor(`Test.1.Benchmark.Category`,
                                                     "At/Above Benchmark" = "At/Above Benchmark",
                                                     "On Watch" = "On Watch",
                                                     "Intervention" = "Intervention",
                                                     "Urgent Intervention"= "Urgent Intervention",
                                                     .ordered = TRUE))%>%
  mutate (`Test 1 Benchmark Category` = na_if(`Test.1.Benchmark.Category`, "-"))%>%
  mutate(`Growth Proficiency Category` = recode_factor(`Growth.Proficiency.Category`,
                                                       "High Growth, High Proficiency" = "High Growth, High Proficiency",
                                                       "High Growth, Low Proficiency" = "High Growth, Low Proficiency",
                                                       "Low Growth, High Proficiency" = "Low Growth, High Proficiency",
                                                       "Low Growth, Low Proficiency" = "Low Growth, Low Proficiency",
                                                       .ordered = TRUE))%>%
   mutate(`Grade` = as.factor(`Grade`))%>%
  mutate(`IEP` = as.factor(`IEP`))%>%
  mutate(`IEP_Status` = case_when(
    IEP == 0 ~ "No",
    IEP == 1 ~ "Yes"
  ))%>%
  mutate(`Assignment Type` = `Assignment.Type`)%>%
  mutate(`Postal Code` = `Postal.Code`)%>%
  mutate(`Test 1 PR` = `Test.1.PR`)%>%
  mutate(`Test 2 PR` = `Test.2.PR`)%>%
  mutate(`SGP (Expectation=50)` = SGP..Expectation.50.)%>%
  mutate(`TS Placement` = TS.Placement)%>%
  select(`SASID`, `City`, `Postal Code`, `Grade`, `IEP_Status`, `Assignment Type`, `Test 1 Benchmark Category`, `Test 1 PR`, `Test 2 Benchmark Category`, `Test 2 PR`, `Growth Proficiency Category`, `SGP (Expectation=50)`, `TS Placement`)

mydata2<-na.omit(mydata2)

view(mydata2)


# see data ----------------------------------------------------------


head(mydata2)


# see data types ----------------------------------------------------------

str(mydata2)

# deliverable 1 ----------------------------------------------------------
Fall_Benchmark_Dist_Count<- mydata2%>%
  mutate(student_count = 1)%>%
  group_by(`Grade`, `Assignment Type`)%>%
  summarize(student_total = sum(student_count))

#view(Fall_Benchmark_Dist_Count)

Fall_Benchmark_Dist <- mydata2%>%
  filter(`Test 1 Benchmark Category` == "At/Above Benchmark")%>%
  mutate(student_count = 1)%>%
  select( `Assignment Type`, `Grade`,  `student_count`)%>%
  group_by(`Grade`, `Assignment Type`)%>%
  summarize(`Num Students` = sum(student_count))%>%
  left_join(Fall_Benchmark_Dist_Count, by = c("Assignment Type" = "Assignment Type", "Grade" = "Grade"))%>%
  mutate(`% Students` = 100*round(`Num Students`/student_total, 2))

#view(Fall_Benchmark_Dist)

library(ggplot2)
base2 = ggplot(data = Fall_Benchmark_Dist, aes(x=`Grade`,fill = `Grade`, y = `% Students`))

del1 = base2 + geom_bar(position="dodge", stat = "identity") + theme_minimal()+
  facet_wrap(~`Assignment Type`)+
  geom_text(aes( y = `% Students`, label = `% Students`,
                 vjust = -.25))+
  # scale_fill_brewer(palette = "Blues")+
  #theme(axis.title.x=element_blank(),
  #    axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())+
  labs(
    y = "% Students",
    x= "Grade",
    title = "At/Above Benchmark",
    caption = "Fall 2023 Screening") +
  scale_fill_brewer(palette = "Blues")

base= ggplot(data=mydata) 
del1Draft= base + geom_bar(aes(x=LocaleType))
del1Draft

# save del1
saveRDS(del1, file = "del1.rds")
# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------
math_data <- mydata2%>%
  filter(`Assignment Type` == "Star Math")
base2 = ggplot(data = math_data, aes(x=`SGP (Expectation=50)`, fill = IEP_Status))
labels = labs(
  x= "Student Growth Percentile",
  title = "How Our Math Students Grew",
  caption = "Winter 2024 Screening")
del2 = base2 + geom_histogram(color = "#e9ecef", alpha = .6, position = "identity", binwidth = 20)+
  geom_vline(xintercept = 50)+
  scale_fill_manual(values = c("#69b3a2", "#404080"))+ 
  theme_minimal()+ facet_wrap(~`Grade`) + labels + theme(axis.title.y=element_blank())
# save del2
saveRDS(del2, file = "del2.rds")

del2Draft= base + geom_histogram(aes(x=Student.Teacher.Ratio))
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

del3Draft= base + geom_point(aes(x=Student.Teacher.Ratio,
                                 y=Free.Lunch))
del3Draft 

# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
county_map=sf::read_sf("WA_County_Boundaries.geojson")
head(county_map)
head(mydata)

# merge data into map ----------------------------------------------------------
mydataCounty=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
myMapLunch=merge(county_map,mydataCounty,by.x='JURISDIC_2',"County")

# prepare plot

base=ggplot(myMapLunch)
del4Draft=base + geom_sf(aes(fill=Free.Lunch))
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")

