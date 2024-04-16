
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
mydata2_test <- read.csv(filename2)
#view(mydata2_test)
mydata2<- mydata2_test%>%
  mutate(`Test 2 Benchmark Category` = recode_factor(`Test.2.Benchmark.Category`,
                                                     "At/Above Benchmark" = "At/Above Benchmark",
                                                     "On Watch" = "On Watch",
                                                     "Intervention" = "Intervention",
                                                     "Urgent Intervention" = "Urgent Intervention",
                                                     .ordered = TRUE))%>%
  mutate (`Test 2 Benchmark Category` = na_if(`Test.2.Benchmark.Category`, "-"))%>%
  mutate(`Test 1 Benchmark Category` = `Test.1.Benchmark.Category`)%>%
  mutate(`Test 1 Benchmark Category` = recode_factor(`Test 1 Benchmark Category`,
                                                     "At/Above Benchmark" = "At/Above Benchmark",
                                                     "On Watch" = "On Watch",
                                                     "Intervention" = "Intervention",
                                                     "Urgent Intervention" = "Urgent Intervention",
                                                     .ordered = TRUE))%>%
  mutate (`Test 1 Benchmark Category` = na_if(`Test.1.Benchmark.Category`, "-"))%>%
  mutate(`Growth Proficiency Category` = recode_factor(`Growth.Proficiency.Category`,
                                                       "High Growth, High Proficiency" = "High Growth\n High Proficiency",
                                                       "High Growth, Low Proficiency" = "High Growth\n Low Proficiency",
                                                       "Low Growth, High Proficiency" = "Low Growth\n High Proficiency",
                                                       "Low Growth, Low Proficiency" = "Low Growth\n Low Proficiency",
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
mydata2<-mydata2%>%
  mutate(`Test 1 Benchmark Category` = recode_factor(`Test 1 Benchmark Category`,
                              "At/Above Benchmark" = "At/Above Benchmark",
                              "On Watch" = "On Watch",
                              "Intervention" = "Intervention",
                              "Urgent Intervention" = "Urgent Intervention",
                              .ordered = TRUE))%>%
  mutate(`Test 2 Benchmark Category` = recode_factor(`Test 2 Benchmark Category`,
                                                     "At/Above Benchmark" = "At/Above Benchmark",
                                                     "On Watch" = "On Watch",
                                                     "Intervention" = "Intervention",
                                                     "Urgent Intervention" = "Urgent Intervention",
                                                     .ordered = TRUE))
  
view(mydata2)


# see data ----------------------------------------------------------


head(mydata2)


# see data types ----------------------------------------------------------
str(mydata2_test)
str(mydata2)

# deliverable 1 ----------------------------------------------------------
Fall_Benchmark_Dist_Count<- mydata2%>%
  mutate(student_count = 1)%>%
  group_by(`Grade`)%>%
  summarize(student_total = sum(student_count))

#view(Fall_Benchmark_Dist_Count)

Fall_Benchmark_Dist_G5 <- mydata2%>%
  #filter(`Test 1 Benchmark Category` == "At/Above Benchmark")%>%
  mutate(student_count = 1)%>%
  select( `Grade`, `Test 1 Benchmark Category`,  `student_count`)%>%
  group_by(`Grade`, `Test 1 Benchmark Category`)%>%
  summarize(`Num Students` = sum(student_count))%>%
  left_join(Fall_Benchmark_Dist_Count, by = c( "Grade" = "Grade"))%>%
  mutate(`% Students` = 100*round(`Num Students`/student_total, 2))%>%
  filter(`Grade` == 5)
 

#view(Fall_Benchmark_Dist_G5)

library(ggplot2)
base2 = ggplot(data = Fall_Benchmark_Dist_G5, aes(x=`Test 1 Benchmark Category`, y = `% Students`))

del1 = base2 + geom_bar(position="dodge", stat = "identity", fill = "#0066CC") + theme_minimal()+
  geom_text(aes( y = `% Students`, label = `% Students`,
                 vjust = -.25))+
  # scale_fill_brewer(palette = "Blues")+
  theme(axis.title.x=element_blank())+
  #    axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())+
  labs(
    y = "% Students",
    x= "Benchmark Category",
    title = "Grade 5 Fall Skills",
    subtitle = "Sept 2023 Screening Rising Tide Charter Public School",
    caption = "Source: Renaissance Star Literacy and Math Assessment") +
  scale_color_brewer(palette = "Blues")+ theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 

base = ggplot(data=mydata) 
del1Draft= base + geom_bar(aes(x=LocaleType))
del1Draft

# save del1 
saveRDS(del1, file = "del1.rds")

# deliverable 1 Facet ----------------------------------------------------------
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
base2_facet = ggplot(data = Fall_Benchmark_Dist, aes(x=`Grade`,fill = `Grade`, y = `% Students`))

del1_facet = base2_facet + geom_bar(position="dodge", stat = "identity") + theme_minimal()+
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

# base= ggplot(data=mydata) 
# del1Draft= base + geom_bar(aes(x=LocaleType))
# del1Draft

# save del1
saveRDS(del1, file = "del1.rds")

# save del1 facet
saveRDS(del1_facet, file = "del1_facet.rds")
# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------

Winter_Growth_Dist_Count<- mydata2%>%
  mutate(student_count = 1)%>%
  group_by(`Grade`, `Growth Proficiency Category`)%>%
  summarize(student_total = sum(student_count))

#view(Winter_Growth_Dist_Count)

# Fall_Benchmark_Dist_G5 <- mydata2%>%
#   #filter(`Test 1 Benchmark Category` == "At/Above Benchmark")%>%
#   mutate(student_count = 1)%>%
#   select( `Grade`, `Test 1 Benchmark Category`,  `student_count`)%>%
#   group_by(`Grade`, `Test 1 Benchmark Category`)%>%
#   summarize(`Num Students` = sum(student_count))%>%
#   left_join(Fall_Benchmark_Dist_Count, by = c( "Grade" = "Grade"))%>%
#   mutate(`% Students` = 100*round(`Num Students`/student_total, 2))%>%
#   filter(`Grade` == 5)


math_data <- mydata2%>%
  filter(`Grade` == "5")
base2_final = ggplot(data = math_data, aes(x=`SGP (Expectation=50)`))
labels = labs(
  x= "Student Growth Percentile",
  title = "G5 Fall-Winter Growth",
  subtitle = "Jan 2024 Rising Tide Charter Public School",
  caption = "Source: Renaissance Star Literacy and Math Assessment")
del2_final = base2_final + geom_histogram(fill = "#0066CC",color="#e9ecef", alpha = .6, position = "identity", binwidth = 20)+
  geom_vline(xintercept = 50, color = "grey")+
  annotate("text", x = 63, y = 45, label = "45% Achieved") +
 # scale_fill_manual(values = "#0066CC")+ 
  theme_minimal() + labels + theme(axis.title.y=element_blank()) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
# save del2
saveRDS(del2_final, file = "del2_final.rds")

del2Draft= base + geom_histogram(aes(x=Student.Teacher.Ratio))
del2Draft


# deliverable 2 facet----------------------------------------------------------
math_data <- mydata2%>%
  filter(`Assignment Type` == "Star Math")
base2 = ggplot(data = math_data, aes(x=`SGP (Expectation=50)`, fill = IEP_Status))
labels = labs(
  x= "Student Growth Percentile",
  title = "How Our Math Students Grew",
  caption = "Winter 2024 Screening")
del2 = base2 + geom_histogram(color = "#e9ecef", alpha = .6, position = "identity", binwidth = 20)+
  geom_vline(xintercept = 50)+
  scale_fill_manual(values = c( "#0066CC", "lightcyan"))+ 
  theme_minimal()+ facet_wrap(~`Grade`) + labels + theme(axis.title.y=element_blank())
# save del2
saveRDS(del2, file = "del2.rds")

del2Draft= base + geom_histogram(aes(x=Student.Teacher.Ratio))
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")

# deliverable 3 Scatter with IEP Status ----------------------------------------------------------
G5_Math_data <- mydata2%>%
  filter(Grade == "5")%>%
  filter(`Assignment Type` == "Star Math")

base3_scat = ggplot(data = G5_Math_data, x = `Test 1 PR`, y = `SGP (Expectation=50)`)
del3_scat= base3_scat + geom_point(aes(x=`Test 1 PR`,
                                 y=`SGP (Expectation=50)`, color = `IEP_Status`))+
  annotate("rect", xmin = 0, xmax = 25, ymin = 0, ymax = 49,
           alpha = .2)+
  geom_hline(yintercept = 50, color = "grey")+
  scale_color_manual(values = c("cyan","#0066CC"))+
  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
labs(
  y = "Student Growth Percentile",
  x= "Test 1 Percentile",
  title = "What is happening with Math students on IEPs?",
  subtitle = "Fall 2023 - Winter 2024 Rising Tide Charter Public School",
  caption = "Source: Renaissance Star Math Assessment")
 
del3_scat 

# save del3_scat ----------------------------------------------------------
saveRDS(del3_scat, file = "del3_scat.rds")


# deliverable 3 Growth Category by Subject ----------------------------------------------------------
G5_data <- mydata2%>%
  filter(Grade == "5")%>%
  group_by(`Assignment Type`)%>%
  mutate(student_count = 1)%>%
  summarize(student_total = sum(student_count))

#view(G5_data)

G5_Growth <- mydata2%>%
  filter(`Grade` == "5")%>%
  mutate(student_count = 1)%>%
  select( `Growth Proficiency Category`, `Assignment Type`,  `student_count`)%>%
  group_by(`Assignment Type`, `Growth Proficiency Category`)%>%
  summarize(`Num Students` = sum(student_count))%>%
  left_join(G5_data, by = c("Assignment Type" = "Assignment Type"))%>%
  mutate(`% Students` = 100*round(`Num Students`/student_total, 2))%>%
  ungroup()

#view(G5_Growth)

G5_Growth_Edit <- mydata2%>%
  filter(`Grade` == "5")%>%
  mutate(student_count = 1)%>%
  mutate(`Growth Category` = case_when(
    `Growth Proficiency Category` == "High Growth\n High Proficiency" ~ "High Growth",
    `Growth Proficiency Category` == "High Growth\n Low Proficiency" ~ "High Growth",
    `Growth Proficiency Category` == "Low Growth\n High Proficiency" ~ "Low Growth",
    `Growth Proficiency Category` == "Low Growth\n Low Proficiency" ~ "Low Growth"
  ))%>%
  select( `Growth Category`, `Assignment Type`,  `student_count`)%>%
  group_by(`Assignment Type`, `Growth Category`)%>%
  summarize(`Num Students` = sum(student_count))%>%
  left_join(G5_data, by = c("Assignment Type" = "Assignment Type"))%>%
  mutate(`% Students` = 100*round(`Num Students`/student_total, 2))%>%
  ungroup()

# view(G5_Growth_Edit)


  
 base3_bar = ggplot(data = G5_Growth_Edit, aes(x = `Growth Category`, y = `% Students`, fill = `Assignment Type`))
 del3_bar= base3_bar + geom_bar(position="dodge", stat = "identity")+
   coord_flip()+
 theme(axis.title.y=element_blank())+
   #    axis.text.x=element_blank(),
   #   axis.ticks.x=element_blank())+
   labs(
     y = "% Students",
     x= "Growth Proficiency Category",
     title = "What is happening in G5 Math?",
     subtitle = "Fall 2023 - Winter 2024 Rising Tide Charter Public School",
     caption = "Source: Renaissance Star Literacy and Math Assessment") +
   scale_fill_manual(values = c("darkblue", "skyblue"))+ theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
# 
 del3_bar
 


# save del3_bar ----------------------------------------------------------
saveRDS(del3_bar, file = "del3_bar.rds")
 
# deliverable 3 stacked --------------------------------------------------
 
 base3_stack = ggplot(data = G5_Growth_Edit, aes(x = `Assignment Type`, y = `% Students`, fill = `Growth Category`))
 del3_stack= base3_stack + geom_bar( stat = "identity")+
   coord_flip()+
   theme(axis.title.y=element_blank())+
   #    axis.text.x=element_blank(),
   #   axis.ticks.x=element_blank())+
   labs(
     y = "% Students",
     x= "Assginment Type",
     title = "What is happening in G5 Math?",
     subtitle = "Fall 2023 - Winter 2024 Rising Tide Charter Public School",
     caption = "Source: Renaissance Star Literacy and Math Assessment") +
   scale_fill_manual(values = c("darkblue", "skyblue"))+ theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
 
 # save del3_stack ----------------------------------------------------------
 saveRDS(del3_stack, file = "del3_stack.rds")
 
 
 # deliverable 3 box --------------------------------------------------
 G5_Data<- mydata2%>%
   filter(Grade == "5")
 base3_box = ggplot(data = G5_Data, aes(x = `Assignment Type`, y = `SGP (Expectation=50)`, fill = `Assignment Type`))
 del3_box= base3_box + geom_boxplot()+
   coord_flip()+
   theme(axis.title.y=element_blank())+
   #    axis.text.x=element_blank(),
   #   axis.ticks.x=element_blank())+
   labs(
     y = "Student Growth Percentile",
     x= "Assginment Type",
     title = "What is happening in G5 Math?",
     subtitle = "Fall 2023 - Winter 2024 Rising Tide Charter Public School",
     caption = "Source: Renaissance Star Literacy and Math Assessment") +
   scale_fill_brewer(palette = "Blues")+ theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
 
 # save del3_box ----------------------------------------------------------
 saveRDS(del3_box, file = "del3_box.rds")
 
 
 # deliverable 3 hist --------------------------------------------------
 G5_Data<- mydata2%>%
   filter(Grade == "5")
 dat_text <- data.frame(label = c("40% Achieved", "55% Achieved"), cyl = c("Star Math", "Star Reading"))
 base3_hist = ggplot(data = G5_Data, aes(x = `SGP (Expectation=50)`))
 del3_hist= base3_hist + geom_histogram(fill = "#0066CC",color="#e9ecef", alpha = .6, position = "identity", binwidth = 20)+
   facet_wrap(~`Assignment Type`)+
   theme(axis.title.y=element_blank(),
       axis.title.x=element_blank())+
   #   axis.ticks.x=element_blank())+
   geom_vline(xintercept = 50, color = "grey")+
  # annotate("text", x = 72, y = 25, label = "40% Achieved") +
  # geom_text(data = dat_text, mapping = aes(x = -Inf, y = -Inf, label = label), x = 70, y = 25)+#hjust  = .5, vjust = 1)+
  # ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text_Math",
  #                        cyl = factor(8,levels = c("4","6","8")))
 #p + geom_text(data = ann_text,label = "Text_Math")
   labs(
     y = "Student Growth Percentile",
     x= "Assginment Type",
     title = "What is happening in G5 Math?",
     subtitle = "Fall 2023 - Winter 2024 Rising Tide Charter Public School",
     caption = "Source: Renaissance Star Literacy and Math Assessment") +
   scale_fill_brewer(palette = "Blues")+ theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
 
 # save del3_hist ----------------------------------------------------------
 saveRDS(del3_hist, file = "del3_hist.rds")
 
 
 # deliverable 3 ----------------------------------------------------------

del3Draft= base + geom_point(aes(x=Student.Teacher.Ratio,
                                 y=Free.Lunch))
del3Draft 

# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")

# deliverable 4 final ----------------------------------------------------------

library(sf)
mass_zip_map=sf::read_sf("_data/zipcodes_nt/ZIPCODES_NT_POLY.shp")
head(mass_zip_map)
#head(mydata)

# merge data into map ----------------------------------------------------------
mydataZip=aggregate(data=mydata2,`Test 1 PR`~`Postal Code`,FUN = mean)
#view(mydataZip)
myMapGrade=merge(mass_zip_map,mydataZip,by.x= 'POSTCODE', 'Postal Code', all = TRUE)

# prepare plot

view(myMapGrade)
#myMapGrade_Crop<- st_crop(myMapGrade, xmin = 1, xmax = 2)

base4 = ggplot(myMapGrade)
del4= base4 + geom_sf(aes(fill=`Test 1 PR`), colour = "white")+
  scale_fill_gradient(low = "skyblue",
                      high = "navyblue",
                      name = "Mean PR") +
  labs(
    title = "Where do our struggling students come from?",
    subtitle = "Fall 2023 Screening Rising Tide Charter Public School",
    caption = "Source: Renaissance Star Literacy and Math Assessment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
del4

# save del4 ----------------------------------------------------------
saveRDS(del4, file = "del4.rds")



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

