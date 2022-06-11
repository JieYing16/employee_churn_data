library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(reshape)

#----------------------------------Data Cleaning & Transformation-------------------------------------------
#load dataset
employee<-read.csv('employee_churn_data.csv')
glimpse(employee)

#filter only retail department 
retail<-employee %>%
  filter(department == 'retail')

#filter only employee who left 
left_yes<- retail %>%
  filter(left=="yes")

#categorize 'review' column
retail$review<-round(retail$review,2)
retail$review <- replace(retail$review, (retail$review > 0.7), "Exceeding Expectations")
retail$review<-replace(retail$review,retail$review <= 0.50,"Not Meeting Expectations")
retail$review<-replace(retail$review,(retail$review > 0.50 & retail$review <= 0.70),"Meeting Expectations")

#categorize 'tenure' column to category
retail$tenure<-replace(retail$tenure,retail$tenure>=9,"9 to 11 years")
retail$tenure<-replace(retail$tenure,retail$tenure<6,"3 to 5 years")
retail$tenure<-replace(retail$tenure,(retail$tenure>=6 & retail$tenure<9),"6 to 8 years")

#categorize 'ave_hrs_month' column 
retail$avg_hrs_month<-replace(retail$avg_hrs_month,retail$avg_hrs_month>189,"Longest")
retail$avg_hrs_month<-replace(retail$avg_hrs_month,retail$avg_hrs_month<181,"Shortest")
retail$avg_hrs_month<-replace(retail$avg_hrs_month,(retail$avg_hrs_month>=181 & retail$avg_hrs_month<=189),"Average")

#categorize 'satisfaction' column 
retail$satisfaction<-round(retail$satisfaction,2)
retail$satisfaction<-replace(retail$satisfaction,retail$satisfaction>0.60,"Satisfied")
retail$satisfaction<-replace(retail$satisfaction,retail$satisfaction<=0.30,"Dissatisfied")
retail$satisfaction<-replace(retail$satisfaction,(retail$satisfaction>0.30 & retail$satisfaction<=0.60),"Neutral")

#convert 'promoted' column to category
retail$promoted<-replace(retail$promoted,retail$promoted=="0","no")
retail$promoted<-replace(retail$promoted,retail$promoted=="1","yes")

#convert 'bonus' column to category
retail$bonus<-replace(retail$bonus,(retail$bonus=="0"),"no")
retail$bonus<-replace(retail$bonus,(retail$bonus=="1"),"yes")

#----------------------------------EDA---------------------------------------------------------------------

#----------------------------------turnover count (pie chart)----------------------------------------------

left<-retail %>%
  count(left) %>%
  group_by(left) %>%
  mutate(cnt=n()) %>%
  unique() 
left

ggplot(left,aes(x='',y=n,fill=left))+
  geom_bar(width=1,stat='identity',color="white")+
  # geom_text(aes(label=n),vjust=-1)+
  geom_text(aes(label=n), position = position_stack(vjust = 0.5)) +
  coord_polar(theta="y", start=0,direction=-1)+
  theme_minimal()+
  labs(x="",y="",title="Employee Turnover Rate")+
  # theme(legend.position = "none")+
  guides(fill=guide_legend(title="")) +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank() ) +
  theme(axis.ticks = element_blank())

#----------------------------------turnover vs promotion (pie chart)---------------------------------------

leftyes_promoted<-retail %>%
  count(promoted) %>%
  group_by(promoted) %>%
  mutate(cnt=n()) %>%
  unique() 
leftyes_promoted

ggplot(leftyes_promoted,aes(x='',y=n,fill=promoted))+
  geom_bar(width=1,stat='identity',color="white")+
  coord_polar(theta="y", start=0,direction=-1)+
  theme_minimal()+
  labs(x="",y="",title="Employee who left vs Promotion")+
  guides(fill=guide_legend(title="Promotion")) +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank() ) +
  theme(axis.ticks = element_blank())

#----------------------------------turnover vs job performance (bar plot)----------------------------------

left_review<-left_yes %>%
  count(left,review) %>%
  group_by(left,review) %>%
  mutate(cnt=n()) %>%
  unique() 
left_review

ggplot(left_review,aes(x=left,y=n,fill=review))+
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=n),vjust=-0.1,position = position_dodge(width=0.9))+
  labs(x="",y="Count",
       title = "Employee who left vs Reviews",
       fill="Reviews received")+
  theme_minimal()

#----------------------------------turnover vs salary range (bar plot)-------------------------------------

left_salary<-left_yes %>%
  count(salary,left) %>%
  group_by(salary,left) %>%
  mutate(cnt=n()) %>%
  unique() 
left_salary

ggplot(left_salary,aes(x=salary,y=n,fill=salary))+
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=n),vjust=-0.1)+
  labs(x="Salary Category",y="Count",
       title = "Employee who left vs Salary",
       fill="Turnover Rate")+
  theme_minimal()+
  theme(legend.position="none")

#----------------------------------reviews (job performance) vs average working hours per month------------

review_avg<-left_yes %>%
  count(review,avg_hrs_month) %>%
  group_by(review,avg_hrs_month) %>%
  mutate(cnt=n()) %>%
  unique() 
review_avg

ggplot(review_avg,aes(x=avg_hrs_month,y=n,fill=review))+
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=n),vjust=-0.1, position = position_dodge(width=0.9))+
  labs(x="Mean Working Hours per Month",y="Count",
       title = "",
       fill="Job Performance")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.6))

#----------------------------------------------------------------------------------------------------------

#----------------------------------correlation matrix------------------------------------------------------

#filter 'retail' department column
retail_corr<-employee %>%
  filter(department == 'retail') 

#convert 'salary' column to numeric 
retail_corr$salary<-replace(retail_corr$salary,retail_corr$salary=="low","0")
retail_corr$salary<-replace(retail_corr$salary,retail_corr$salary=="medium","1")
retail_corr$salary<-replace(retail_corr$salary,retail_corr$salary=="high","2")

#convert 'left' column to numeric 
retail_corr$left<-replace(retail_corr$left,retail_corr$left=="no","0")
retail_corr$left<-replace(retail_corr$left,retail_corr$left=="yes","1")

#convert all columns to numeric 
lapply(retail_corr_plot,as.numeric)

#create correlation matrix with heat map 
cormat<-cor(retail_corr_plot)
melted_cormat<-melt(cormat)
melted_cormat$value<-round(melted_cormat$value,2)
ggplot(melted_cormat,aes(x=X1,y=X2,fill=value))+
  geom_tile(color="white",lwd=0.5,linetype=1)+
  geom_text(aes(label=value),color="white",size=2.5)+
  coord_fixed()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        rect=element_rect(fill="transparent"))+
  labs(x="",y="",
       title = "Correlation Matrix",
       fill="Corr")
