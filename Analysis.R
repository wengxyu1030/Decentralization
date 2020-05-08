library(readxl)
library(openxlsx)
library(tidyverse)
library(readxl)

#############Data import######

#input HR data
library("readxl")
hr <- read_excel("input/WBG Full-time Active Staff Data - FY96 to FY20.xlsx",sheet = "SOB Template")%>%
  mutate(fy = substr(hr$Snapshot,1,4)) %>%
  mutate(.,fy=as.numeric(fy)) %>%
  mutate(TRS.Employee.UPI= as.numeric(UPI))

hr_2<-hr %>% #select and filter hr data: grade, institution
  filter(`Current Grade Group Name`=="GE and up" & 
           `VPU Grouping (New)` != "IGA" & Org=="Bank",
          !grepl("EC",`Revised New Grades`) )  %>%
  select(-Snapshot, -UPI, -`PMU (Curr)`, -`PMU Name (Curr)`, -`VPU Grouping (New)`)

#extract the TRS official data. 
trs_time <- read.xlsx("input/trs/trs_0219_time.xlsx",sheet = "FY19",startRow = 9) #staff.week
trs_cost <- read.xlsx("input/trs/trs_0219_cost.xlsx",sheet = "FY19",startRow = 10) #actual cost
trs <- trs_time %>% 
  left_join(., trs_cost) %>%
  mutate(fy = paste0("20",substr(Fiscal.year,3,4))) %>%
  mutate(fy = as.numeric(fy),
         Employee = as.numeric(Employee)) %>%
  select(-X1,-X2) %>%
  rename( Proj.Name = X5,
          Employee.Name = X7, 
          Duty.Country.Name = X9,
          Team.Leader.Name = X11,
          Actual.Cost = `$`) %>%
  select(fy,Proj.ID, Employee,Employee.Name,
         Gross.Staff.weeks, Normalized.Staff.weeks, Actual.Cost) # there are cases with charged time with no empolyee info however.

#extrat team data
team <- read_excel("input/PROJECT_TEAM_V2.xlsx") %>%
  distinct(PROJ_ID,ROLE_CODE,STAFF_UPI,TEAM_SPECIALIZATION_NAME)

team %>% 
  group_by(PROJ_ID, STAFF_UPI) %>%
  mutate(n = n_distinct(ROLE_CODE)) %>% 
  filter(n>1) %>%
  arrange(PROJ_ID, STAFF_UPI, ROLE_CODE) %>%
  View()

team_test<- read_excel("input/")

#import complete operations data
operation_raw <- read_excel("input/portfolio_second_version_withASA.xlsx",
           sheet = "Rawdata_Projects&Upi", col_types = "text")

operation_1 <- read_excel("input/portfolio_second_version_withASA.xlsx",
                          sheet = "Rawdata_Projects&Upi", col_types = "text")%>%
  mutate(addfinancing = ifelse(is.na(addfinancing),FALSE,TRUE),
         app_date = as.Date(app_date, "%d.%m.%Y"),
         revclosing_date = as.Date(revclosing_date, "%d.%m.%Y"), 
         length = as.numeric( revclosing_date - app_date))%>%
  select(projectid,addfinancing,length,inst,amount,ttl_upi, ttl_name, projectname,
         practice,countryname,revclosing_yr,appfy,
         cottl1_upi,cottl2_upi,leadcoteamleader_upi,leadttl_upi,fmsspecialist_upi)

operation_2 <- read_excel("input/LENDING_PROGRAM_V2.xlsx", col_types = "text") %>%
  select(PROJ_ID,FRAGILE_STATES_CODE,CNTRY_CODE,LNDNG_GRP_CODE,INC_GRP_CODE,TOT_CMT_USD_AMT) #import FCV, Income Group, Lending Group, Commmitment.

operation_3 <- read_excel("input/PROJECT_MASTER_V2.xlsx", col_types = "text") %>%
  filter(!is.na(COMPLEX_PROJ_IND)) %>%
  mutate(COMPLEX_PROJ_IND = ifelse(is.na(COMPLEX_PROJ_IND),FALSE,TRUE)) %>%
  select( COMPLEX_PROJ_IND, PROJ_ID ) #complex index. 

operation <- operation_1 %>%
  left_join(.,operation_2, by = c("projectid" = "PROJ_ID")) %>%
  left_join(.,operation_3, by = c("projectid" = "PROJ_ID")) 

#IEG Rating data 
var_rate <- c("IEG_Outcome", "IEG_BankQualityAtEntry","IEG_BankQualityOfSupervision","IEG_OverallBankPerf","IEG_OverallBorrPerf","IEG_GovernmentPerf","IEG_ImplementingAgencyPerf","IEG_MEQuality","IEG_ICRQuality")
rate_levels<- c("HIGHLY UNSATISFACTORY","UNSATISFACTORY",
                "MODERATELY UNSATISFACTORY","MODERATELY SATISFACTORY",
                "SATISFACTORY","HIGHLY SATISFACTORY") #covert the rating to factor with ordered levels. 
cvt_lvls <- function(x) {
  x <- factor(x, levels = rate_levels)
}

rating<-read.csv("input/IEG_World_Bank_Project_Performance_Ratings.csv",header = TRUE)
rating %>%   #there's duplicating in Rating for project, exclude the duplicated IEG_EvalDate,IEG_EvalType
  select(Project.ID,IEG_EvalDate,IEG_EvalType)%>%
  anyDuplicated()
rating[var_rate] <- lapply(rating[var_rate],cvt_lvls)

rating_2 <- rating %>%   #exclude the duplicated case (latest time and exclude "PAR")
  mutate(IEG_EvalDate= as.Date(rating$IEG_EvalDate, "%m/%d/%y"))%>%
  filter(!grepl("NOT",IEG_OverallBankPerf)|IEG_OverallBankPerf=="")%>%  #drop the rating is as "not applicable" or "not rated" or missing                  
  filter(!grepl("NOT",IEG_BankQualityAtEntry)|IEG_BankQualityAtEntry=="") %>%
  filter(!grepl("NOT",IEG_BankQualityOfSupervision)|IEG_BankQualityOfSupervision=="") %>%
  group_by(Project.ID) %>%
  top_n(1,IEG_EvalDate) %>%
  top_n(1,IEG_EvalFY) %>%
  filter(!(IEG_EvalType=="PAR" & n() >=2)) %>%
  mutate(join_id = as.numeric(sub(".","",Project.ID))) %>%
  select(var_rate,Project.ID,IEG_EvalFY,join_id,Approval.FY)

length(unique(rating$Project.ID))/length(unique(rating_2$Project.ID))
anyDuplicated(rating_2$Project.ID)

save(rating_2,file = "data/rating_2.RData")

#CPIA data
cpia <- read_excel("input/CPIAEXCEL.xlsx")

#Fragile State Index
fsi_2019 <-read_xlsx("input/fragile_state_index/fsi-2019.xlsx") %>%
  select(-17) %>%
  write.xlsx("input/fragile_state_index/fsi-2019.xlsx")

files <- list.files("input/fragile_state_index")
fsi <- lapply(paste0("input/fragile_state_index/",files),read_xlsx)
fsi <- do.call("rbind" , fsi)

unique(fsi$Country) %>% length()
#############Data preparation######

#data preparation: ops_trs_team_rating
ops_trs_team_raw <- trs %>%
  right_join(.,operation, by = c("Proj.ID"="projectid"))%>%
  full_join(.,hr, by = c("fy" = "fy", "Employee" = "TRS.Employee.UPI")) %>%  #selected employee
  left_join(.,team, by = c("Proj.ID" = "PROJ_ID","Employee" = "STAFF_UPI" )) # the team role info is not useful

ops_trs_team <- trs %>%
  right_join(.,operation, by = c("Proj.ID"="projectid"))%>%
  inner_join(.,hr_2, by = c("fy" = "fy", "Employee" = "TRS.Employee.UPI")) %>%  #selected employee
  left_join(.,team, by = c("Proj.ID" = "PROJ_ID","Employee" = "STAFF_UPI" )) # the team role info is not useful

ops_trs_hr <- trs %>%
  right_join(.,operation, by = c("Proj.ID"="projectid"))%>%
  inner_join(.,hr_2, by = c("fy" = "fy", "Employee" = "TRS.Employee.UPI")) #selected employee

ops_trs_hr_rating <- ops_trs_hr %>%      #join by number is better than string
  mutate(join_id = as.numeric(sub(".","",Proj.ID))) %>%
  inner_join(.,rating_2, by = "join_id") %>%
  filter(Gross.Staff.weeks >= 0) %>%                        
  mutate(dec_all= ifelse(`US/Non-US Location`=="Non-US Based",1,0)) %>%  
  mutate(dec_ctr = ifelse(`Duty Country Name`== countryname,1,0)) %>%  #use iso code is better, which ctr to use?
  mutate(dec_oth = ifelse(dec_all==1 & dec_ctr==0,1,0)) %>%
  group_by(Proj.ID) %>%
  mutate(part = (Gross.Staff.weeks/sum(Gross.Staff.weeks)*100) )%>% #footnote that the selection criteria applied to the sum as well.
  ungroup() %>%
  select(Proj.ID,dec_all,dec_ctr,dec_oth,part,length,sort(names(.)))



