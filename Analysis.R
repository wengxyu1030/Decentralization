library(readxl)
library(openxlsx)
library(tidyverse)
library(readxl)

#############Data import######


# HR Data -----------------------------------------------------------------
library("readxl")
hr_text <- read_excel("input/WBG Full-time Active Staff Data - FY96 to FY20.xlsx",sheet = "SOB Template", col_types = "text")

hr_raw <- read_excel("input/WBG Full-time Active Staff Data - FY96 to FY20.xlsx",sheet = "SOB Template")%>%
  mutate(fy = substr(Snapshot,1,4)) %>%
  mutate(.,fy=as.numeric(fy)) %>%
  mutate(UPI= as.numeric(UPI))

hr_add <- hr_raw %>%    #assume they have same position in the coming year
  group_by(UPI) %>%     #because there are data in trs though not in HR.
  top_n(1,fy) %>%
  ungroup %>%
  mutate(fy = fy+1)

hr <- rbind(hr_raw,hr_add)
save(hr,file = "data/hr.rda")

hr_2<-hr %>% #select and filter hr data: grade, institution
  filter(`Current Grade Group Name`=="GE and up" & 
           `VPU Grouping (New)` != "IGA" & Org=="Bank",  #IGA? NEED EXPAND THE DEF.
         !grepl("EC",`Revised New Grades`) )  %>%
  mutate(upi = as.numeric(UPI)) %>%
  select(-Snapshot, -`PMU (Curr)`, -`PMU Name (Curr)`, -`VPU Grouping (New)`,-UPI) 
  
save(hr_2, file = "data/hr_2.rda")


# TRS official data.  -----------------------------------------------------
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
save(trs,file = "data/trs.rda")



# operation ---------------------------------------------------------------
#import complete operations data(actually the ASA is not accurate, but in analysis only lending needed with rate)
operation_raw <- read_excel("input/portfolio_second_version_withASA.xlsx",
                            sheet = "Rawdata_Projects&Upi", col_types = "text")

add_fin <- read_excel("input/LENDING_PROGRAM_V2.xlsx", col_types = "text") %>%
  filter(ADDTNL_FNCNG_IND == "Y") %>%
  select(ADDTNL_FNCNG_IND, PARENT_PROJ_ID) %>%
  mutate(add_fin = 1) %>%
  rename(projectid = PARENT_PROJ_ID) %>%
  distinct(add_fin, projectid)   #additional financing project parent project identified
  
length <- trs %>% #use the trs range of fy instead of closing fy
  group_by(Proj.ID) %>%
  summarize(length = max(fy) - min(fy)+1)

  
operation_1 <- read_excel("input/portfolio_second_version_withASA.xlsx",
                          sheet = "Rawdata_Projects&Upi", col_types = "text")%>%
  select(projectid,inst,amount,projectname,ttl_upi,
         practice,countryname,revclosing_yr,appfy,lending_instrument)

operation_3 <- read_excel("input/PROJECT_MASTER_V2.xlsx", col_types = "text") %>%
  filter(!is.na(COMPLEX_PROJ_IND)) %>%
  mutate(COMPLEX_PROJ_IND = ifelse(is.na(COMPLEX_PROJ_IND),FALSE,TRUE)) %>%
  select( COMPLEX_PROJ_IND, PROJ_ID ) #complex index. 

operation <- operation_1 %>%
  left_join(.,operation_3, by = c("projectid" = "PROJ_ID")) %>%
  left_join(length,by = c("projectid" = "Proj.ID"))
save(operation, file = "data/operation.rda")

#additional financing should be identified with parent project and join?





# team data ---------------------------------------------------------------
library(lubridate)
team_raw <- read_excel("input/PROJECT_TEAM_V2.xlsx")

date_fy <- function(date) {
  ifelse( month(date) >= 7,
          year(date) +1,
          year(date))
}

team_ttl <- team_raw %>%   
  #date data preparation
  filter(grepl("P",PROJ_ID)) %>%
  mutate(TEAM_START_DATE = as.Date(TEAM_START_DATE, "%Y-%m-%d"),
         TEAM_END_DATE = as.Date(TEAM_END_DATE, "%Y-%m-%d"),
         start_fy = date_fy(TEAM_START_DATE),
         end_fy = date_fy(TEAM_END_DATE)) %>%  
  #identify ttl for each fiscal year. 
  mutate(length = end_fy - start_fy ) %>%
  filter( ROLE_CODE == "TEAMLEAD" ) %>%   #there are cases that unrealistic.
  mutate( end_fy = ifelse(end_fy > 2020, 2020, end_fy)) %>%
  distinct(PROJ_ID,STAFF_UPI,start_fy,end_fy,ADM_LEAD_FLAG) %>% #keep the ADM_LEAD_Flag.
  arrange(PROJ_ID,STAFF_UPI,start_fy) %>%   
  group_by(PROJ_ID, STAFF_UPI, start_fy ,end_fy) %>% #identify the situation where multiple roles in fy+proj.
  mutate(n = n()) %>% 
  filter( n == 1 |
            (n >= 1 & ADM_LEAD_FLAG =="X")) %>%
  mutate(n = n()) %>%   #uniqully identified for proj + fy_period + upi as ttl. 
  ungroup() 
 
 #expand the ttl list to the whole project cycle 
team_ttl_fy <- team_ttl %>%
  group_by(r=row_number()) %>% 
  mutate(fy = list(start_fy:end_fy))%>%
  ungroup %>% 
  select(-r,-start_fy,-end_fy,-n) %>% 
  unnest() %>%
  mutate(ttl = 1)

save(team_ttl_fy, file = "data/team_ttl_fy.rda")

tech <- c("spec(.|ialist)","econ","((?<!country )operations (officer|analyst))","(program |reserach |learning |(?<!accounting ))analyst",
          "Professional","(program |practice |sector )manager","(?<!operations )adviser","engineer",
          "Program Coordinator","scientist","Statistician")

ops <- c("procurement","financial management","safeguard","social development","environmental specialist",
         "Environmental Economist","Counsel")

plsl <- c("Country Sector Coordinator","Sector Leader","Program Leader")


min_trs_fy <- min(trs$fy)

staff_pre <- operation %>%  #staff placement and hr info to project+fy level.
  dplyr::rename(ttl_upi_current = ttl_upi)  %>%  
  
  #only project approved after with trs record
  filter(appfy >= min_trs_fy) %>%
  
  #choose operations with trs record (fy and team member upi)
  inner_join(.,trs[,c("fy","Proj.ID","Employee")], by = c("projectid" = "Proj.ID")) %>%
  mutate(fy = as.numeric(fy)) %>%
  
  #tag the project ttl info by fy
  left_join(.,team_ttl_fy, by = c("projectid" = "PROJ_ID", "fy"="fy", "Employee" = "STAFF_UPI"))%>%  
  mutate( ttl = ifelse(is.na(ttl),0,1)) %>% #trs not matched because the string matching issue. 
  
  #tag the staff hr info by fy
  inner_join(.,hr_2, by = c("Employee" = "upi", "fy"="fy"))%>%  
  dplyr::rename(upi = Employee) %>%
  select(fy,projectid,upi,ttl_upi_current,                            
         ttl,ADM_LEAD_FLAG,`IRS Flag`,`Duty Country Name`,`Title Name`) %>%
  mutate(ttl_upi_fy = ifelse(ttl == 1, upi,NA) ) %>%
  distinct() %>%
  group_by(projectid,fy) %>%  
  #the main ttl in that fy and the co_ttls
  mutate(  lead_ttl = ifelse(
           ADM_LEAD_FLAG == "X" & ( n_distinct(ttl_upi_fy) == 1 |
           (ttl_upi_current == ttl_upi_fy & n_distinct(ttl_upi_fy) >= 2)),
           1,0),
           co_ttl = ifelse(ttl == 1 & lead_ttl == 0,1,0)
           )%>%
  ungroup() %>%
  #the type of staff 
  mutate(type = case_when(grepl(paste(ops,collapse = "|"),`Title Name`,ignore.case = TRUE) ~ "Fiduciary, safeguards, legal staff",
                          grepl(paste(tech,collapse = "|"), `Title Name`,perl=TRUE, ignore.case=TRUE) ~ "Sector specialists/program managers (PMs)",
                          grepl(paste(plsl,collapse = "|"),`Title Name`,ignore.case = TRUE) ~ "Program/sector leaders",
                          TRUE ~ "CMU")) 

staff_dec <- staff_pre %>% #test dummies on field prsence of IRS, Functional Staff, Co-TTLs
  group_by(projectid) %>%
  mutate(length = max(fy)-min(fy)+1) %>%
  group_by(projectid,length) %>%
  mutate( dec = ifelse(`Duty Country Name` == "United States",0,1),
          #field prsence of international staff
          irs = ifelse(`IRS Flag` == "Y" & dec == 1, 1, 0),
          #field presence of functional staff on field
          tech = ifelse(type == "Sector specialists/program managers (PMs)"& dec == 1,1,0),
          #field presence of co_ttl
          co_ttl = ifelse(co_ttl == 1 & dec == 1,1,0))%>%
  summarise_at(vars(irs,tech,co_ttl),sum) %>%
  mutate(irs = irs/length, 
            tech = tech/length,
            co_ttl = co_ttl/length) %>%
  ungroup() %>%
  select(-length)
   
skim(staff_dec)
staff_dec %>% mutate_at(vars(irs,tech,co_ttl),log)%>%skim()  

# IEG Rating --------------------------------------------------------------
#IEG Rating data (need the governmence performance rating.)
var_rate <- c("IEG_Outcome", "IEG_BankQualityAtEntry","IEG_BankQualityOfSupervision","IEG_OverallBankPerf","IEG_OverallBorrPerf","IEG_GovernmentPerf","IEG_ImplementingAgencyPerf","IEG_MEQuality","IEG_ICRQuality")
rate_levels<- c("HIGHLY UNSATISFACTORY","UNSATISFACTORY",
                "MODERATELY UNSATISFACTORY","MODERATELY SATISFACTORY",
                "SATISFACTORY","HIGHLY SATISFACTORY") #covert the rating to factor with ordered levels. 
cvt_lvls <- function(x) {
  x <- factor(x, levels = rate_levels)}

rating <- read.csv("input/IEG_World_Bank_Project_Performance_Ratings.csv",header = TRUE) %>%
  mutate_at(vars(var_rate),cvt_lvls)

rating %>%   #there's duplicating in Rating for project, exclude the duplicated IEG_EvalDate,IEG_EvalType
  select(Project.ID,IEG_EvalDate,IEG_EvalType)%>%
  anyDuplicated()

perf_vars <- c("IEG_OverallBankPerf","IEG_BankQualityAtEntry","IEG_BankQualityOfSupervision")

rating_2 <- rating %>%   #exclude the duplicated case (latest time and exclude "PAR")
  mutate(IEG_EvalDate= as.Date(rating$IEG_EvalDate, "%m/%d/%y"))%>%
  filter_at(vars(perf_vars),any_vars(!is.na(.))) %>%   #drop if all the rating is missing. 
  filter_at(vars(perf_vars),all_vars(!grepl("NOT",.))) %>%  #drop the rating is as "not applicable" or "not rated" or missing                  
  group_by(Project.ID) %>%
  top_n(1,IEG_EvalDate) %>%
  top_n(1,IEG_EvalFY) %>%
  filter(!(IEG_EvalType=="PAR" & n() >=2)) %>%
  mutate(join_id = as.numeric(sub(".","",Project.ID))) %>%
  select(var_rate,Project.ID,IEG_EvalFY,join_id,Approval.FY)

save(rating_2,file = "data/rating_2.rda")


# control vars ------------------------------------------------------------

#iso code
iso <-read_excel("input/iso.xlsx") 

#CPIA data (waiting for update complete data)
library(data.table)
library(readxl)

df <- data.table()
for (i in 2002:2018){
  sheet <- as.character(i)
  temp.df <- as.data.table(read.xlsx("input/CPIA_1990-2018_aw.xlsx",
                                     sheet = sheet, cols = c(1:3),skipEmptyRows = TRUE))
  temp.df[,fy:=i]
  df <- rbind(df,temp.df)
}
  
cpia_raw <- df %>%  #clean the data
  mutate(Country = toupper(str_trim(Country)),Code = toupper(str_trim(Code)),
         fy = as.character(fy))

cpia_ctr <- cpia_raw %>%
  distinct(Country,Code)  #there are different fype of country, suggest all using coutnry code in analysis.

cpia_id <- table(cpia_raw$Code,cpia_raw$fy) %>%   #prepare to impute
  as.data.frame() %>%
  rename(Code = Var1,
         fy  = Var2) %>%
  select(Code,fy) %>%
  arrange(Code,fy) %>%
  mutate(fy = as.character(fy)) %>%
  distinct()

count(cpia_raw,fy)%>% summary()
count(cpia_id,Code) %>% summary() #Every country has 17 years recode. 

library(imputeTS)
library(zoo)

cpia <- cpia_id %>%
  left_join(cpia_raw) %>% #need to impute the missing data. 
  select(-Country) %>%
  mutate(fy = as.numeric(fy))%>% 
  pivot_wider( names_from = Code, values_from = Overall) %>%
  as.ts() %>% 
  na_interpolation(option = "linear")%>%
  as.data.frame() %>%
  pivot_longer(-fy,names_to = "code", values_to = "cpia") %>%
  left_join(iso,by = c("code" = "iso3")) %>%
  rename(iso_n = Numeric,
         iso3 = code)
  
#other country level control
ctr_tag <- read_excel("input/LENDING_PROGRAM_V2.xlsx", col_types = "text")%>%
  #import FCV, Income Group, Lending Group,country tag by fy.
  distinct(APPRVL_FY,FRAGILE_STATES_CODE,CNTRY_CODE,LNDNG_GRP_CODE,INC_GRP_CODE) %>%
  rename(iso2 = CNTRY_CODE,
         fcv = FRAGILE_STATES_CODE,
         appfy = APPRVL_FY) %>%
  select(appfy, iso2, fcv) #income group is missing but currently not using it in analysis

#The time zone data +-3 as nearby.

#############Data preparation######

# Dummies generate --------------------------------------------------------


#data preparation: ops_trs_team_rating
ops_trs_ttl <- trs %>%
  right_join(.,operation, by = c("Proj.ID"="projectid"))%>%
  inner_join(.,hr_2, by = c("fy" = "fy", "Employee" = "UPI")) %>%  #selected employee
  left_join(.,team, by = c("Proj.ID" = "PROJ_ID","Employee" = "STAFF_UPI" )) # add the team ttl info

ops_trs_hr <- trs %>%
  right_join(.,operation, by = c("Proj.ID"="projectid"))%>%
  inner_join(.,hr_2, by = c("fy" = "fy", "Employee" = "UPI")) #selected employee
save(ops_trs_hr, file = "data/ops_trs_hr.rda")

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
save(ops_trs_hr_rating, file = "data/ops_trs_hr_rating.rda")


