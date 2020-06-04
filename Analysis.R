library(readxl)
library(openxlsx)
library(tidyverse)
library(readxl)

#############Data import and clean######


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
  mutate(upi = as.numeric(UPI),
         ctr_duty_iso3c = countrycode(`Duty Country Name`, 
                             origin = 'country.name',
                             destination = 'iso3c',
                             custom_match = c('Kosovo'='KVO',
                                              'Micronesia'= 'FSM',
                                              'Yugoslavia'= 'SRB',
                                              'Serbia and Montenegro' = 'SRB'))) %>%
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
  summarise(length = max(fy) - min(fy)+1) %>%
  ungroup

operation_1 <- read_excel("input/portfolio_second_version_withASA.xlsx",
                          sheet = "Rawdata_Projects&Upi", col_types = "text")%>%
  select(projectid,inst,amount,projectname,ttl_upi,
         practice,countryname,revclosing_yr,appfy,lending_instrument,region) %>%
  dplyr::rename(rgn_proj = region)

operation_3 <- read_excel("input/PROJECT_MASTER_V2.xlsx", col_types = "text") %>%
  filter(!is.na(COMPLEX_PROJ_IND))  %>%
  select( COMPLEX_PROJ_IND, PROJ_ID ) #complex index. 

operation <- operation_1 %>%
  left_join(.,operation_3, by = c("projectid" = "PROJ_ID")) %>%
  inner_join(length,by = c("projectid" = "Proj.ID"))%>% #caveat: there are projects never in the TRS system.
  left_join(add_fin,by = "projectid") %>%
  mutate(ctr_proj_iso3c = countrycode(countryname, 
                             origin = 'country.name',
                             destination = 'iso3c',
                             custom_match = c('Kosovo'='KVO',
                                              'Micronesia'= 'FSM',
                                              'Yugoslavia'= 'SRB',
                                              'Serbia and Montenegro' = 'SRB')),
         add_fin = ifelse(!is.na(add_fin),1,0),
         COMPLEX_PROJ_IND = ifelse(is.na(COMPLEX_PROJ_IND),0,1),
         appfy = round(as.numeric(appfy),0))
skim(operation)
save(operation, file = "data/operation.rda")


#############Data preparation######

# staff_proj_level data ---------------------------------------------------------------
library(lubridate)
load("data/trs.rda")
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

legal <- c("counsel","legal","law")

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
         ttl,ADM_LEAD_FLAG,`IRS Flag`,`Duty Country Name`,ctr_duty_iso3c,`Title Name`,
         countryname, ctr_proj_iso3c,`Bank Geographical Regions`,rgn_proj) %>%
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
                          TRUE ~ "CMU")) %>%
  mutate(func = ifelse(type == "Fiduciary, safeguards, legal staff" & 
                         !grepl(paste(legal,collapse = "|"),`Title Name`,ignore.case = TRUE),
                       1,0))
skim(staff_pre)
save(staff_pre, file = "staff_pre.rda")
save(staff_pre,file = "data/staff_pre.rda")

#The time zone data by based cities (+-3 as nearby).
load("data/time_gap.rda")

#generate the field presence dummies
staff_dec <- staff_pre %>% #test dummies on field prsence of IRS, Functional Staff, Co-TTLs
  left_join(time_gap,by=c("ctr_proj_iso3c"="iso3c"))%>%
  dplyr::rename(ctr_proj_utc = utc,
                rgn_duty = `Bank Geographical Regions`,
                ctr_proj = countryname,
                ctr_duty = `Duty Country Name`)%>%
  left_join(time_gap,by=c("ctr_duty_iso3c"="iso3c"))%>%
  dplyr::rename(ctr_duty_utc = utc) %>%
  #the definition of field presence: exclude not in DC not nearby (+-3UTC)
  mutate( dec = ifelse(ctr_duty != "United States" & 
                       (rgn_duty == rgn_proj | abs(ctr_proj_utc-ctr_duty_utc)<=3),
                       1,0)) %>%
  select(-ctr_duty_utc, -ctr_proj_utc, -rgn_proj, -rgn_duty) %>%
  #define the field presence by tests
  mutate(#field prsence of international staff
          irs = ifelse(`IRS Flag` == "Y" & dec == 1, 1, 0),
          #field presence of functional staff on field
          tech = ifelse(type == "Sector specialists/program managers (PMs)"& dec == 1,1,0),
          #field presence of co_ttl
          co_ttl = ifelse(co_ttl == 1 & dec == 1,1,0), #both lead and co_ttl
          ttl = ifelse(ttl == 1 & dec == 1,1,0),
          func = ifelse(func == 1 & dec == 1,1,0)) %>%
  distinct(projectid, dec, irs, tech, co_ttl,ctr_proj,ctr_proj_iso3c,func, ttl) %>%  
  group_by(projectid,ctr_proj,ctr_proj_iso3c) %>%
  #as long as there's one type of staff exist on the field, count as 1. 
  summarise_at(vars(irs,tech,co_ttl,func,ttl,dec),mean)%>% 
  #later could caculate the intensity: more field staff or less field staff.
  mutate(irs = ifelse(irs > 0, TRUE, FALSE),
         tech = ifelse(tech > 0, TRUE, FALSE),
         co_ttl = ifelse(co_ttl > 0, TRUE, FALSE),
         func = ifelse(func > 0, TRUE, FALSE),
         ttl = ifelse(ttl > 0, TRUE, FALSE),
         dec = ifelse(dec > 0, TRUE, FALSE)) %>%
  ungroup() 
   
skim(staff_dec)
   #staff_dec %>% mutate_at(vars(irs,tech,co_ttl),log)%>%skim()  #later
save(staff_dec,file = "data/staff_dec.rda")

#PM field presence
read_excel("input/LENDING_PROGRAM_V2.xlsx", col_types = "text") %>% #sector/practice manager in title.
  distinct(PRACTICE_MGR_UPI,PROJ_ID,RGN_CODE,CNTRY_SHORT_NAME,APPRVL_FY) %>%
  mutate(pm = ifelse(PRACTICE_MGR_UPI>=1,1,0)) %>%
  group_by(APPRVL_FY) %>%
  summarize(n = mean(pm)) %>%
  ungroup %>%
  filter(between(APPRVL_FY,2002,2018))%>%
  mutate(APPRVL_FY = round(as.numeric(APPRVL_FY),0)) %>%
  ggplot(.,aes(APPRVL_FY,n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

pm_pre <- read_excel("input/portfolio_second_version_withASA.xlsx",
                     sheet = "Rawdata_Projects&Upi", col_types = "text") %>%
  dplyr::rename( PRACTICE_MGR_UPI = practicemgr_upi,
                 PROJ_ID = projectid,
                 RGN_CODE = region,
                 CNTRY_SHORT_NAME = countryname,
                 APPRVL_FY = appfy) %>%
  distinct(PRACTICE_MGR_UPI,PROJ_ID,RGN_CODE,CNTRY_SHORT_NAME,APPRVL_FY) %>%
  mutate(fy = round(as.numeric(APPRVL_FY),0),
         upi = round(as.numeric(PRACTICE_MGR_UPI),0),
         ctr_proj_iso3c = countrycode(CNTRY_SHORT_NAME, 
                                      origin = 'country.name',
                                      destination = 'iso3c',
                                      custom_match = c('Kosovo'='KVO',
                                                       'Micronesia'= 'FSM',
                                                       'Yugoslavia'= 'SRB',
                                                       'Serbia and Montenegro' = 'SRB'))) %>%
  select(PROJ_ID,fy,upi,RGN_CODE,ctr_proj_iso3c) %>%
  inner_join(.,hr_2[,c("upi","fy","ctr_duty_iso3c","Bank Geographical Regions")], 
            by = c("upi", "fy")) %>%
  dplyr::rename(rgn_proj = RGN_CODE,
                rgn_duty = `Bank Geographical Regions`) %>%
  left_join(time_gap,by=c("ctr_proj_iso3c"="iso3c"))%>%
  dplyr::rename(ctr_proj_utc = utc)%>%
  left_join(time_gap,by=c("ctr_duty_iso3c"="iso3c"))%>%
  dplyr::rename(ctr_duty_utc = utc,
                projectid = PROJ_ID ) %>%
  #the definition of field presence: exclude not in DC not nearby (+-3UTC)
  mutate(pm = ifelse(rgn_duty != "HQ" & 
                         (rgn_duty == rgn_proj | abs(ctr_proj_utc-ctr_duty_utc)<=3),
                     TRUE,FALSE)) %>%
  distinct(projectid,pm)

skim(pm_pre)
save(pm_pre,file = "data/pm_pre.rda")
  

# IEG Rating --------------------------------------------------------------
#IEG Rating data (need the governmence performance rating.)
var_rate <- c("IEG_Outcome", "IEG_BankQualityAtEntry","IEG_BankQualityOfSupervision","IEG_OverallBankPerf","IEG_OverallBorrPerf","IEG_GovernmentPerf","IEG_ImplementingAgencyPerf","IEG_MEQuality","IEG_ICRQuality")
rate_levels<- c("HIGHLY UNSATISFACTORY","UNSATISFACTORY",
                "MODERATELY UNSATISFACTORY","MODERATELY SATISFACTORY",
                "SATISFACTORY","HIGHLY SATISFACTORY") #covert the rating to factor with ordered levels. 
cvt_lvls <- function(x) {
  x <- factor(x, levels = rate_levels)}

rating <- read.csv("input/IEG_World_Bank_Project_Performance_Ratings.csv",header = TRUE) %>%
  mutate_at(vars(var_rate),cvt_lvls) %>%
  dplyr::rename(rate_bankpfr = IEG_OverallBankPerf,
                rate_entry = IEG_BankQualityAtEntry,
                rate_superv = IEG_BankQualityOfSupervision,
                rate_govpfr = IEG_GovernmentPerf)

rating %>%   #there's duplicating in Rating for project, exclude the duplicated IEG_EvalDate,IEG_EvalType
  select(Project.ID,IEG_EvalDate,IEG_EvalType)%>%
  anyDuplicated()

perf_vars <- c("rate_bankpfr","rate_entry","rate_superv","rate_govpfr")

rating_2 <- rating %>%   #exclude the duplicated case (latest time and exclude "PAR")
  mutate(IEG_EvalDate= as.Date(rating$IEG_EvalDate, "%m/%d/%y"))%>%
  filter_at(vars(perf_vars),any_vars(!is.na(.))) %>%   #drop if all the rating is missing. 
  filter_at(vars(perf_vars),all_vars(!grepl("NOT",.))) %>%  #drop the rating is as "not applicable" or "not rated" or missing                  
  group_by(Project.ID) %>%
  top_n(1,IEG_EvalDate) %>%
  top_n(1,IEG_EvalFY) %>%
  filter(!(IEG_EvalType=="PAR" & n() >=2)) %>%
  mutate(join_id = as.numeric(sub(".","",Project.ID))) %>%
  select(perf_vars,Project.ID,IEG_EvalFY,join_id) %>%
  ungroup

success_rate <- function(rate) {
  ifelse(grepl("UNSATIS",rate,ignore.case = TRUE),0,1)
}

save(rating_2,file = "data/rating_2.rda")  #get the quantitative measure.

rating_4 <- rating_2 %>%  # get the measurement of performance
  #dummy of success rate
  mutate_at(vars(perf_vars),.funs = list(md = ~success_rate(.)))%>%
  #find FYs with enough sample
  group_by(IEG_EvalFY) %>%
  filter(n()>1) %>%
  mutate_at(vars(perf_vars),as.numeric) %>%
  mutate_at(vars(perf_vars),percent_rank)%>%
  ungroup() %>%
  select(Project.ID,perf_vars,IEG_EvalFY,paste0(perf_vars,"_md")) 
skim(rating_4)
  
save(rating_4,file = "data/rating_4.rda")  #get the quantitative measure.
  

# control vars (ctr) ------------------------------------------------------------


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
         fy = as.character(fy),
         Code = countrycode(Country, 
                     origin = 'country.name',
                     destination = 'iso3c',
                     custom_match = c('KOSOVO'='KVO',
                                      'MICRONESIA, FS'= 'FSM',
                                      'YUGOSLAVIA, FR'= 'SRB',
                                      'SERBIA & MONTENEGRO' = 'SRB',
                                      "CENTRAL AFR. REP." = 'CAF'))) %>%
  filter(!is.na(Code))

cpia_raw%>% filter(is.na(iso3c))%>%count(Country)

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
  select(-Country)%>%
  mutate(fy = as.numeric(fy))%>% 
  pivot_wider( names_from = Code, values_from = Overall) %>%
  as.ts() %>% 
  na_interpolation(option = "linear")%>%
  as.data.frame() %>%
  pivot_longer(-fy,names_to = "code", values_to = "cpia") %>%
  rename(iso3c = code)   #all iso3 in historic data. 

count(cpia,iso3c) %>% summary() #Every country has 17 years recode. 
save(cpia, file="data/cpia.rda")   

#add other country level control (income, fcv, region)
ctr_tag <- read_excel("input/LENDING_PROGRAM_V2.xlsx", col_types = "text")%>%
  #import FCV, Income Group, Lending Group,country tag by fy.
  distinct(APPRVL_FY,FRAGILE_STATES_CODE,CNTRY_CODE,RGN_CODE,LNDNG_GRP_CODE,INC_GRP_CODE,CNTRY_SHORT_NAME) %>%
  dplyr::rename( appfy = APPRVL_FY,
         income = INC_GRP_CODE,
         ctr = CNTRY_SHORT_NAME,
         rgn_proj = RGN_CODE) %>%
  mutate(appfy = round(as.numeric(appfy),0),
         fcv = ifelse(FRAGILE_STATES_CODE == "YES", 1, 0),
         iso3c = countrycode(ctr, 
                             origin = 'country.name',
                             destination = 'iso3c',
                            custom_match = c('Kosovo'='KVO',
                                             'Micronesia'= 'FSM',
                                             'Yugoslavia'= 'SRB',
                                             'Serbia and Montenegro' = 'SRB'))) %>%  #the converged updated iso3c list
  filter(dplyr::between(appfy,2002,2018))%>% #the rated projects range.
  
  #add cpia data
  left_join(cpia,by=c("appfy"="fy","iso3c"="iso3c")) %>%
  select(appfy,iso3c,fcv, income, ctr, rgn_proj,cpia) %>%
  #make income level numeric
  mutate(income_n = case_when(income == "High" ~ 4,
                            income == "Upper Middle" ~ 3,
                            income == "Lower Middle" ~ 2,
                            income == "Low" ~ 1)) %>%
  #add the UTC offset
  left_join(time_gap, by = "iso3c")%>%
  dplyr::rename(ctr_proj_utc = utc)

skim(ctr_tag) 
save(ctr_tag,file = "ctr_tag.rda")

#add region impute the fy level income, fcv, cpia. 
ctr_tag_full <- ctr_tag %>%
  group_by(rgn_proj,appfy) %>%
  summarise_at(vars(income_n,fcv,cpia,ctr_proj_utc), mean, na.rm = TRUE) %>%
  ungroup() %>%
  select(appfy,rgn_proj,income_n,fcv,cpia,ctr_proj_utc) %>%
  right_join(ctr_tag[is.na(ctr_tag$cpia),c("appfy","iso3c","ctr","rgn_proj","income")], 
             by = c("appfy","rgn_proj")) %>%
  rbind(ctr_tag[!is.na(ctr_tag$cpia),])

save(ctr_tag_full,file = "data/ctr_tag_full.rda")
#project region tagged as "OTH" has no data.
skim(ctr_tag_full)  

ctr_tag_full %>%
filter(is.na(cpia)) %>%
  #only "World" Has no data for cpia/income 5 projects overall 
  count(ctr) 

#iso code (with historical Bank iso data)
iso <- ctr_tag %>% 
  distinct(iso3c,ctr)
save(iso, file = "data/iso.rda")


# Merge to master ---------------------------------------------------------
usa_utc <- time_gap[time_gap$iso3c=="USA","utc"] %>% as.numeric()

master <- 
  #project level control vars
  operation %>%
  select(projectid,practice,amount,COMPLEX_PROJ_IND,length,add_fin,appfy)%>%
  #field presence of irs, tech, co_ttl
  right_join(staff_dec, by = "projectid") %>%
  #field presence of pm
  left_join(pm_pre,by = "projectid")%>%
  #only rated projects in the analysis scope
  inner_join(rating_4, by = c("projectid" = "Project.ID")) %>%
  mutate(ctr_proj = ifelse(ctr_proj == "Egypt, Arab Rep","Egypt, Arab Republic of",ctr_proj))%>%
  #country level control variables (year variance matching with project approval yar)
  left_join(ctr_tag_full, by = c("ctr_proj"="ctr","appfy"="appfy")) %>%
  mutate(amount = as.numeric(amount),
         utc_dif = abs(ctr_proj_utc-usa_utc)) %>%
  #only compare within project with decentralized staff
  filter(dec == TRUE) %>%
  select(-ctr_proj_utc)

master %>% filter(is.na(cpia))%>%count(ctr_proj)

skim(master)
save(master,file = "data/master.rda")
  

  
