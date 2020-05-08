library(tidyverse)

#data inqury: Albania teammember in FY19 active projects.  

result_wb_19 <- ops_trs_team %>%
  filter(countryname == "Albania", #P128416 is not in the Albania list. 
         fy == "2019",
         revclosing_yr > "2019",
         appfy <= "2019") %>%
  arrange(Proj.ID,Employee)  %>%
  group_by(Proj.ID,Employee,`Title Name`) %>%
  select(Employee,Proj.ID,`Title Name`,`Grade (Curr)`,
         countryname, ttl_name, ttl_upi,
         `Duty Country Name`, Employee, Employee.Name,
         ROLE_CODE,Gross.Staff.weeks,Normalized.Staff.weeks,Actual.Cost,
         projectname,practice,appfy,inst,revclosing_yr) %>%
  aggregate(cbind(Gross.Staff.weeks,Normalized.Staff.weeks)~.,.,sum) %>%
  select(Proj.ID,
         projectname,
         Employee,
         Employee.Name,
         `Title Name`,
         `Grade (Curr)`,
         `Duty Country Name`,
         everything() ) %>%
  select(-Actual.Cost,everything()) %>%
  rename(Project_ID = Proj.ID,
         Employee_Name = Employee.Name,
         Practice = practice,
         Approval_FY = appfy,
         Instrument = inst,
         Project_Name = projectname,
         Country_Nmae = countryname,
         Title_Name = `Title Name`,
         Team_Role = ROLE_CODE)

write_csv(result_wb_19, "output/Albania_19.csv")
 #P162786 P163239 P162079 P159520 P125856 P166469 original missing, stil misisng, due to additional financing. 

#data inqury: Albania TTL works for other country project. 
ttl_diverse <- ops_trs_team %>%
  filter(countryname == "Albania",
         fy == "2019",
         revclosing_yr > "2019",
         appfy <= "2019",
         ROLE_CODE == "TEAMLEAD" ) %>%
  distinct(Employee) %>%
  left_join(.,ops_trs_team,by = "Employee") %>% 
  filter(countryname != "Albania",
         fy == "2019",
         revclosing_yr > "2019",
         appfy <= "2019") %>%
  distinct(Employee,Employee.Name,`Duty Country Name`,Proj.ID,countryname,
         ROLE_CODE,fy,Gross.Staff.weeks,Actual.Cost) %>%
  rename(Project.Country = countryname)

write_csv(ttl_diverse, "output/Albania_19_ttl_diverse.csv")

#people in albania work on other ctr + IRS(HR) + ttl/cottl + tilte + procurement/safeguard considered.
ttl_diverse_from <- ops_trs_team %>%
  filter(`Duty Country Name` == "Albania",
         fy == "2019") %>%
  distinct(Employee) %>%
  left_join(.,ops_trs_team,by = "Employee") %>% 
  filter(countryname != "Albania",
         fy == "2019",
         revclosing_yr > "2019",
         appfy <= "2019") %>%
  mutate(cottl1_upi = ifelse(is.na(cottl1_upi),0,cottl1_upi),
         cottl2_upi = ifelse(is.na(cottl2_upi),0,cottl2_upi),
         CoTTL = ifelse(as.numeric(cottl1_upi) == Employee | as.numeric(cottl2_upi) == Employee,1,0)) %>%
  distinct(Employee,Employee.Name,`Duty Country Name`,`IRS Flag`,
           `Title Name`,`Proj.ID`,countryname,
           ROLE_CODE,fy,Gross.Staff.weeks,Actual.Cost,
           CoTTL) %>%
  mutate(dummy = 1) %>%
  spread(key = ROLE_CODE,
         value = dummy, 
         fil = 0) %>%
  select(-`<NA>`,
         - MEMBER) %>%
  rename(Project.Country = countryname)

write_csv(ttl_diverse_from, "output/From_Albania_19_ttl_diverse.csv")

#people in albania work on other ctr + IRS(HR) + ttl/cottl + tilte + procurement/safeguard considered.
ttl_diverse_to <- ops_trs_team %>%
  filter(`Duty Country Name` != "Albania", #those ont based in Albania
         fy == "2019") %>%
  distinct(Employee) %>%
  left_join(.,ops_trs_team,by = "Employee") %>% 
  filter(countryname == "Albania",   #but project country in Albania
         fy == "2019",
         revclosing_yr > "2019",
         appfy <= "2019") %>%
  mutate(cottl1_upi = ifelse(is.na(cottl1_upi),0,cottl1_upi),
         cottl2_upi = ifelse(is.na(cottl2_upi),0,cottl2_upi),
         CoTTL = ifelse(as.numeric(cottl1_upi) == Employee | as.numeric(cottl2_upi) == Employee,1,0)) %>%
  distinct(Employee,Employee.Name,`Duty Country Name`,`IRS Flag`,
           `Title Name`,`Proj.ID`,countryname,
           ROLE_CODE,fy,Gross.Staff.weeks,Actual.Cost,
           CoTTL) %>%
  mutate(dummy = 1) %>%
  spread(key = ROLE_CODE,
         value = dummy, 
         fil = 0) %>%
  select(-`<NA>`,
         - MEMBER) %>%
  rename(Project.Country = countryname) 

write_csv(ttl_diverse_to, "output/To_Albania_19_ttl_diverse.csv")
