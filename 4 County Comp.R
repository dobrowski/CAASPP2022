


udp <- tbl(con, "upc") %>% 
    filter(# County_Code == "27",
        # DistrictCode == "10272",
        academic_year == yr.acad
    ) %>%
    #    head() %>%
    collect() 




udp.county <- udp %>%
    group_by(county_name) %>%
    summarise(across(c(english_learner_el,unduplicated_frpm_eligible_count,total_enrollment) ,
                     sum)) %>%
    mutate(el.perc = round2(100*english_learner_el/total_enrollment,1),
           frpm.perc = round2(100*unduplicated_frpm_eligible_count/total_enrollment,1)
    )


caaspp.county <- tbl(con, "CAASPP") %>% 
    filter(Type_ID == 5,
           Subgroup_ID == 1,
           Grade == 13,
        # County_Code == "27",
           # DistrictCode == "10272",
           Test_Year >= yr.curr) %>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric)) %>%
    select(county_name = County_Name, Percentage_Standard_Met_and_Above, Test_Id) %>%
    pivot_wider(id_cols = county_name,
                names_from = Test_Id,
                values_from = Percentage_Standard_Met_and_Above) %>%
    rename(ELA = `1`,
           Math = `2`) %>%
    left_join(udp.county) %>%
    arrange(desc(Math)) %>%
    mutate(Rank = row_number())
    
write_rds( caaspp.county, here("data","county-comp.rds"))
