



udp.with.perc <- udp %>%
    mutate(el.perc = english_learner_el/total_enrollment,
           frpm.perc = unduplicated_frpm_eligible_count/total_enrollment,
           )


SoMoCo <- udp.with.perc %>% 
    filter(str_detect(district_name, "South Monterey"))


udp.comp <- udp.with.perc %>%
    filter(district_type == "High School District",
           high_grade == "12",
           el.perc >= .22,
           frpm.perc >= .80,
           charter_school_y_n == "N",
           str_detect(school_type,"Public"),
           total_enrollment >= 1000
           )





caaspp.somoco.comp <- tbl(con, "CAASPP") %>% 
    filter(#County_Code == "27",
           # DistrictCode == "10272",
           Test_Year >= "2022",
           Grade == 11,
           Subgroup_ID == "1",
           Type_ID == "7") %>%
    collect() %>%
    select(county_code = County_Code,
           district_code = District_Code,
           school_code = School_Code,
           Test_Id,
           Percentage_Standard_Met_and_Above) %>%
    mutate(Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above)) %>%
    pivot_wider(id_cols = c(county_code,district_code,school_code),
                names_from = Test_Id,
                values_from = Percentage_Standard_Met_and_Above) %>%
    rename(ELA = `1`,
           Math = `2`)

joint <- left_join(udp.comp, caaspp.somoco.comp) %>%
    filter(ELA > 60,
           Math > 15) %>%
    select(county_name:school_name, total_enrollment, el.perc:Math) %>%
    mutate(el.perc = round2(el.perc*100,1),
           frpm.perc = round2(frpm.perc*100,1),
           ELA = round2(ELA,1),
           Math = round2(Math,1))
