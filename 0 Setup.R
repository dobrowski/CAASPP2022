

# Load libraries

library(tidyverse)
library(janitor)
library(MCOE)
library(here)
library(ggthemes)
library(vroom)
library(ggrepel)


con <- mcoe_sql_con()

ent <- read_delim(here("data","sb_ca2022entities_csv.txt"), delim = "^")


ent <- vroom(here("data","sb_ca2022entities_csv.txt"),
             .name_repair = ~ janitor::make_clean_names(., case = "none"),
             delim = "^"
)


ent2 <- ent %>%
    select(-Test_Year, -Type_ID)


districts <- c("Carmel",
               "Pacific Grove",
               "Graves",
               "Washington",
               "Lagunita",
               "Spreckels",
               "South Monterey",
               "Chualar",
               "Mission",
               "San Antonio",
               "Santa Rita",
               "Salinas Union",
               "Alisal",
               "Monterey Peninsula",
               "North Monterey",
               "San Lucas",
               "Salinas City",
               "Soledad",
               "King City",
               "Monterey County Office of Ed",
               "Gonzales",
               "Greenfield",
               "Bradley",
               "San Ardo"
               
)


caaspp.mry <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
           # DistrictCode == "10272",
           Test_Year >= "2022") %>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent2) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))

clean.caaspp <- function(df) {
    df %>%
        mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
        left_join_codebook("CAASPP", "Subgroup_ID") %>%
        rename(Subgroup = definition) %>%
        left_join(ent2) %>%
        mutate(Type_ID = as.character(Type_ID)) %>%
        left_join_codebook("CAASPP", "Type_ID") %>%
        rename(Entity_Type = definition) %>%
        mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))
    
}

caaspp.suhsd <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
            District_Code == "66159",
           Test_Year %in% c("2019", "2022")
           )%>%
    collect() %>%
    clean.caaspp()



udp <- tbl(con, "upc") %>% 
    filter(# County_Code == "27",
        # DistrictCode == "10272",
        academic_year == "2021-2022"
    ) %>%
    #    head() %>%
    collect() 




caaspp.wide <- function(df, namer) {
    df %>%
        pivot_wider(id_cols = {{namer}},
                    names_from = Test_Id,
                    values_from = Percentage_Standard_Met_and_Above) %>%
        rename(ELA = `1`,
               Math = `2`)
    
}


