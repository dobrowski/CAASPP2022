

# Load libraries

library(tidyverse)
library(janitor)
library(MCOE)
library(here)
library(ggthemes)
library(vroom)


con <- mcoe_sql_con()

ent <- read_delim(here("data","sb_ca2022entities_csv.txt"), delim = "^")


ent <- vroom(here("data","sb_ca2022entities_csv.txt"),
      .name_repair = ~ janitor::make_clean_names(., case = "none"),
      delim = "^"
)


caaspp.mry <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
        # DistrictCode == "10272",
        Test_Year >= "2022") %>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))

### Graph Functions


# All Districts All Students 

caaspp.mry %>%
    filter(Grade == 13,
           Subgroup_ID == "1",
           Test_Id == 1, # ELA 
           Entity_Type == "District",
           !is.na(Percentage_Standard_Met_and_Above)
    ) %>%
lollipop(Percentage_Standard_Met_and_Above,
         District_Name,
         "sea green") +
    labs(x = "",
         y = "",
         color ="",
         title = ("CAASPP ELA Rates Meeting or Exceeding by District"),
         caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 

ggsave(here("figs", paste0("CAASPP ELA Rates Meeting or Exceeding by District",  Sys.Date(),".png" )),
       width = 8, height = 6)


#  11th Grade ELA 

caaspp.mry %>%
    filter(Grade == 11,
           Subgroup_ID == "1",
           Test_Id == 1, # ELA 
           Entity_Type == "School",
           !is.na(Percentage_Standard_Met_and_Above)
           )%>%
    lollipop(Percentage_Standard_Met_and_Above,
             School_Name,
             "sea green") +
    labs(x = "",
         y = "",
         color ="",
         title = ("CAASPP ELA Rates Meeting or Exceeding by 11th grade"),
         caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 


ggsave(here("figs", paste0("CAASPP ELA Rates Meeting or Exceeding by 11th grade",  Sys.Date(),".png" )),
       width = 8, height = 6)

# Student Groups at Salinas Union

caaspp.mry %>%
    filter(Grade == 13,
           str_detect(District_Name,"Salinas Union"),
          # Subgroup_ID == "1",
           Test_Id == 1, # ELA 
           Entity_Type == "District",
           !is.na(Percentage_Standard_Met_and_Above),
          !str_detect(Subgroup, " - ")
    ) %>%
    lollipop(Percentage_Standard_Met_and_Above,
             Subgroup,
             "sea green") +
    labs(x = "",
         y = "",
         color ="",
         title = ("CAASPP ELA Rates Meeting or Exceeding at Salinas Union by Student Group"),
         caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 


ggsave(here("figs", paste0("CAASPP ELA Rates Meeting or Exceeding at Salinas Union by Student Group",  Sys.Date(),".png" )),
       width = 8, height = 6)

