

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
         "steel blue") +
    labs(x = "",
         y = "",
         color ="",
         title = ("CAASPP ELA Rates Meeting or Exceeding by District"),
         caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 

ggsave(here("figs", paste0("All Districts ELA Rates Meeting or Exceeding ",  Sys.Date(),".png" )),
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


ggsave(here("figs", paste0("All Districts 11th grade ELA Rates Meeting or Exceeding",  Sys.Date(),".png" )),
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




caaspp.mry %>%
    filter(Grade == 13,
           is.na(District_Name),
           # Subgroup_ID == "1",
           Test_Id == 1, # ELA 
     #      Entity_Type == "District",
           !is.na(Percentage_Standard_Met_and_Above),
           !str_detect(Subgroup, " - ")
    ) %>%
    lollipop(Percentage_Standard_Met_and_Above,
             Subgroup,
             "steel blue") +
    labs(x = "",
         y = "",
         color ="",
         title = paste0("Monterey County ", "ELA" ," Rates Meeting or Exceeding by Student Group"),
         caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 


ggsave(here("figs", paste0("Monterey County ", "ELA" ," Rates Meeting or Exceeding by Student Group",  Sys.Date(),".png" )),
       width = 8, height = 6)





lolli.subgroups <- function(dist, test.id = 1) {

    test.name <- if_else(test.id == 1, "ELA", "Math")
    
caaspp.mry %>%
    filter(Grade == 13,
           str_detect(District_Name,dist),
           # Subgroup_ID == "1",
           Test_Id == test.id, # ELA 
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
         title = paste0("CAASPP ", test.name ," Rates Meeting or Exceeding at ",dist," by Student Group"),
         caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 


ggsave(here("figs", paste0(dist, " ", test.name,  " Rates by Student Group ",  Sys.Date(),".png" )),
       width = 8, height = 6)

}

lolli.subgroups("South Monterey County", 2)


lolli.subgroups("Mission", 2)


for (i in 1:2) {
    for (j in districts) {
        
        lolli.subgroups(j, i)
        
    }
    
    
}


lolli.schools <- function(dist, test.id = 1) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    
    caaspp.mry %>%
        filter(Grade == 13,
               str_detect(District_Name,dist),
               Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               Entity_Type == "School",
               !is.na(Percentage_Standard_Met_and_Above)
        )%>%
        lollipop(Percentage_Standard_Met_and_Above,
                 School_Name,
                 "sea green") +
        labs(x = "",
             y = "",
             color ="",
             title = paste0("CAASPP ", test.name ," Rates Meeting or Exceeding at ",dist," by School"),
             caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 
    

ggsave(here("figs", paste0(dist, " ", test.name,  " Rates by School ",  Sys.Date(),".png" )),
       width = 8, height = 6)

    
}

lolli.schools("Salinas City", 2)


lolli.schools("Alisal", 1)



for (i in 1:2) {
    for (j in districts) {
        
        lolli.schools(j, i)
        
    }
    
    
}




###  Slopegraph ------


caaspp.mry2019 <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
           # DistrictCode == "10272",
           Test_Year == "2019") %>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent2) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))



caaspp.long <- caaspp.mry %>%
    bind_rows(caaspp.mry2019) %>%
    filter(Grade == 13,
                      Subgroup_ID == "1",
                      Test_Id == 2, # ELA 
                      School_Code == "0000000",
                      # !is.na(Percentage_Standard_Met_and_Above)
    )



ggplot(data = caaspp.long, aes(x = Test_Year, y = Percentage_Standard_Met_and_Above, group = District_Name)) +
    geom_line(aes(color = District_Name, alpha = 1), size = 1) +
    geom_text_repel(data = caaspp.long %>% filter(Test_Year == "2019"), 
                    aes(label = District_Name) , 
                    hjust = "left", 
                    segment.size = .2,
                    segment.color = "grey",
                    size = 3, 
                    nudge_x = -.4, 
                    direction = "y") +
    geom_text_repel(data = caaspp.long %>% filter(Test_Year == "2022"), 
                    aes(label = District_Name) , 
                    hjust = "right", 
                    segment.size = .2,
                    segment.color = "grey",
                    fontface = "bold", 
                    size = 3, 
                    nudge_x = .4, 
                    direction = "y") +
    geom_label(aes(label = Percentage_Standard_Met_and_Above), 
               size = 2.5, 
               label.padding = unit(0.05, "lines"), 
               label.size = 0.0) +
    theme_hc() +  # Remove the legend
    theme(axis.text.y      = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    scale_x_discrete(position = "top") +
    theme(legend.position = "none") +
    labs(title = "Most districts decreased\n from 18-19 to 21-22 on Math",
         y = "Percent Meeting or Exceeding Standard by Grade",
         x = "")

### Overlapping bars ----


caaspp.long <- caaspp.mry %>%
    bind_rows(caaspp.mry2019) %>%
    filter(Grade == 13,
           Subgroup_ID == "1",
           Test_Id == 2, # Math 
           School_Code == "0000000",
           # !is.na(Percentage_Standard_Met_and_Above)
    )



caaspp.long2 <- caaspp.long %>%
    mutate(ranker = ifelse(Test_Year == "2022", Percentage_Standard_Met_and_Above, NA)) %>%
    filter(District_Name != "NA",
           District_Name != "Big Sur Unified")


    ggplot(mapping = aes(x = reorder(District_Name, Percentage_Standard_Met_and_Above),
                         y = Percentage_Standard_Met_and_Above/100)) +
        geom_col(data =  caaspp.long2[caaspp.long2$Test_Year == "2019",], 
                 fill = "light grey",
                 width = 0.75) +
        geom_col(data = caaspp.long2[caaspp.long2$Test_Year == "2022",], 
        #         position = "dodge" ,
                 width = 0.5,
                 fill = "sea green") + 
        coord_flip() + 
        mcoe_theme + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
 
        labs(title = "CAASPP Math Percent Meet and Exceed by District",
             subtitle = "Grey is 2019 and Green is 2022",
             y = "",
             x = "",
             caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 
        
    
    ggsave(here("figs", paste0("All Districts Math compared 2019 ",  Sys.Date(),".png" )),
           width = 8, height = 6)
    

    # ELA version
    
    
    caaspp.long <- caaspp.mry %>%
        bind_rows(caaspp.mry2019) %>%
        filter(Grade == 13,
               Subgroup_ID == "1",
               Test_Id == 1, # ELA 
               School_Code == "0000000",
               # !is.na(Percentage_Standard_Met_and_Above)
        )
    
    
    
    caaspp.long2 <- caaspp.long %>%
        mutate(ranker = ifelse(Test_Year == "2022", Percentage_Standard_Met_and_Above, NA)) %>%
        filter(District_Name != "NA",
               District_Name != "Big Sur Unified")
    
    
    ggplot(mapping = aes(x = reorder(District_Name, Percentage_Standard_Met_and_Above),
                         y = Percentage_Standard_Met_and_Above/100)) +
        geom_col(data =  caaspp.long2[caaspp.long2$Test_Year == "2019",], 
                 fill = "light grey",
                 width = 0.75) +
        geom_col(data = caaspp.long2[caaspp.long2$Test_Year == "2022",], 
                 #         position = "dodge" ,
                 width = 0.5,
                 fill = "steel blue") + 
        coord_flip() + 
        mcoe_theme + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "CAASPP ELA Percent Meet and Exceed by District",
             subtitle = "Grey is 2019 and Blue is 2022",
             y = "",
             x = "",
             caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 
    
    
    ggsave(here("figs", paste0("All Districts ELA compared 2019 ",  Sys.Date(),".png" )),
           width = 8, height = 6)
    
    
    
    
compare.years <- function(df,collie, test.id = 1) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    df <- df %>%
        filter(Test_Id == test.id) #%>%
     #   mutate(coll = fct_reorder({{collie}}, desc(Percentage_Standard_Met_and_Above))) %>%
    
    
    ggplot(mapping = aes(x = reorder({{collie}}, Percentage_Standard_Met_and_Above),
                         y = Percentage_Standard_Met_and_Above/100)) +
        geom_col(data =  df[df$Test_Year == "2019",],  
                 fill = "light grey",
                 width = 0.75) +
        geom_col(data =  df[df$Test_Year == "2022",],
                 #         position = "dodge" ,
                 width = 0.5,
                 fill = "steel blue") +
         coord_flip() +
        mcoe_theme + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = paste0( "CAASPP ", test.name ," Rates Meeting or Exceeding"),
             subtitle = "Grey is 2019 and Blue is 2022",
             y = "",
             x = "",
             caption = "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac-preview.ets.org/caaspp/ResearchFileListSB") 
    
#    ggsave(here("figs", paste0(dist, " ", test.name,  " Rates by Student Group ",  Sys.Date(),".png" )),
#           width = 8, height = 6)
    
}

caaspp.county.comp <- tbl(con, "CAASPP") %>% 
    filter(# Type_ID == 5,
           Subgroup_ID == 1,
           Grade == 13,
           # County_Code == "27",
            District_Code == "00000",
           Test_Year %in% c("2019","2022")) %>%
    collect() %>%
    left_join(ent2)
    

caaspp.county.comp %>%
    clean.caaspp() %>%
    mutate(Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above)) %>%
    filter(County_Code != "00") %>%
    compare.years(County_Name, 1)



caaspp.long %>%
    compare.years(District_Name)
    


caaspp.suhsd %>%
    filter(Grade == 13,
           str_detect(District_Name,"Salinas Union"),
           School_Code == "0000000",
           !str_detect(Subgroup, "Not migrant"),  # missing in 2019 and so messes up order if not excluded
           !is.na(Percentage_Standard_Met_and_Above),
           !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
    ) %>%
    compare.years(Subgroup, 1)


temp <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
           District_Code == "66191",
           Test_Year %in% c("2019", "2022")
    )%>%
    collect() %>%
    clean.caaspp() %>%
    filter(Grade == 13,
 #          str_detect(District_Name,"Salinas Union"),
           School_Code == "0000000",
    #       !str_detect(Subgroup, "Not migrant"),  # missing in 2019 and so messes up order if not excluded
     #      !is.na(Percentage_Standard_Met_and_Above),
           !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
    ) 

temp  %>%
    group_by(Subgroup) %>%
    filter(!is.na(Percentage_Standard_Met_and_Above)) %>%
    add_count() %>%
    filter(n>=4) %>%
    compare.years(Subgroup, 1)
