

# SoMoCo Comparisons 


caaspp.mry %>%
    filter(Entity_Type == "School",
           Grade == 11, 
           Subgroup == "All Students",
            str_detect(District_Name,"Salinas|Gonz|Soled"),
           Test_Id ==1
    )



somoco.comp.schools <- tribble(~which.comp,~cds, ~kular,
                               "Greenfield","27660682730174", "#5e043b",
                               "King City",  "27660682732170","#5e043b",
                         "Greenfield", "44697990105858", "gray60",
                         "Greenfield","36677103633302","gray60",
                         "Greenfield", "15739081534155","gray60",
                         "Greenfield",  "19645191995661","gray60",
                         "Greenfield",  "30664313030228","gray60",
                         "Greenfield",  "19647330100800","gray60",
                         "Greenfield",   "27661592730109","gray60",
                         "Greenfield", "19647330132126","gray60",
                         "Greenfield",  "13101320134379","gray60",
                         "Greenfield", "44697994437901","gray60",
                         "Greenfield", "15635291530252","gray60",
                         "King City", "27738252730034","gray60",
                         "King City",  "10624301036672","gray60",
                         "King City",  "20652432035707","gray60",
                         "King City",   "28662412831758","gray60",
                         "King City",    "19642791930528","gray60",
                         "King City",    "56725465631742","gray60",
                         "King City",    "27661590136697","gray60",
                         "King City",    "15635291535087","gray60",
                         "King City",    "56725465630389","gray60",
                         "King City",    "54767945436282","gray60",
                         "King City",    "36676523632205","gray60",
                         
                         "Both","27660682730174", "#5e043b",
                         "Both",  "27660682732170","#5e043b",
                         "Both" , "27754402730190","gray60",
                         "Both" , "27661590136697","gray60",
                         "Both" , "27661592730109","gray60",
                         "Both" , "27661592730166","gray60",
                         "Both" , "27754732730885","gray60",
                         "Both" ,  "27661592730273","gray60",
                         "Both" , "27661592733178","gray60",
                         "Both" ,  "27661592734556","gray60",
                         
                         
                         ) %>%
    mutate(County_Code = str_sub(cds,1,2),
           District_Code = str_sub(cds,3,7),
           School_Code = str_sub(cds,8))

school.code.list <- somoco.comp.schools$School_Code


caaspp.comp <- tbl(con, "CAASPP") %>% 
    filter(#County_Code %in% c("27","00"),
           # DistrictCode == "10272",
        School_Code %in% school.code.list,
           Test_Year %in% c("2021", yr.prior ,yr.curr),
        Grade == 11
      #     Type_ID %in% c(3,4,5,6)
    )%>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent2) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))



caaspp.comp <- caaspp.comp %>%
    left_join(somoco.comp.schools) %>%
    mutate(school_kular = paste0("<span style=\"color: ", kular, "\">", School_Name, "</span>"))





lolli.somoco <- function(test.id = 1, site) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    
    caaspp.comp %>%
        filter(Grade == 11,
           str_detect(which.comp,site),
               Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               Entity_Type == "School",
               !is.na(Percentage_Standard_Met_and_Above),
           Test_Year == max(Test_Year)
        ) %>%

                ggplot( aes( y = Percentage_Standard_Met_and_Above/100, x =fct_reorder(school_kular, Percentage_Standard_Met_and_Above/100) ,  label = percent( Percentage_Standard_Met_and_Above/100, accuracy = .1) )) +
        geom_segment( aes(x=fct_reorder(school_kular,Percentage_Standard_Met_and_Above/100), xend=fct_reorder(school_kular, Percentage_Standard_Met_and_Above/100), y=0, yend=Percentage_Standard_Met_and_Above/100,
                          color=kular),
                      size =2 ) +
        geom_point( aes(color=kular), size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black", nudge_y = .005) +
        scale_color_identity()+
        theme_hc() +
        mcoe_theme +
        theme(axis.text.y = element_markdown()
        ) +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        )  +
        labs(x = "",
             y = "",
             color ="",
             title = paste0(yr.curr," CAASPP ", test.name ," Rates Meeting or Exceeding Standards"),
             subtitle = paste0(site," High schools comparison"),
             caption = "Source: https://caaspp-elpac.ets.org/"
        ) 
    
     ggsave(here("figs", "somoco", paste0(site, " ", test.name,  " School Comparison",  Sys.Date(),".png" )),
            width = 8, height = 4.5)
    
    
}


for (i in 1:2) {
    for (j in c("Both","King City", "Greenfield")) {
        
        lolli.somoco(i, j)
        
    }
}


lolli.somoco(test.id = 1, site = "King City")

lolli.somoco(test.id = 1, site = "Greenfield")

lolli.somoco(test.id = 2, site = "King City")

lolli.somoco(test.id = 2, site = "Greenfield")


lolli.somoco(test.id = 2, site = "Both")




for (i in c("King City", "Greenfield")) {
    for (j in 1:2) {
        lolli.subgroups.school("South Monterey", i, j, "#850654") 
    }
}




compare.years.dist.group(dist = "South Monterey County", testy = 2, kular = "DarkMagenta")





### Making formulas ------

udp.comp <- udp.with.perc %>%
    filter(# district_type == "High School District",
        high_grade == "8",
        el.perc >= .40,
        frpm.perc >= .85,
        charter_school_y_n == "N",
        str_detect(school_type,"Public"),
        total_enrollment <= 200
    )

udp.comp <- udp.comp %>%
    transmute(which.comp = "San Lucas",
           kular = if_else(str_detect(district_name, "San Lucas"),"#800080" ,"gray60"),
           County_Code = county_code,
           District_Code = district_code,
           School_Code = school_code,
           School_Name = school_name)


lolli.comp <- function(df, test.id = 1, site) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    
    df %>%
        filter(Grade == 13,
               str_detect(which.comp,site),
               Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               Entity_Type == "School",
               !is.na(Percentage_Standard_Met_and_Above),
               Test_Year == max(Test_Year)
        ) %>%
        
        ggplot( aes( y = Percentage_Standard_Met_and_Above/100, x =fct_reorder(school_kular, Percentage_Standard_Met_and_Above/100) ,  label = percent( Percentage_Standard_Met_and_Above/100, accuracy = .1) )) +
        geom_segment( aes(x=fct_reorder(school_kular,Percentage_Standard_Met_and_Above/100), xend=fct_reorder(school_kular, Percentage_Standard_Met_and_Above/100), y=0, yend=Percentage_Standard_Met_and_Above/100,
                          color=kular),
                      size =2 ) +
        geom_point( aes(color=kular), size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black", nudge_y = .005) +
        scale_color_identity()+
        theme_hc() +
        mcoe_theme +
        theme(axis.text.y = element_markdown()
        ) +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(c(0.1, 0.1))
        )  +
        labs(x = "",
             y = "",
             color ="",
             title = paste0(yr.curr," CAASPP ", test.name ," Rates Meeting or Exceeding Standards"),
             subtitle = paste0(site," schools comparison"),
             caption = "Source: https://caaspp-elpac.ets.org/"
        ) 
    
    ggsave(here("figs", paste0(site, " ", test.name,  " School Comparison",  Sys.Date(),".png" )),
           width = 8, height = 4.5)
    
    
}


school.comp <- function(df, skul) {


school.code.list <- df$School_Code


caaspp.comp <- tbl(con, "CAASPP") %>% 
    filter(School_Code %in% school.code.list,
        Test_Year %in% c(yr.curr),
        Subgroup_ID == 1,  # Only for all students for now
        Grade == 13 # Change if only high school
    )%>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent2) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric)) 


caaspp.comp <- caaspp.comp %>%
    left_join(df) %>%
    mutate(school_kular = paste0("<span style=\"color: ", kular, "\">", School_Name, "</span>"))


lolli.comp(caaspp.comp, 1, skul)
lolli.comp(caaspp.comp, 2, skul)

}

school.comp(udp.comp, "San Lucas")
