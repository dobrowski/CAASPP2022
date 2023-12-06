

### Graph Functions ------


# All Districts All Students 

logo <- mcoe_logo()
    
source.link <- "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac.ets.org/"    


# # png(here("figs", paste0("All Districts ELA Rates Meeting or Exceeding ",  Sys.Date(),".png" )),
# #     width = 600, height = 400)
# caaspp.mry %>%
#     filter(Grade == 13,
#            Subgroup_ID == "1",
#            Test_Id == 2, # ELA 
#            Entity_Type == "District",
#            !is.na(Percentage_Standard_Met_and_Above)
#     ) %>%
# lollipop(Percentage_Standard_Met_and_Above,
#          District_Name,
#          "steel blue") +
#     labs(x = "",
#          y = "",
#          color ="",
#          title = ("CAASPP 2023 Math Rates Meeting or Exceeding by District"),
#          caption = source.link) 
# # grid::grid.raster(logo, x = 0.03, y = 0.03, just = c('left', 'bottom'), width = unit(.75, 'inches'))
# # dev.off()
# 
# 
# 
# ggsave(here("figs", paste0("All Districts Math Rates Meeting or Exceeding ",  Sys.Date(),".png" )),
#        width = 8, height = 6)
# 



county.graph <- function(test.id) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    caaspp.mry %>%
        filter(Grade == 13,
               Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               Entity_Type == "District",
               !is.na(Percentage_Standard_Met_and_Above)
        ) %>%
        lollipop(Percentage_Standard_Met_and_Above,
                 District_Name,
                 "steel blue") +
        labs(x = "",
             y = "",
             color ="",
             title = paste0("CAASPP ", yr.curr ," ", test.name, " Rates Meeting or Exceeding Standards by District"),
             caption = source.link
             ) 
    grid::grid.raster(logo, x = 0.03, y = 0.03, just = c('left', 'bottom'), width = unit(.75, 'inches'))
    # dev.off()
    
    
    
    ggsave(here("figs", paste0("All Districts ", test.name, " Rates Meeting or Exceeding ",  Sys.Date(),".png" )),
           width = 8, height = 6)
}


county.graph(1)

county.graph(2)



county.alpha <- function(test.id, colorme) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    caaspp.mry %>%
        filter(Grade == 13,
               Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               Entity_Type == "District",
               !is.na(Percentage_Standard_Met_and_Above)
        ) %>%
        ggplot2::ggplot( aes( y = Percentage_Standard_Met_and_Above/100,
                                 x = reorder(District_Name, desc(District_Name)), #forcats::fct_reorder(District_Name,Percentage_Standard_Met_and_Above) ,
                                 label = scales::percent(Percentage_Standard_Met_and_Above/100, accuracy = .1))) +
        geom_segment( aes(x= reorder(District_Name, desc(District_Name)), #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
                          xend= reorder(District_Name, desc(District_Name)), #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
                          y=0,
                          yend=Percentage_Standard_Met_and_Above/100),
                      color=colorme,
                      size =2 ) +
        geom_point( color=colorme, size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
        theme_hc() +
        mcoe_theme +
        labs(x = "",
             y = "",
             color ="",
             title = paste0("CAASPP ", yr.curr, " ", test.name, " Rates Meeting or Exceeding Standards by District"),
             caption = source.link
        ) 
    grid::grid.raster(logo, x = 0.03, y = 0.03, just = c('left', 'bottom'), width = unit(.75, 'inches'))
    # dev.off()
    
    
    
    ggsave(here("figs", paste0("All Districts ", test.name, " Rates Meeting or Exceeding - Alpha ",  Sys.Date(),".png" )),
           width = 8, height = 6)
}



county.alpha(1, colorme =  "steel blue")
county.alpha(2, colorme =  "steel blue")





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
         caption = source.link
    ) 

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
         caption = source.link
    ) 

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
         title = paste0("Monterey County ", "ELA" ," Rates Meeting or Exceeding Standards by Student Group"),
         caption = source.link
    ) 

ggsave(here("figs", paste0("Monterey County ", "ELA" ," Rates Meeting or Exceeding by Student Group",  Sys.Date(),".png" )),
       width = 8, height = 6)


### Subgroups by District ------


lolli.subgroups <- function(dist = "", test.id = 1) {

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
    mutate(subgroup.n = paste0(Subgroup," (",Total_Tested_At_Entity_Level,")" )) %>%
    lollipop(Percentage_Standard_Met_and_Above,
             subgroup.n,
             "sea green") + 
    labs(x = "",
         y = "",
         color ="",
         title = paste0("CAASPP ", yr.curr, " ", test.name ," Rates Meeting or Exceeding Standards at \n",dist," by Student Group"),
         caption = source.link
    ) 

ggsave(here("figs", paste0(dist, " ", test.name,  " Rates by Student Group ",  Sys.Date(),".png" )),
       width = 8, height = 6)

}

lolli.subgroups("South Monterey County", 2)

lolli.subgroups("South Monterey County", 1)

lolli.subgroups("Soledad", 2)





districts<- "Greenfield"

for (i in 1:2) {
    for (j in districts) {
        
        lolli.subgroups(j, i)
        
    }
    
    
}

### Subgroups countywide ------


caaspp.mry %>%
    filter(Grade == 13,
           is.na(District_Name),
           
           # Subgroup_ID == "1",
           Test_Id == 1, # ELA 
 #          Entity_Type == "District",
           !is.na(Percentage_Standard_Met_and_Above),
           !str_detect(Subgroup, " - ")
    ) %>%
    lollipop(Percentage_Standard_Met_and_Above,
             Subgroup,
             "sea green") +
    labs(x = "",
         y = "",
         color ="",
         title = paste0("CAASPP ", "ELA" ," Rates Meeting or Exceeding Standards for Monterey County by Student Group"),
         caption = source.link
    ) 

ggsave(here("figs", paste0("Monterey County ", "ELA" ,  " Rates by Student Group ",  Sys.Date(),".png" )),
       width = 8, height = 6)




#### Subgroups in a School --------


lolli.subgroups.school <- function(dist = "", schoo = "", test.id = 1, kular = "sea green") {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    caaspp.mry %>%
        filter(Grade == 13,
               str_detect(District_Name,dist),
               str_detect(School_Name,schoo),
               
               # Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               Entity_Type == "School",
               !is.na(Percentage_Standard_Met_and_Above),
               !str_detect(Subgroup, " - "),
               !str_detect(Subgroup, "Declined")
        ) %>%
        mutate(subgroup.n = paste0(Subgroup," (",Total_Tested_At_Entity_Level,")" )) %>%
        lollipop(Percentage_Standard_Met_and_Above,
                 subgroup.n,
                 kular) + 
        # theme(panel.grid.major.x = element_line(color = "dark grey",
        #                                       size = 0.5,
        #                                       linetype = 1)) +
        labs(x = "",
             y = "",
             color ="",
             title = paste0("CAASPP ", yr.curr, " "  , test.name ," Rates Meeting or Exceeding Standards"),
             subtitle = paste0(dist, " - ",schoo," by Student Group"),
 #            caption = source.link
        ) 
    
    ggsave(here("figs", paste0(dist, "-",schoo, " ", test.name,  " Rates by Student Group ",  Sys.Date(),".png" )),
           width = 8, height = 4.5)
    
}


lolli.subgroups.school("Soledad", "Main St", 2)

lolli.subgroups.school("Soledad", "Soledad High", 2)

lolli.subgroups.school("Soledad", "Franscioni", 2)

lolli.subgroups.school("South Monterey", "Greenfield", 1)




lolli.subgroups.school("Santa Rita", "Santa Rita Elementary", 2)

lolli.subgroups.school("Santa Rita", "Gavilan View", 2)


lolli.subgroups.school("Monterey Peninsula", "Highland", 1)


lolli.subgroups.school("South Monterey", "Greenfield", 1)

lolli.subgroups.school("South Monterey", "Greenfield", 2)

lolli.subgroups.school("Salinas Union", "^Salinas High", 1)

lolli.subgroups.school("Salinas Union", "^Salinas High", 2)

caaspp.mry %>%
    filter(Grade == 13,
           str_detect(District_Name,"South Monterey County"),
           str_detect(School_Name,"King City"),
           
            Subgroup_ID %in% c("1","128", "160") ,
           Test_Id == 2, # ELA 
           Entity_Type == "School",
           !is.na(Percentage_Standard_Met_and_Above),
           !str_detect(Subgroup, " - "),
           !str_detect(Subgroup, "Declined")
    ) %>%
    mutate(subgroup.n = paste0(Subgroup," (",Total_Tested_At_Entity_Level,")" )) %>%
    lollipop(Percentage_Standard_Met_and_Above,
             subgroup.n,
             "sea green") +
    labs(x = "",
         y = "",
         color ="",
         title = paste0("CAASPP ", "Math" ," Rates Meeting or Exceeding Standards"),
         subtitle = paste0("South Monterey County", "-", "King City" ," by Student Group"),
         caption = source.link
    ) 

ggsave(here("figs", paste0("South Monterey County", "-", "King City ", " ", "Math",  " Rates by Student Group ",  Sys.Date(),".png" )),
       width = 4, height = 3)




caaspp.mry %>%
    filter(str_detect(District_Name,"Salinas Union")) %>%
    select(School_Name) %>%
    distinct()


schools <- c( "El Puente"                      ,          
              "Rancho San Juan High"            ,         
              "Alisal High"                      ,        
              "Everett Alvarez High"              ,       
     #         "Carr Lake Community Day"            ,      
              "North Salinas High"                  ,     
              "Mount Toro High"                      ,    
              "Salinas High"                          ,   
              "El Sausal Middle"                       ,  
              "Washington Middle"                       , 
              "Harden Middle"                            ,
              "La Paz Middle"
              )

for (i in schools) {
    for (j in 1:2) {
        lolli.subgroups.school("Salinas Union", i, j)
    }
}


### Charter Schools -------


lolli.subgroups.charter <- function(schoo = "", test.id = 1) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    caaspp.mry %>%
        filter(Grade == 13,
               str_detect(School_Name,schoo),
               
               # Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               Type_ID %in% c(9,10),
               !is.na(Percentage_Standard_Met_and_Above),
               !str_detect(Subgroup, " - "),
               !str_detect(Subgroup, "Declined")
        ) %>%
        mutate(subgroup.n = paste0(Subgroup," (",Total_Tested_At_Entity_Level,")" ))  %>%
        lollipop(Percentage_Standard_Met_and_Above,
                 subgroup.n,
                 "sea green") +
        theme(panel.grid.major.x = element_line(color = "dark grey",
                                                size = 0.5,
                                                linetype = 1)) +
        labs(x = "",
             y = "",
             color ="",
             title = paste0("CAASPP ", yr.curr ," ", test.name ," Rates Meeting or Exceeding Standards"),
             subtitle = paste0(schoo," by Student Group"),
             caption = source.link
        )

    ggsave(here("figs", paste0(schoo, " ", test.name,  " Rates by Student Group ",  Sys.Date(),".png" )),
           width = 8, height = 6)
    
}

lolli.subgroups.charter("Oasis", 1)


schools <- caaspp.mry %>%
    filter(Type_ID %in% c(9,10)) %>%
    select(School_Name) %>%
    distinct() %>%
    unlist()

for (i in schools) {
    for (j in 1:2) {
        lolli.subgroups.charter(i, j)
    }
}


#### Subgroups by Feeder schools ----


lolli.subgroups.school.feeder8 <- function(dist = "", schoo = "", test.id = 1) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    caaspp.mry2019 %>%
        filter(Grade == 8,
               str_detect(District_Name,dist),
               str_detect(School_Name,schoo),
               
               # Subgroup_ID == "1",
               Test_Id == test.id, # ELA 
               # Entity_Type == "School",
               !is.na(Percentage_Standard_Met_and_Above),
               !str_detect(Subgroup, " - "),
               !str_detect(Subgroup, "Declined")
        ) %>%
        mutate(subgroup.n = paste0(Subgroup," (",Total_Tested_At_Entity_Level,")" )) %>%
        lollipop(Percentage_Standard_Met_and_Above,
                 subgroup.n,
                 "sea green") +
        labs(x = "",
             y = "",
             color ="",
             title = paste0("CAASPP ", test.name ," Rates Meeting or Exceeding in 8th Grade 2019"),
             subtitle = paste0(dist, "-",schoo," by Student Group"),
             caption = source.link
        ) 
    
    ggsave(here("figs", paste0(dist, "-",schoo, " ", test.name,  " Rates by Student Group ",  Sys.Date(),".png" )),
           width = 8, height = 6)
    
}




lolli.subgroups.school.feeder8("King City", "Chalone Peaks", 1)

lolli.subgroups.school.feeder8("Greenfield", "Vista Verde" , 1)


lolli.subgroups.school.feeder8("King City", "Chalone Peaks", 2)

lolli.subgroups.school.feeder8("Greenfield", "Vista Verde" , 2)


### Schools in a District Comparison -------

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
             title = paste0("CAASPP ", test.name ," Rates Meeting or Exceeding Standards at ",dist," by School"),
             caption = source.link
        ) 

ggsave(here("figs", paste0(dist, " ", test.name,  " Rates by School ",  Sys.Date(),".png" )),
       width = 8, height = 6)

    
}

lolli.schools("Gonzales", 1)


lolli.schools("Greenfield", 1)



for (i in 1:2) {
    for (j in districts) {
        
        lolli.schools(j, i)
        
    }

}




###  Slopegraph ------


caaspp.mry.prior <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
           # DistrictCode == "10272",
           Test_Year == yr.prior) %>%
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
    bind_rows(caaspp.mry.prior) %>%
    filter(Grade == 13,
                      Subgroup_ID == "1",
                      Test_Id == 2, # ELA 
                      School_Code == "0000000",
                      # !is.na(Percentage_Standard_Met_and_Above)
    )



ggplot(data = caaspp.long, aes(x = Test_Year, y = Percentage_Standard_Met_and_Above, group = District_Name)) +
    geom_line(aes(color = District_Name, alpha = 1), size = 1) +
    geom_text_repel(data = caaspp.long %>% filter(Test_Year == "2022"), 
                    aes(label = District_Name) , 
                    hjust = "left", 
                    segment.size = .2,
                    segment.color = "grey",
                    size = 3, 
                    nudge_x = -.4, 
                    direction = "y") +
    geom_text_repel(data = caaspp.long %>% filter(Test_Year == yr.curr), 
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
    labs(title = "Most districts were flat\n from 21-22 to 22-23 on Math",
         y = "Percent Meeting or Exceeding Standard by Grade",
         x = "")

### Overlapping bars ----


caaspp.long <- caaspp.mry %>%
    bind_rows(caaspp.mry.prior) %>%
    filter(Grade == 13,
           Subgroup_ID == "1",
           Test_Id == 2, # Math 
           School_Code == "0000000",
           # !is.na(Percentage_Standard_Met_and_Above)
    )



caaspp.long2 <- caaspp.long %>%
    mutate(ranker = ifelse(Test_Year == yr.curr, Percentage_Standard_Met_and_Above, NA)) %>%
    filter(District_Name != "NA",
           District_Name != "Big Sur Unified")


    ggplot(mapping = aes(x = reorder(District_Name, Percentage_Standard_Met_and_Above),
                         y = Percentage_Standard_Met_and_Above/100)) +
        geom_col(data =  caaspp.long2[caaspp.long2$Test_Year == yr.prior,], 
                 fill = "light grey",
                 width = 0.75) +
        geom_col(data = caaspp.long2[caaspp.long2$Test_Year == yr.curr,], 
        #         position = "dodge" ,
                 width = 0.5,
                 fill = "sea green") + 
        coord_flip() + 
        mcoe_theme + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
 
        labs(title = "CAASPP Math Percent Meet and Exceed by District",
             subtitle = paste0("Grey is ",yr.prior," and Green is ", yr.curr),
             y = "",
             x = "",
             caption = source.link
        ) 
    
    ggsave(here("figs", paste0("All Districts Math compared 2022 ",  Sys.Date(),".png" )),
           width = 8, height = 6)
    

    # ELA version
    
    
    caaspp.long <- caaspp.mry %>%
        bind_rows(caaspp.mry.prior) %>%
        filter(Grade == 13,
               Subgroup_ID == "1",
               Test_Id == 1, # ELA 
               School_Code == "0000000",
               # !is.na(Percentage_Standard_Met_and_Above)
        )
    
    
    
    caaspp.long2 <- caaspp.long %>%
        mutate(ranker = ifelse(Test_Year == yr.curr, Percentage_Standard_Met_and_Above, NA)) %>%
        filter(District_Name != "NA",
               District_Name != "Big Sur Unified")
    
    
    ggplot(mapping = aes(x = reorder(District_Name, Percentage_Standard_Met_and_Above),
                         y = Percentage_Standard_Met_and_Above/100)) +
        geom_col(data =  caaspp.long2[caaspp.long2$Test_Year == yr.prior,], 
                 fill = "light grey",
                 width = 0.75) +
        geom_col(data = caaspp.long2[caaspp.long2$Test_Year == yr.curr,], 
                 #         position = "dodge" ,
                 width = 0.5,
                 fill = "steel blue") + 
        coord_flip() + 
        mcoe_theme + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "CAASPP ELA Percent Meet and Exceed by District",
             subtitle = paste0("Grey is ",yr.prior," and Blue is ",yr.curr),
             y = "",
             x = "",
             caption = source.link
        ) 
    
    ggsave(here("figs", paste0("All Districts ELA compared 2022 ",  Sys.Date(),".png" )),
           width = 8, height = 6)
    
    
    
    
compare.years <- function(df,collie, test.id = 1, title.name, kular = "steel blue") {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    df <- df %>%
        filter(Test_Id == test.id) #%>%
     #   mutate(coll = fct_reorder({{collie}}, desc(Percentage_Standard_Met_and_Above))) %>%
    

    ggplot(mapping = aes(x = reorder({{collie}}, Percentage_Standard_Met_and_Above),
                         y = Percentage_Standard_Met_and_Above/100)) +
        geom_col(data =  df[df$Test_Year == yr.prior,],  
                 fill = "light grey",
                 width = 0.75) +
        geom_col(data =  df[df$Test_Year == yr.curr,],
                 #         position = "dodge" ,
                 width = 0.5,
                 fill = kular) +
         coord_flip() +
        mcoe_theme + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = paste0(title.name, " CAASPP ", test.name ," Rates Meeting or Exceeding"),
             subtitle = paste0("Grey is ",yr.prior," and ",kular," is ",yr.curr),
             y = "",
             x = "",
             caption = source.link
        ) 
    
   ggsave(here("figs", paste0(title.name, " ", test.name,  " Change in Rates by Student Group ",  Sys.Date(),".png" )),
          width = 8, height = 4.5)
    
}

caaspp.county.comp <- tbl(con, "CAASPP") %>% 
    filter(# Type_ID == 5,
           Subgroup_ID == 1,
           Grade == 13,
           # County_Code == "27",
            District_Code == "00000",
           Test_Year %in% c(yr.prior,yr.curr)) %>%
    collect() %>%
    left_join(ent2) %>%
    clean.caaspp()
    

caaspp.county.comp  %>%
    mutate(Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above)) %>%
    filter(County_Code != "00") %>%
    compare.years(County_Name, 2, "Counties")



caaspp.long2 %>%
    compare.years(District_Name, 1, "Districts")

###

caaspp.mry %>%
    bind_rows(caaspp.mry.prior) %>%
    filter(Grade == 13,
          # Subgroup_ID == "1",
          Subgroup_ID %notin% c(190, 200, 201, 202, 203, 204, 205, 206, 207, 220, 221, 222, 223, 224, 225, 226, 227, 250, 251, 252),  # Removing the 250-252 because did not exist in 2022
          Entity_Type == "County",
          !is.na(Percentage_Standard_Met_and_Above)
    ) %>%
    compare.years(Subgroup, 2, "County Student Groups")



###


compare.years.dist.group <- function(dist, testy, kular) {
    

dist.hold <- caaspp.mry %>%
    bind_rows(caaspp.mry.prior) %>%
    filter(Grade == 13,
           Entity_Type == "District",
           str_detect(District_Name, dist),
        #   Subgroup_ID == "1",
           Test_Id == testy, # Math 
       #    School_Code == "0000000",
            !is.na(Percentage_Standard_Met_and_Above),
       Subgroup_ID %notin% c(121, 190, 200, 201, 202, 203, 204, 205, 206, 207, 220, 221, 222, 223, 224, 225, 226, 227, 250, 251, 252)
    ) 


tabyl.set <- dist.hold %>%
    tabyl(Subgroup_ID) %>%
    filter(n != 2)

tabyl.set

purge.list <- tabyl.set  %>%
    select(Subgroup_ID)  %>%
    unlist()

purge.list

  dist.hold %>%
      filter(Subgroup_ID %notin% tabyl.set$Subgroup_ID)  %>%
      mutate(ranker = ifelse(Test_Year == yr.curr, Percentage_Standard_Met_and_Above, NA))  %>%
      compare.years(Subgroup, testy, dist, kular)

    
    }



compare.years.dist.group(dist = "Chualar", testy = 1, kular = "Pink")


compare.years.dist.group(dist = "South Monterey County", testy = 1, kular = "DarkMagenta")


compare.years.dist.group(dist = "Greenfield", testy = 2, kular = "Limegreen") # #007937ff


# 
compare.years.dist.group("Bradley", 1)


# Not ordered for some, like Chualar and SoMoCo
for (i in 1:2) {
    for (j in districts) {
        
        compare.years.dist.group(j, i)
        
    }
    
    
}


# TRoubleshooting 

caaspp.mry %>%
    bind_rows(caaspp.mry.prior) %>%
    filter(Grade == 13,
           # Subgroup_ID == "1",
           Subgroup_ID %notin% c(121, 190, 200, 201, 202, 203, 204, 205, 206, 207, 220, 221, 222, 223, 224, 225, 226, 227, 250, 251, 252),  # Removing the 250-252 because did not exist in 2022
           Entity_Type == "District",
           !is.na(Percentage_Standard_Met_and_Above),
           str_detect(District_Name, "Chualar")
    ) %>%
    tabyl(Subgroup_ID)

### Three Year Facet -----


caaspp.three.year <- tbl(con, "CAASPP") %>% 
    filter(County_Code %in% c("27","00"),
           # DistrictCode == "10272",
           Test_Year %in% c("2021", yr.prior ,yr.curr),
           Type_ID %in% c(3,4,5)
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




temp <- caaspp.three.year %>%
    filter(Subgroup_ID %in% c(74,75,76,77,78,79,80),
           Grade == 13,
           Test_Id == 1) %>%
  #  mutate(definition = fct_relevel(definition, "Kindergarten", after = 0 )) %>%
  #  filter(str_starts(reporting_category, subby)) %>%
    ggplot(aes(x = Test_Year, y = Percentage_Standard_Met_and_Above, group = County_Name ,color = County_Name)) +
    geom_line() +
    geom_line(size = 1.5) +
    geom_point(size = 3) + 
    facet_wrap(~Subgroup) +
    mcoe_theme +
    scale_color_few() +
    ylim(0,30) +    
    guides(color = guide_legend(""))




### Grades by EL ----

caaspp.mry %>%
    filter(Subgroup_ID %in% c(1,160),
           Entity_Type == "County",
           Test_Id == 1
           ) %>%
    ggplot(aes(y = Percentage_Standard_Met_and_Above, x = Grade, group = Subgroup, fill = Subgroup)) +
    geom_col(position = "dodge") +
    mcoe_theme +
    labs(title = "The percent of EL students meeting or exceeding standards decreases the higher the grade level. \nFor students overall it increases by grade level.")




    
### End ish ------

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



# ELA by Grade --- 
caaspp.ela.3.5 <- tbl(con, "CAASPP") %>% 
    filter(# Type_ID == 5,
        Subgroup_ID == 1,
        Grade %in% c(3,4,5),
         County_Code == "27",
     #   District_Code == "00000",
     School_Code == "0000000",
        Test_Year %in% c("2019","2022")) %>%
    collect() %>%
    clean.caaspp() %>%
    left_join(ent2) %>%
    mutate(Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above))


for (g in 3:5) {
    
caaspp.ela.3.5  %>%
    filter(Grade == g,
           District_Code != "00000") %>%
    compare.years(District_Name, 1, paste0("ELA ",g," Grade"))

}


caaspp.ela.3.5.wide <- caaspp.ela.3.5 %>%
    filter(Test_Id == 1) %>%
    select(District_Code, District_Name, Grade, Test_Id, Test_Year, Percentage_Standard_Met_and_Above,
           Students_with_Scores ) %>%
    mutate(Count_Met_And_Above = round2(Students_with_Scores * Percentage_Standard_Met_and_Above / 100, 0)) %>%
    group_by(District_Code, District_Name, Test_Id,Test_Year) %>%
    mutate(denom = sum(Students_with_Scores),
           numer = sum(Count_Met_And_Above),
           Percentage_Standard_Met_and_Above = 100*numer/denom) %>%
    select(District_Code, District_Name, Test_Id, Test_Year, Percentage_Standard_Met_and_Above) %>%
    distinct() %>%
    na.omit() %>%
    filter(!str_detect(District_Name,"Lagunita|Mission|Office|Antonio")) %>%
    compare.years(District_Name, 1, paste0("Combined 3-5 Grade"))



### Single School ----

caaspp.soledad <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
           District_Code == "75440",
           Test_Year %in% c("2019", "2022")
    )%>%
    collect() %>%
    clean.caaspp()

caaspp.soledad %>%
    filter(Grade == 13,
   #        str_detect(District_Name,"Salinas Union"),
           str_detect(School_Name,"Franscioni"),
           !str_detect(Subgroup, "Not migrant"),  # missing in 2019 and so messes up order if not excluded
   !str_detect(Subgroup, "Declined"),  # missing in 2019 and so messes up order if not excluded
 #  !str_detect(Subgroup, "IFEP"),  # missing in 2019 and so messes up order if not excluded
 #  !str_detect(Subgroup, "Homeless"),  # missing in 2019 and so messes up order if not excluded
   !is.na(Percentage_Standard_Met_and_Above),
           !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
    ) %>%
    compare.years(Subgroup, 2, "Franscioni")

### San Antonio -----

dist.name <- "South Monterey County"
dist.code <- 66068

caaspp.san.antonio <- tbl(con, "CAASPP") %>% 
    filter(County_Code %in% c("00" ,"27"),
           District_Code %in% c("00000", dist.code),
           Test_Year %in% c("2019", "2022")
    )%>%
    collect() %>%
    clean.caaspp()


san.antonio.groups <- caaspp.san.antonio %>%
    filter(Test_Year == "2022",
           District_Code == dist.code,
           !is.na(Percentage_Standard_Met_and_Above),
           Grade == "13"
           ) %>% 
    select(Subgroup, Percentage_Standard_Met_and_Above) %>% 
    mutate(Subgroup = factor(Subgroup),
           Subgroup = fct_reorder(Subgroup, Percentage_Standard_Met_and_Above)) 
    


# Compare Years 

caaspp.san.antonio %>%
    filter(Grade == 13,
           District_Code == dist.code,
           School_Code == "0000000",
           Subgroup %in% san.antonio.groups$Subgroup,
            !str_detect(Subgroup, "Not migrant"),  # missing in 2019 and so messes up order if not excluded
   #        !str_detect(Subgroup, "Graduate school"),    # missing in 2019 and so messes up order if not excluded
#   !str_detect(Subgroup, "with disability"),  # missing in 2019 and so messes up order if not excluded
#   !str_detect(Subgroup, "English learner"),  # missing in 2019 and so messes up order if not excluded
#   !str_detect(Subgroup, "Homeless"),  # missing in 2019 and so messes up order if not excluded
   !str_detect(Subgroup, "Not Foster"),  # missing in 2019 and so messes up order if not excluded
   !str_detect(Subgroup, "White - Not"),  # missing in 2019 and so messes up order if not excluded
   !str_detect(Subgroup, "Declined"),  # missing in 2019 and so messes up order if not excluded
           !is.na(Percentage_Standard_Met_and_Above),
   #        !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
    ) %>%
    compare.years(Subgroup, 1, dist.name)

#ggsave(here("figs", paste0("Compared years ",dist.name," ELA.png")))





# s.a.years <- caaspp.san.antonio %>%
#     filter(Grade == 13,
#            District_Code == dist.code,
#            School_Code == "0000000",
#            Test_Id == 2, 
#          #  Subgroup %in% san.antonio.groups$Subgroup,
#            # #        str_detect(District_Name,"Salinas Union"),
#             !str_detect(Subgroup, "Not migrant"),  # missing in 2019 and so messes up order if not excluded
#            !str_detect(Subgroup, "Graduate school"),  
#            # !str_detect(Subgroup, "Declined"),  # missing in 2019 and so messes up order if not excluded
#            # !str_detect(Subgroup, "Not a high school graduate"),
#            # !str_detect(Subgroup, "English learners enrolled in school "),
#            # !str_detect(Subgroup, "English learner"),
#            # # #  !str_detect(Subgroup, "IFEP"),  # missing in 2019 and so messes up order if not excluded
#            # #  !str_detect(Subgroup, "Homeless"),  # missing in 2019 and so messes up order if not excluded
#            !is.na(Percentage_Standard_Met_and_Above),
#            #        !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
#     ) 
# 
# 
# 
# ggplot(mapping = aes(x = reorder(Subgroup, Percentage_Standard_Met_and_Above),
#                      y = Percentage_Standard_Met_and_Above/100)) +
#     geom_col(data =  s.a.years[s.a.years$Test_Year == "2019",],  
#              position = "dodge" ,
#              
#              fill = "light grey",
#              width = 0.75) +
#     geom_col(data =  s.a.years[s.a.years$Test_Year == "2022",],
#                       position = "dodge" ,
#              width = 0.5,
#              fill = "steel blue") +
#     coord_flip() +
#     mcoe_theme + 
#     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#     labs(title = paste0(dist.name, " CAASPP Math Rates Meeting or Exceeding"),
#          subtitle = "Grey is 2019 and Blue is 2022",
#          y = "",
#          x = "",
#          caption = source.link
#     ) 




### Compare three levels 

# caaspp.san.antonio %>%
#     filter(Grade == 13,
#            Test_Year == "2022",
#            Subgroup %in% san.antonio.groups$Subgroup,
#            Test_Id == 1,
#            Entity_Type != "School",
#            Subgroup != "NA"
#     ) %>%
#     ggplot(aes(x = reorder(Subgroup, Percentage_Standard_Met_and_Above),
#                y = Percentage_Standard_Met_and_Above/100,
#                fill = Entity_Type
#                )) +
#     geom_col(position = "dodge") + 
#     coord_flip() + 
#     mcoe_theme + 
#     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#     labs(title = paste0( dist.name," 2022 CAASPP ELA Percent Meet and Exceed by Student Group"),
# #         subtitle = "Grey is 2019 and Blue is 2022",
#          y = "",
#          x = "",
#          caption = source.link
#     ) 

three.levels <- function(df, test.id, dist.name) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    

san.antonio.graph <- df %>%
    filter(Grade == 13,
           Test_Year == "2022",
           Subgroup %in% san.antonio.groups$Subgroup,
           Test_Id == test.id,
           Entity_Type != "School",
           Subgroup != "NA"
    ) 


    ggplot(mapping = aes(x = reorder(Subgroup, Percentage_Standard_Met_and_Above),
               y = Percentage_Standard_Met_and_Above/100,
               fill = Entity_Type
    )) +
    geom_col(data =  san.antonio.graph[san.antonio.graph$Entity_Type == "State",], 
             fill = "light grey",
             width = 0.8) + 
        geom_col(data =  san.antonio.graph[san.antonio.graph$Entity_Type == "County",], 
                 fill = "light blue",
                 width = 0.65) + 
        geom_col(data =  san.antonio.graph[san.antonio.graph$Entity_Type == "District",], 
                 fill = "orange",
                 width = 0.50) + 
    coord_flip() + 
    mcoe_theme + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste0("2022 CAASPP ", test.name ," Percent Meet and Exceed by Student Group"),
                  subtitle = paste0("Grey is California, Blue is Monterey County and Orange is ", dist.name),
         y = "",
         x = "",
         caption = source.link
    ) 

ggsave(here("figs", paste0(dist.name, ", Monterey County and California - ", test.name ,  Sys.Date(),".png" )),
       width = 8, height = 6)

}

three.levels(caaspp.san.antonio, 2, dist.name)

#  Over time 


over.time <- function(df, test.id, dist.name, heading) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    

sa.long <- df %>%
    filter(Grade == 13,
           Subgroup_ID == "1",
           Test_Id == test.id, # ELA 
           School_Code == "0000000"
    ) 

entit <- sa.long %>%
    filter(Test_Year == "2022") %>%
    select(County_Code,District_Code,School_Code, Entity = Entity_Type)


sa.long <- sa.long %>%
    left_join(entit) %>%
    distinct()


ggplot(data = sa.long, aes(x = Test_Year, y = Percentage_Standard_Met_and_Above, group = Entity)) +
  #  facet_wrap(~Entity) +
    geom_line(aes(color = Entity, alpha = 1), size = 1) +
    geom_text_repel(data = sa.long %>% filter(Test_Year == "2019"),
                    aes(label = Entity) ,
                    hjust = "left",
                    segment.size = .2,
                    segment.color = "grey",
                    size = 3,
                    nudge_x = -.4,
                    direction = "y") +
    geom_text_repel(data = sa.long %>% filter(Test_Year == "2022"),
                    aes(label = Entity) ,
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
    # theme(axis.text.y      = element_blank()) +
    # theme(panel.grid.major.y = element_blank()) +
    # theme(panel.grid.minor.y = element_blank()) +
    # theme(axis.ticks       = element_blank()) +
    scale_x_discrete(position = "top") +
    theme(legend.position = "none") +
    labs(#title = "San Antonio Decreased More Sharply than Monterey County or California",
         #subtitle = "CAASPP ELA",
        title = paste0(dist.name, heading, " compared to Monterey County, and California"),
        subtitle = paste0("CAASPP ",test.name),
         y = "Percent Meeting or Exceeding Standard",
         x = "")



ggsave(here("figs", paste0("Over time ",dist.name," Monterey County and California - ", test.name ,  Sys.Date(),".png" )),
       width = 8, height = 6)
}

over.time(caaspp.san.antonio,
          1,
          dist.name,
          " decreased less sharply")



### End -----