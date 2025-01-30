#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#moved library to global.R
#library(dplyr)
# list loupe image paths to filter from
#loupe_image_paths <- tibble("LoupeImages" = list.files(path = "www/LoupeImages/"))

#qoptics_data <- readxl::read_excel("data/Dental_data.xlsx",
#                                      sheet = "Loupe_types") %>%
#  filter(`Mfg` == "Q-Optics") %>%
#  rename(`Q-Optics Frame` = Mod, Style = Size, `Innovative Optics Insert` = `Insert Part Number`)

# Load dental data
#lens_data <- readxl::read_excel("data/Dental_data.xlsx", sheet = "Lens_details") %>%
#  select(-VLT)

#dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
#  filter(`Laser Mfg` != "") %>%
#  select(-Website) %>%
#  mutate(VLT = scales::percent(as.numeric(VLT))) %>%
#  left_join(lens_data, by = join_by(`Eyewear Lens Compatible` == Lens))


 # ui = uiOutput("tab")

shinyServer(function(input, output, session) {
  observeEvent(input$mfg,{
    # filter dental data to select mfg
    mfg_filtered_dental_data <- dental_data %>% 
      filter(`Laser Mfg` == input$mfg)
    # update select input - laser model
    updateSelectInput(inputId = "mod",
                      choices = sort(mfg_filtered_dental_data$`Laser Model`))
  })
  
  observeEvent(input$loupestyle,{
    # filter dental data to select mfg
    style_filtered_qoptics_data <- qoptics_data %>% 
      filter(`Q Optics Loupe` == input$loupestyle)
    # update select input - laser model
    updateSelectInput(inputId = "style",
                      choices = sort(style_filtered_qoptics_data$`Style`))
  })

  
  loupe_insert <- eventReactive(c(input$loupestyle, input$style),{
    result <- qoptics_data %>% 
      filter(`Q Optics Loupe` == input$loupestyle) %>%
      filter(`Style` == input$style) %>%
      distinct()
    
    print("\nLoupe Insert")
    print(result)
    result
  })
  
  # selected_data <- eventReactive(input$mod,{
  #   req(input$mfg)
  #   dental_data %>% 
  #     filter(`Laser Mfg` == input$mfg,
  #            `Laser Model` == input$mod)
  # })
  
  selected_data <- reactive({
    ##print('test')
    req(input$mod)
    req(loupe_insert())
    
    ##print(loupe_insert())
    
    result <- dental_data %>%
      filter(`Laser Mfg` == input$mfg,
             `Laser Model` == input$mod) %>%
      mutate(`INVO Part Number Raw` = if_else(`Eyewear Lens Compatible` == "Gi1",
                                              glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , `Eyewear Lens Compatible`),
                                              glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , `Eyewear Lens Compatible`, ".2B")),
             `Website`= case_when(loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/pi1-inview-large-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/ivl-r-pi1-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/pi1-inview-large-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/ivr-r-pi1-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/pi17-inview-large-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/ivl-r-pi17-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/ivl-r-pi19-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/ivr-r-pi19-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/pi23-inview-large-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivl-r-pi23-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/pi23-inview-large-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivr-r-pi23-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivr-r-pi23-laser-insert-for-loupes/",

                                  loupe_insert()$`Innovative Optics Insert` %in% c("Primo") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/primo-pi1-laser-inserts/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("Primo") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/primo-pi17-laser-inserts/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("Primo") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/primo-pi19-laser-inserts/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("Primo") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/primo-pi23-laser-inserts/",

                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-large-laser-clip-in/",

                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/ivl-r-gi1-laser-insert-for-loupes/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-regular-laser-clip-in/",
                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-regular-laser-clip-in/",
                                  .default = Website)
      ) %>%
     mutate(`INVO Part Number` = glue::glue_safe("<a href='{Website}' target ='_blank'> {INVO Part Number Raw} </a> "))

#      mutate(`INVO Part Number` = ifelse(
#        str_detect(Website, "https://innovativeoptics.com/pi(1|17|23|19)-laser-glasses-frames/"),`INVO Part Number Raw`, glue::glue_safe("<a href='{Website}' target ='_blank'> {INVO Part Number Raw} </a> ")
#      ))
    
      print(result)
      result
      
  })
 
  
  
  user_info <- eventReactive(input$run,{
    result <- tibble(
    #"Q-Optics Loupe Style" = loupe_insert()$`Q Optics Loupe`, 
    "Q-Optics Loupe Style" = glue::glue_safe(input$loupestyle, " ", input$style), #qoptics_data()$`Mod`,
    "Laser Information" = glue::glue_safe(selected_data()$`Laser Mfg`, " ", selected_data()$`Laser Model`),
    "Laser Specifications" = selected_data()$Wavelengths) %>%
      distinct()
  })
  
  output$userInfo <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  {
                                    user_info()
                                  })
  
  # table_info <- eventReactive(input$run,{
#    tibble("INVO Part Number" = if_else(selected_data()$`Eyewear Lens Compatible` == "GP30",
#                                                     glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`),
#                                                     glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`, ".2B")),
#           "Optical Density Specifications" = selected_data()$`Optical Density`,
#           "Visible Light Transmission" = selected_data()$VLT)
    
#    tibble("INVO Part Number" = if_else(selected_data()$`Eyewear Lens Compatible` == "Pi1",
#                                        glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`),
#                                        glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`, ".2B")),
#           "Optical Density Specifications" = selected_data()$`Optical Density`,
#           "Visible Light Transmission" = selected_data()$VLT
#            )
 #   tags$a(href="https://innovativeoptics.com/pi1-laser-glasses-frames/", "Pi1")
  #  example_tag <- tags$a(href = "https://innovativeoptics.com/pi1-laser-glasses-frames/", " Innovative Optics Pi1 Laser Glasses")
  #selected_data()$`Eyewear Lens Compatible` == "GP30"|  
    
 #   LensID <- selected_data()$`Eyewear Lens Compatible`
    
#    if(selected_data()$`Eyewear Lens Compatible` == "Pi1")
      
     # tibble("Lens ID =" tags$a(href="https://innovativeoptics.com/pi1-laser-glasses-frames/")
      
#    tibble("Lens ID =" if_else(selected_data()$`Eyewear Lens Compatible` == "Pi1",
#           tags$a(href="https://innovativeoptics.com/pi1-laser-glasses-frames/", "")
    
    # })
  
  table_info <- eventReactive(input$run,{
    tibble("INVO Part Number" = selected_data()$`INVO Part Number`,
           "Optical Density Specs" = selected_data()$`Optical Density`,
           "% VLT" = selected_data()$VLT) 
#            %>%
#      distinct()
  })
  
  output$tableInfo <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  height="100%",
                                  {
    table_info()
  }, sanitize.text.function = function(x) x)
  
 rec1_table <- eventReactive(input$run,{
   ## print(selected_data())
   tibble("INVO Part Number" = selected_data()$`Rec1`)
 })
 output$tableRec1 <- renderTable(bordered = T,
                                 align = "l",
                                 striped=T,
                                 {
                                   rec1_table()
                                 })
 rec2_table <- eventReactive(input$run,{
   tibble("INVO Part Number" = selected_data()$`Rec2`)
 })
 output$tableRec2 <- renderTable(bordered = T,
                                 align = "l",
                                 striped=T,
                                 {
                                   rec2_table()
                                 })
 rec3_table <- eventReactive(input$run,{
   tibble("INVO Part Number" = selected_data()$`Rec3`)
 })
 output$tableRec3 <- renderTable(bordered = T,
                                 align = "l",
                                 striped=T,
                                 {
                                   rec3_table()
                                 })
  
 
 # 
 #  image_location <- eventReactive(c(input$mod, input$loupestyle),{
 #    print("dadas")
 #    req(input$run)
 # #   #get lens from QOptics based on Eyewear Lens Compatible From Dental data EXCEL
 #    print(loupe_image_paths)
 #    loupe_rec <- loupe_image_paths %>%
 #      filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "",  input$loupestyle)) &
 #               stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "."))
 #               )
 #      )
 #    
 #    print(loupe_rec)
 #    
 #    result <- c(glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[2]]),
 #      if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
 #              glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
 #              glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
 #      ),
 #      if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
 #              glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpeg"),
 #              glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
 #      ),
 #      glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"),
 #      glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[1]]))
 #    
 #    print(result)
 #    result
 #    
 #  })
 
 # 
 # image_location <- reactive({
 #   print("dadawws")
 #   ## req(input$run)
 #   #   #get lens from QOptics based on Eyewear Lens Compatible From Dental data EXCEL
 #   print(loupe_image_paths)
 #   loupe_rec <- loupe_image_paths %>%
 #     filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "",  input$loupestyle)) &
 #              stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "."))
 #              )
 #     )
 #   
 #   print(loupe_rec)
 #   
 #   result <- c(glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[2]]),
 #               if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
 #                       glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
 #                       glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
 #               ),
 #               if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
 #                       glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpeg"),
 #                       glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
 #               ),
 #               glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"),
 #               glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[1]]))
 #   
 #   print(result)
 #   result
 # })
  
 image_location <- eventReactive(input$run,{
#   req(input$run)
   req(input$loupestyle)
   req(input$mfg)
   req(input$mod)
   
   #   #get lens from QOptics based on Eyewear Lens Compatible From Dental data EXCEL
   loupe_rec <- loupe_image_paths %>%
     filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "",  input$loupestyle)) &
              stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "",  input$style)) &
              stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "Front"))
              )
     )
   
#     loupe_rec <- loupe_image_paths %>%
#     filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "", input$loupestyle)) &
#              stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "", input$style)) &
#              stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "."))
#              )
#     )  
     
   ## print(loupe_rec)
   
   result <-  c(glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[1]]),
                if_else(selected_data()$`Eyewear Lens Compatible` %in% c("Pi19", "Pi23"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
                ),
                if_else(selected_data()$`Eyewear Lens Compatible` %in% c("Pi19", "Pi23"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
                ),
                glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"))
   
   print(result)
   result
   
 })


  output$productImage <- renderImage({
    list(src = image_location()[[1]],
         width = "500px",
         contentType = "image/jpeg")
  }
  ,deleteFile = FALSE)
  
  output$rec1 <- renderImage({
    list(src = image_location()[[2]],
         height = "90px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)
  
  output$rec2 <- renderImage({
    list(src = image_location()[[3]],
         height = "90px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)
  
  output$rec3 <- renderImage({
    list(src = image_location()[[4]],
         height = "90px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

})


