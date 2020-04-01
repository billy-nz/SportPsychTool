
library(formattable)

shinyServer(function(input, output, session) {

  KEY <- as.data.frame(readxl::read_xlsx("data/OutputScaleMapping.xlsx"))[-c(2,4:9),]
  RV  <- reactiveValues()

  # ---- A.  File Upload Action ----
  observeEvent(input$upload, {

    file.name <- input$upload$name
    file.path <- paste0("data/", file.name)

    if(tools::file_ext(file.name) == "xlsx"){

      file.rename(input$upload$datapath, file.name) # Rename shiny datapath to file.name
      file.copy(file.name, "./data") # Copy to data directory
      file.remove(file.name)

    } else {
      return()
    }

    RV$DAT <- as.data.frame(suppressMessages(readxl::read_xlsx(file.path)))[-1, -c(2,4:9)]

    source("preprocessing.R", local = T)

    # metadata tables
    output$SampleMeta <- renderFormattable({
      formattable(METADAT_n,
                  align = 'l',
                  list(` ` = formatter("span", style =~ formattable::style(font.weight = "bold"))))
    })

    output$MHWBMeta <- renderFormattable({
      formattable(METADAT_MH,
                  align = 'l',
                  list(` ` = formatter("span", style =~ formattable::style(font.weight = "bold"))))
    })

    output$CopingMeta <- renderFormattable({
      formattable(METADAT_COPE,
                  align = 'l',
                  list(` ` = formatter("span", style =~ formattable::style(font.weight = "bold"))))
    })

  })

  observeEvent(input$expand_meta, {
    shinyjs::toggle("element")
  })

  # ---- B. Data Filter Action ----

  # Default
  observe({

    if(is.null(input$filter) & is.null(RV$DAT)){  # No filter & no data - do nothing
      return()
    } else if(is.null(input$filter) & !is.null(RV$DAT)) { # No filter but has data - show all names

      updateSelectInput(session = session,
                        inputId = "SelectPlayer",
                        label   = paste0("Select Player (", nrow(RV$DAT), ")"),
                        choices = paste(RV$DAT$Demo_fname, RV$DAT$Demo_lname),
                        selected = paste(RV$DAT$Demo_fname, RV$DAT$Demo_lname)[1])

    } else if(!is.null(input$filter) & is.null(RV$DAT)) {  # No data but filters being pressed - show error message & auto-reset filters

      showNotification("Please upload data", type = "error", duration = 1)
      updateCheckboxGroupInput(session = session, inputId = "filter", selected = c("0"))

    } else {  # Data available and filter being pressed - update names list per filter

      # Very important - using mget() :. name must match UI filter name
      isRedflag  <- which(RV$DAT$Redflag == 1)
      isRookie   <- which(RV$DAT$Demo_rookie == 2)
      isRelocate <- which(RV$DAT$Demo_reloc1 == 2)
      isWomens   <- which(RV$DAT$Demo_lg == "Womens")
      isLast3    <- which(as.Date(Sys.time()) - RV$DAT$Admin_stdate <= 3)

      filter.by <- Reduce(intersect, mget(input$filter))

      if(length(filter.by)==0){
        showNotification("No players matching criteria", type = "error", duration = 2)

        output$NameClub  <- renderUI({fluidRow(style="margin-top: 100px")})
        output$Status    <- renderUI({})
        output$DemoTable <- renderFormattable({formattable(RV$EMPTY_DF)})
        output$Summary   <- renderUI({})

      }

      updateSelectInput(session = session,
                        inputId = "SelectPlayer",
                        label   = paste0("Select Player (", length(filter.by), ")"),
                        choices = paste(RV$DAT$Demo_fname[filter.by], RV$DAT$Demo_lname[filter.by]),
                        selected = paste(RV$DAT$Demo_fname[filter.by], RV$DAT$Demo_lname[filter.by])[1])

    }
  })


  # ---- C.  Player Select Action ----
  observeEvent(input$SelectPlayer, {

    if(input$SelectPlayer %in% c("", "--- select ---")) return() # If no selection, then do not execute rest

    userid  <- RV$DAT$Admin_rid[which(paste(RV$DAT$Demo_fname, RV$DAT$Demo_lname) == input$SelectPlayer)][1]
    IPLDAT  <- RV$DAT[which(RV$DAT$Admin_rid == userid), ]

    RV$IPLDAT <- IPLDAT

    source("individual_analysis.R", local = T)

    # Status Symbols
    output$Status <- renderUI({
      list(
        if(IPLDAT$Redflag == 1){bsButton("anyredflag", label = "Red Flag", size = "extra-small", style = "danger")},
        if(IPLDAT$Demo_rookie == 2){bsButton("rookie", label = "Rookie", size = "extra-small", style = "success")},
        if(IPLDAT$Demo_lg == "Womens"){bsButton("womens", label = "Womens", size = "extra-small", style = "info", disable = T)},
        if(IPLDAT$Demo_reloc1 == 2){bsButton("reloc", label = "Relocating", size = "extra-small", style = "primary")})
    })

    # Summary Plots
    output$MWHBPlot <- renderPlot({
      barplot(MHWB_val,
              # names.arg = c("Self-eff.", "Self-est.", "Life-sat.", "Kessler-6", "PHQ9", "Trauma", cult),
              names.arg = MHWB_names,
              ylim = c(0, 100),
              cex.axis = 0.8,
              cex.names = 0.75,
              col = c(rep("limegreen", 3),
                      ifelse(MHWB_val[4]>=50, "orangered", "limegreen"),
                      ifelse(MHWB_val[5]>=18.5, "orangered", "limegreen"),
                      ifelse(MHWB_val[6]>=46.5, "orangered", "limegreen"),
                      rep("limegreen", 3)))
    })


    output$CopingPlot <- renderPlot({
      barplot(Coping.val,
              names.arg = Coping.names,
              ylim = c(0, 100),
              cex.axis = 0.8,
              cex.names = 0.7,
              col = c(rep("limegreen", 8), ifelse(Coping.val[9]>=40, "orangered", "limegreen")))
    })

    output$RelocPlot <- renderPlot({
      barplot(Reloc.val,
              names.arg = Reloc.names,
              ylim = c(0, 100),
              cex.axis = 0.8,
              cex.names = 0.6,
              col = ifelse(Reloc.val <50, "orangered","limegreen"))
    })

    # 1.  Player Profile
    output$NameClub <- renderUI({
      list(
        h1(paste(input$SelectPlayer)),
        h3(IPLDAT$Demo_club1))
    })

    output$DemoTable <- renderFormattable({
      formattable(DEMO_DF,
                  align = 'l',
                  list(` ` = formatter("span", style = ~ formattable::style(font.weight = "bold"))))
    })

    # 2.  MH & Wellbeing
    output$MHWBHeader <- renderUI({
      list(
        h3("Mental Health & Wellbeing"),
        if(IPLDAT$PsyDist_1 > 0){bsButton("hopeless", label = "Hopelessness", size = "extra-small", style = "danger")},
        if(IPLDAT$Depress_9 > 0){bsButton("selfharm", label = "Self-harm", size = "extra-small", style = "danger")},
        if(IPLDAT$psydist >= 12){bsButton("psydist", label = "Distressed", size = "extra-small", style = "danger")},
        if(IPLDAT$depress >= 5){bsButton("depress", label = "Depressed", size = "extra-small", style = "danger")},
        if(IPLDAT$trauma >= 14){bsButton("trauma", label = "Trauma", size = "extra-small", style = "danger")}
      )
    })

    output$MHWTable <- renderFormattable({
      formattable(MHWB_DF,
                  align = 'l',
                  list(`Scale` = formatter("span", style =~ formattable::style(font.weight = "bold"))))
    })

    # 3.  Coping Profile
    output$CopingHeader <- renderUI({
      list(
        h3("Coping Profile"),
        if(IPLDAT$alcohol >= 16){bsButton("alcohol", label = "Alcohol Use", size = "extra-small", style = "danger")}
      )
    })

    output$CopingTable <- renderFormattable({
      formattable(COPING_DF,
                  align = 'l',
                  list(`Scale` = formatter("span", style = ~ formattable::style(font.weight = "bold"))))
    })

    # 4.  Relocation Profile
    output$RelocHeader <- renderUI({
      list(
        h3("Relocation Profile"),
        if(IPLDAT$Demo_reloc1 == 1){bsButton("noreloc2", label = "No Relocation", size = "extra-small", style = "warning")}
      )
    })

    output$RelocTable <- renderFormattable({
      formattable(RELOC_DF,
                  align = 'l',
                  list(`Scale` = formatter("span", style = ~ formattable::style(font.weight = "bold"))))
    })

    # 5. Render Summary Tab
    output$Summary <- renderUI({
      list(
        fluidRow(style="margin-left: 10px",
                 splitLayout(cellWidths = c("50%", "50%"),
                             align = "center",
                             list(uiOutput("MHWBHeader"), # MH-WB
                                  div(formattableOutput("MHWTable"), style = "margin-top:20px; padding-left: 80px; padding-right: 100px;"),
                                  div(plotOutput("MWHBPlot"), style = "margin-top:-10px"),
                                  div(downloadButton(outputId = "save_MHWB_Plot", label = "Save Plot"), style = "margin-top:-10px")
                             ),

                             list(uiOutput("CopingHeader"), # Coping
                                  div(formattableOutput("CopingTable"), style = "margin-top:20px; padding-left: 80px; padding-right: 100px;"),
                                  div(plotOutput("CopingPlot"), style = "margin-top:-30px"),
                                  div(downloadButton(outputId = "save_Coping_Plot", label = "Save Plot"), style = "margin-top:-10px")
                             ))),
        hr(),
        fluidRow(style="margin-left: 10px",
                 splitLayout(cellWidths = c("40%", "60%"),
                             align = "center",
                             list(uiOutput("RelocHeader", style = "margin-top: -10px"),
                                  div(formattableOutput("RelocTable"), style = "margin-top:20px; padding-left: 50px; padding-right: 70px;"),
                                  br()),

                             list(
                               div(plotOutput("RelocPlot"), style = "margin-top:70px;")
                             )))
      )

    })


    # 5.  Save / Download
    wb.name <- gsub(" ", "_",
                    paste(input$SelectPlayer,
                          paste0(gsub("-", "", Sys.Date()), ".xlsx")))

    output$dl <- downloadHandler(
      filename = function() {wb.name},
      content = function(file) {writexl::write_xlsx(list(Demo = DEMO_DF,
                                                         MH_Welling = MHWB_DF,
                                                         Coping = COPING_DF,
                                                         Relocation = RELOC_DF), path = file)
      })

    output$ExcelDL <- renderUI({
      list(p("Assessment summary for ", strong(input$SelectPlayer), " | ", downloadLink("dl", "Download")))
    })

    # Plot Download
    output$save_MHWB_Plot <- downloadHandler(
      filename = function() { paste0(input$SelectPlayer, "_MHWB", ".png")},
      content = function(file) {
        png(file,
            width     = 5.25,
            height    = 3.25,
            units     = "in",
            res       = 300,
            pointsize = 7)
        barplot(MHWB_val,
                names.arg = MHWB_names,
                ylim = c(0, 100),
                cex.axis = 0.8,
                cex.names = 0.7,
                col = c(rep("limegreen", 3),
                        ifelse(MHWB_val[4]>=50, "orangered", "limegreen"),
                        ifelse(MHWB_val[5]>=18.5, "orangered", "limegreen"),
                        ifelse(MHWB_val[6]>=46.5, "orangered", "limegreen"),
                        rep("limegreen", 3)))
        dev.off()
      }
    )


    output$save_Coping_Plot <- downloadHandler(
      filename = function() { paste0(input$SelectPlayer, "_Coping", ".png")},
      content = function(file) {
        png(file,
            width     = 5.25,
            height    = 3.25,
            units     = "in",
            res       = 300,
            pointsize = 7)
        barplot(Coping.val,
                names.arg = Coping.names,
                ylim = c(0, 100),
                cex.axis = 0.8,
                cex.names = 0.7,
                col = c(rep("limegreen", 8), ifelse(Coping.val[9]>=40, "orangered", "limegreen")))
        dev.off()
      }
    )

    # # --- Traceback ---
    output$Traceback <- renderUI({
      fluidRow(style="margin-left: 10px; margin-top:20px",
               splitLayout(cellWidths = c("20%", "80%"),
                           align = "left",
                           radioButtons("SelectScale",
                                        label = "Select a Sub-scale:",
                                        selected = "",
                                        inline = F,
                                        choices = list("Self-Efficacy" = "Selfeff",
                                                       "Self-Esteem" = "Selfest",
                                                       "Life Satisfaction" = "Satisf",
                                                       "Distress" = "PsyDist",
                                                       "Depression" = "Depress",
                                                       "Trauma" = "Trauma",
                                                       "Task/O Coping" = "Coping_T",
                                                       "Emo/O Coping" = "Coping_E",
                                                       "Avoid/O Coping" = "Coping_A",
                                                       "Extraversion" = "IPIP_Ex",
                                                       "Agreeableness" = "IPIP_Ag",
                                                       "Conscientiousness" = "IPIP_Con",
                                                       "Emotionality" = "IPIP_Em",
                                                       "Openness to Exp." = "IPIP_Op",
                                                       "Alcohol Use" = "Alcohol")),

                           list(p("** Scale reversal has been applied to the score value shown, if applicable. **"),
                                formattableOutput("TraceTable"))
               )
      )
    },
    observeEvent(input$SelectScale, {

      QS  <- KEY[KEY$Subscale == input$SelectScale, ]
      QS$Score <- unlist(RV$IPLDAT[KEY$Varname[KEY$Subscale == input$SelectScale]])
      QS <- QS[, c("Varname", "Score", "Desc")]
      names(QS)[3] <- "Description"
      row.names(QS) <- NULL

      output$TraceTable <- renderFormattable({
        formattable(QS,
                    align = 'l',
                    list(` ` = formatter("span", style =~ formattable::style(font.weight = "bold"))))
      })


    }))

  })

  # ---- D. Security ----
  url <- a("sample dataset", href="https://github.com/billy-nz/EchoNLP/blob/master/data/SampleData.xlsx")

  PassModal <- function(failed = FALSE){
    showModal(
      tags$div(
        id="modal-box",
        modalDialog(
          size = "s",
          easyClose = FALSE,
          fade = FALSE,
          tagList("Download: ", actionLink(inputId = "download", label = "sample dataset",
                                           onclick ="location.href='https://github.com/billy-nz/EchoNLP/blob/master/data/SampleData.xlsx';")),
          footer = NULL
        )))
  }

  PassModal()

  observeEvent(input$download, {
      removeModal()
  })


})
