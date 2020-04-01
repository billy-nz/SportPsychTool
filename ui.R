
library(shinyBS)
library(formattable)
library(shinyjs)

# Javascript
PressEnter <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#ok").click();
}});'


# Shiny Page
fluidPage(
  useShinyjs(),

  # ---- A.  CSS ----
  tags$head(tags$script(HTML(PressEnter))),
  tags$style("#modal-box .modal { text-align:center;
                                  padding-top: 80px;}"),
  tags$style(".shiny-notification { position: fixed;
                                    top: calc(50%);
                                    left: calc(0%);
                                    width: 12em}"),

  # ---- B.  Header ----
  fluidRow(
    column(12, h2("Athlete Wellbeing Assessment"),
           hr(style = "margin-top:5px"))
  ),

  # ---- C.   Data Management ----

  column(2,
         fluidRow(h3("Data Selection"),
                  wellPanel(
                            fileInput("upload",
                                      label = "Upload",
                                      multiple = FALSE,
                                      buttonLabel = "Open",
                                      accept = "xlsx", ),
                            selectInput("SelectPlayer",
                                        label = "Select Player",
                                        choices = NULL, size = 10, selectize = F),
                            checkboxGroupInput("filter",
                                               label = "Filter by",
                                               select = NULL,
                                               inline = F,
                                               choices = list("Red Flag(s)" = "isRedflag",
                                                              "Last 3 Days" = "isLast3",
                                                              "Relocating" = "isRelocate",
                                                              "Rookie" = "isRookie",
                                                              "Women's League" = "isWomens")),
                            style="border: 1px solid #ccc;")),

         fluidRow(h3("Download"),
                  wellPanel(uiOutput("ExcelDL"))),

         fluidRow(div(style="display:inline-block", h3("Meta Data")),
                  actionLink(inputId = "expand_meta", label = "hide/show"),
                  wellPanel(
                  hidden(div(id = "element",
                             p("Data about the data: This is a summary for the uploaded dataset and the distribution across all respondents."),
                             formattableOutput("SampleMeta"),
                             formattableOutput("MHWBMeta"),
                             formattableOutput("CopingMeta"),
                             p("Warnings: Will not work if player fills in 4 or more sets of cultural hertiage responses; additional programme required."),
                             p("Consider additional rules for flags & status tags, and plot bar-colors changes for quick visualisation. Consider additional metadata fields."),
                             p("Consider automatically generated generic / summary text based on rules, status, conditions-met.")))
                  ))
  ),

  # ---- D. Individual Player Summary -----
  column(10, style="padding-left: 20px",
         fluidRow(h3("Player Profile"), style="margin-left: 10px",
                  wellPanel(style="border: 1px solid #ccc; background: none; margin-right: 20px",
                            splitLayout(cellWidths = c("50%", "50%"),
                                        align = "center",
                                        list(uiOutput("NameClub", style = "marging-top: -20px; margin-right:5px"), # Left
                                             uiOutput("Status", style = "marging-top: 5px; margin-right:5px"),
                                             div(formattableOutput("DemoTable"), style = "margin-top: 10px; padding-left: 100px; padding-right: 100px;")),

                                        list(div(style = "margin-top: 50px"))   # Right (verbatum summary)
                                        ))),

         tabsetPanel(id = "tabs",
                     tabPanel("Summary", id = "Summary", uiOutput("Summary")),
                     tabPanel("Traceback", id = "Traceback", uiOutput("Traceback")))


  )

)

