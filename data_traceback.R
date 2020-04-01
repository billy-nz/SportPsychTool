
,
"Task/O Cope" = "Coping_T",
"Emo/O Cope" = "Coping_E",
"Avoid/O Cope" = "Coping_A",
"Extraver" = "IPIP_Ex",
"Agreeable" = "IPIP_Ag",
"Conscien" = "IPIP_Con",
"Emotionality" = "IPIP_Em",
"Openness" = "IPIP_Op",
"Alcohol" = "Alcohol"


observeEvent(input$tabs=="Traceback", {
 
 output$Traceback <- renderUI({
  fluidRow(style="margin-left: 10px; margin-top:20px", 
           list(strong("Select a Subscale:"), br(), 
                div(style="display:inline-block", h4(actionLink(inputId = "selfeff_button", label = "Self-Efficacy"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "selfest_button", label = "Self-Esteem"))), 
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "lifesat_button", label = "Life Satisfaction"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "distress_button", label = "Psychological Distress"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "depress_button", label = "Depressive Screen"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "trauma_button", label = "Trauma"))), 
                
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "tocope_button", label = "Task/O Coping"))), 
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "emcope_button", label = "Emo/O Coping"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "avoid_button", label = "Avoid/O Coping"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "extra_button", label = "Extraversion"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "agree_button", label = "Agreeableness"))), 
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "consc_button", label = "Conscientiousness"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "emotion_button", label = "Emotionality"))),
                div(style="display:inline-block", h4(" | ")), 
                div(style="display:inline-block", h4(actionLink(inputId = "open_button", label = "Openness"))), 
                
                hidden(div(id = "selfeff_ans", 
                           p("Testing1 Testing1 Testing1"))),
                
                hidden(div(id = "selfest_ans", 
                           p("Testing2 Testing2 Testing2"))),
                
                hidden(div(id = "lifesat_ans", 
                           p("Testing3"))),
                
                hidden(div(id = "distress_ans", 
                           p("Testing4"))),
                
                hidden(div(id = "depress_ans", 
                           p("Testing5"))),
                
                hidden(div(id = "trauma_ans", 
                           p("Testing6"))),
                
                
                hidden(div(id = "tocope_ans", 
                           p("cop"))),
                
                hidden(div(id = "emcope_ans", 
                           p("emcsaop"))),
                
                hidden(div(id = "avoid_ans", 
                           p("avoid"))),
                
                
                hidden(div(id = "extra_ans", 
                           p("Testing7"))),
                
                hidden(div(id = "agree_ans", 
                           p("Testing8"))),
                
                hidden(div(id = "consc_ans", 
                           p("Testing9"))),
                
                hidden(div(id = "emotion_ans", 
                           p("Testing10"))),
                
                hidden(div(id = "open_ans", 
                           p("Testing11")))
           ))
 })
})


ans.vars <- c("selfeff_ans", "selfest_ans", "lifesat_ans", "distress_ans", "depress_ans", "trauma_ans",
              "tocope_ans", "emcope_ans", "avoid_ans", "extra_ans", "agree_ans", "consc_ans", "emotion_ans", "open_ans")

observeEvent(input$selfeff_button, {
 shinyjs::toggle("selfeff_ans")
 for(i in setdiff(ans.vars, "selfeff_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$selfest_button, {
 shinyjs::toggle("selfest_ans")
 for(i in setdiff(ans.vars, "selfest_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$lifesat_button, {
 shinyjs::toggle("lifesat_ans")
 for(i in setdiff(ans.vars, "lifesat_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$distress_button, {
 shinyjs::toggle("distress_ans")
 for(i in setdiff(ans.vars, "distress_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$depress_button, {
 shinyjs::toggle("depress_ans")
 for(i in setdiff(ans.vars, "depress_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$trauma_button, {
 shinyjs::toggle("trauma_ans")
 for(i in setdiff(ans.vars, "trauma_ans")){
  shinyjs::hide(i)
 }
})

observeEvent(input$tocope_button, {
 shinyjs::toggle("tocope_ans")
 for(i in setdiff(ans.vars, "tocope_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$emcope_button, {
 shinyjs::toggle("emcope_ans")
 for(i in setdiff(ans.vars, "emcope_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$avoid_button, {
 shinyjs::toggle("avoid_ans")
 for(i in setdiff(ans.vars, "avoid_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$extra_button, {
 shinyjs::toggle("extra_ans")
 for(i in setdiff(ans.vars, "extra_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$agree_button, {
 shinyjs::toggle("agree_ans")
 for(i in setdiff(ans.vars, "agree_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$consc_button, {
 shinyjs::toggle("consc_ans")
 for(i in setdiff(ans.vars, "consc_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$emotion_button, {
 shinyjs::toggle("emotion_ans")
 for(i in setdiff(ans.vars, "emotion_ans")){
  shinyjs::hide(i)
 }
})
observeEvent(input$open_button, {
 shinyjs::toggle("open_ans")
 for(i in setdiff(ans.vars, "open_ans")){
  shinyjs::hide(i)
 }
})
