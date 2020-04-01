
# Individual Data Analysis

# Player information
# browser()
DEMO_DF <- data.frame(
  var = c("Name", "Age", "Date of Birth", "Ethnicity", "Religion", "Locating From", "Locating To", "Date of Assessment"),
  val = c(input$SelectPlayer,
          as.character(floor(as.numeric(IPLDAT$Admin_stdate - IPLDAT$Demo_dob) / 365.25)),
          as.character(format(IPLDAT$Demo_dob, "%d-%m-%Y")),
          IPLDAT$Demo_ethOTH,
          IPLDAT$Demo_relig2,
          IPLDAT$Demo_reloc2,
          IPLDAT$Demo_dest,
          as.character(format(IPLDAT$Admin_stdate, "%d-%m-%Y")))
)

names(DEMO_DF) <- c(" ","  ")


# Mental Health & Wellbeing
MH_WELLB <- list(
  selfeff = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Selfeff"]], 1, sum)),
  selfest = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Selfest"]], 1, sum)),
  satisf  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Satisf"]], 1, sum)),
  psyDist = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "PsyDist"]], 1, sum)),
  depress = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Depress"]], 1, sum)),
  trauma  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Trauma"]], 1, sum)),
  cult_au = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Culture_AU"]], 1, sum)),
  cult_nz = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Culture_NZ"]], 1, sum)),
  cult_ma = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Culture_MA"]], 1, sum)),
  cult_mb = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Culture_MB"]], 1, sum)),
  cult_pa = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Culture_PA"]], 1, sum)),
  cult_ab = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Culture_AB"]], 1, sum))
)

MHWB_DF <- data.frame(
  Scale = c("Self-efficacy", "Self-esteem", "Life Satisfaction", "Psychological Distress", "Depressive Screen", "Prior Trauma", 
          "Cultural Heritage (AUS)", "Cultural Heritage (NZ)", "Cultural Heritage (Maori1)", "Cultural Heritage (Maori2)", "Cultural Heritage (Pacific)", "Cultural Heritage (Aboriginal)"),
  Score = unname(unlist(MH_WELLB)),
  ScMax = c(56, 70, 80, 24, 27, 30, 25, 25, 25, 25, 25, 25)
)

MHWB_DF <- MHWB_DF[-which(MHWB_DF$Score==0 & grepl("Cultural", MHWB_DF$Scale)), ]
MHWB_DF$Pcn <- paste0(round(MHWB_DF$Score / MHWB_DF$ScMax * 100), "%")

row.names(MHWB_DF) <- NULL

MHWB_DF <- MHWB_DF 

# Plot
# MHWB_Plot <- plot_ly(
#   x = factor(MHWB_DF$Scale, levels = MHWB_DF[["Scale"]]), #default order will be alphabetized unless specified
#   y = round(MHWB_DF$Score / MHWB_DF$ScMax * 100),
#   type = "bar",
#   text = MHWB_DF$Pcn,
#   textposition = 'auto',
#   hoverinfo="none",
#   marker = list(color = ifelse(round(MHWB_DF$Score / MHWB_DF$ScMax * 100) <50,
#                                "orangered", "limegreen"))) %>%
#   layout(yaxis = list(range = c(0, 100))) %>% 
#   config(displayModeBar = F) 

MHWB_val <- round(MHWB_DF$Score / MHWB_DF$ScMax * 100)
MHWB_names <- c("Self-eff.", "Self-est.", "Life-sat.", "Kessler-6", "PHQ9", "Trauma")

# browser()

MHWB_names <- if(all(is.na(MHWB_DF$Scale[7:9]))){
  MHWB_names
} else if(!all(is.na(MHWB_DF$Scale[7:9]))) {
  c(MHWB_names, paste0("CH-", gsub("Cultural Heritage \\(|\\)", "", 
                    na.omit(MHWB_DF$Scale[7:9]))))
} else if(sum(is.na(MHWB_DF$Scale[7:9])) == 1) {
  c(MHWB_names, paste0("CH-", gsub("Cultural Heritage \\(|\\)", "", 
                                   na.omit(MHWB_DF$Scale[7:9])[1:2])))
} else {
  c(MHWB_names, paste0("CH-", gsub("Cultural Heritage \\(|\\)", "", 
                                   na.omit(MHWB_DF$Scale[7:9])[1])))
}


# Coping Profile
COPING_PF <- list(
  cope_to = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Coping_T"]], 1, sum)),
  cope_eo = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Coping_E"]], 1, sum)),
  cope_ao = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Coping_A"]], 1, sum)),
  ipip_ex  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "IPIP_Ex"]], 1, sum)),
  ipip_ag  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "IPIP_Ag"]], 1, sum)),
  ipip_co  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "IPIP_Con"]], 1, sum)),
  ipip_em  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "IPIP_Em"]], 1, sum)),
  ipip_op  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "IPIP_Op"]], 1, sum)),
  alcohol  = unname(apply(IPLDAT[KEY$Varname[KEY$Subscale == "Alcohol"]], 1, sum))
)


COPING_DF <- data.frame(
  Scale = c("Task-Orientated Coping", "Emotion-Orientated Coping", "Avoidance-Orientated Coping", "Extraversion", "Agreeableness", "Conscientiousness", "Emotionality", "Openness to Experience", "Alcohol Use"),
  Score = unname(unlist(COPING_PF)),
  ScMax = c(25, 25, 25, 28, 28, 28, 28, 28, 40)
)

COPING_DF$Pcn <- paste0(round(COPING_DF$Score / COPING_DF$ScMax * 100), "%")

row.names(COPING_DF) <- NULL

# Plot
Coping.val   <- round(COPING_DF$Score / COPING_DF$ScMax * 100)
Coping.names <- c("Task/O Cope", "Emo/O Cope", "Avoid/O Cope", "Extravs", "Agreebns", "Consciens", "Emotion", "Openness", "Alcohol")


# Relocation Assessment
RELOC_DF <- data.frame(
  Scale = c("Homesickness", "Get along without family", "Get along without friends", "Fine on my own", "Take a while to adjust", "Family be fine without me", 
            "Treated the same", "Able to find way around", "Ask for help", "Quickly learn things", "Assimilation", "Integration", "Separation"),
  Score = unname(unlist(IPLDAT[KEY$Varname[KEY$Subscale == "Reloc"]])),
  ScMax = rep(7, 13)
)

RELOC_DF$Pcn <- paste0(round(RELOC_DF$Score / RELOC_DF$ScMax * 100), "%")

row.names(RELOC_DF) <- NULL

# Plot
Reloc.val   <- round(RELOC_DF$Score / RELOC_DF$ScMax * 100)
Reloc.names <- c("Homesick", "No Family", "No Friends", "Fine Own", "While Adj", "Fam Fine", "Treat Same", "Find Way", "Ask Help", "Quick Learn", "Assim", "Integ", "Separate")

# # Red Flags
# RED_FLAGS <- list(
#   psydist  = MH_WELLB$psyDist,
#   depress  = MH_WELLB$depress,
#   alcohol  = COPING_PF$alcohol,
#   trauma   = MH_WELLB$trauma,
#   hopeless = IPLDAT$PsyDist_1,
#   selfharm = IPLDAT$Depress_9
# )
# 
# RED_FLAGS$anyredflag <- +(RED_FLAGS$psydist >= 12 |
#                             RED_FLAGS$depress >= 5 |
#                             RED_FLAGS$alcohol >= 16 |
#                             RED_FLAGS$trauma >= 14 |
#                             RED_FLAGS$hopeless > 0 |
#                             RED_FLAGS$selfharm > 0)


