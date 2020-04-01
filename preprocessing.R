

# Preprocessing
names(RV$DAT) <- KEY$Varname

# Data Type
num.vars <- names(RV$DAT)[which(!names(RV$DAT) %in% c("Admin_stdate", "Demo_fname", "Demo_lname", "Demo_dob", "Demo_club2", "Demo_ethOTH", "Demo_relig2", "Demo_reloc3", "Culture_2"))]

RV$DAT[num.vars] <- lapply(RV$DAT[num.vars], function(x) 
  as.numeric(replace(x, is.na(x), 0)))

# Clean DOB: Check for day/month reversal
dob_list <- strsplit(RV$DAT$Demo_dob, split = "/")
dob_list <- lapply(dob_list, function(x){
  
  x <- if(as.numeric(x[2]) >= 13){
    replace(x, 1:2, x[2:1])
  } else {
      x
  }
  
  x <- if(substr(x[3], 1, 2) == "00"){
    replace(x, 3, gsub("00", "19", x[3]))
  } else {
    x
  }
  
  x <- paste0(x, collapse = "/")
  
  return(x)
  
})

RV$DAT$Demo_dob <- as.Date(unlist(dob_list), "%d/%m/%Y")
RV$DAT$Admin_stdate <- as.Date(as.POSIXct(RV$DAT$Admin_stdate))

# Reverse Scoring
rev.vars <- c("Selfest_3", "Selfest_5", "Selfest_8", "Selfest_9", "Selfest_10", "IPIP_Ex2", "IPIP_Ag2", "IPIP_Em2", "IPIP_Op2", "IPIP_Ex3", "IPIP_Con3", 
              "IPIP_Op3", "IPIP_Ag4", "IPIP_Con4", "IPIP_Em4", "IPIP_Op4")

RV$DAT[rev.vars] <- lapply(RV$DAT[rev.vars], function(x)
  dplyr::case_when(
    x == 1 ~ 7,
    x == 2 ~ 6,
    x == 3 ~ 5,
    x == 4 ~ 4,
    x == 5 ~ 3,
    x == 6 ~ 2,
    x == 7 ~ 1,
    x == 0 ~ 0)
)

# Red Flag Identification
RV$DAT$psydist <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "PsyDist"]], 1, sum))
RV$DAT$depress <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Depress"]], 1, sum))
RV$DAT$alcohol <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Alcohol"]], 1, sum))
RV$DAT$trauma  <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Trauma"]], 1, sum))

redflag.vars <- c("PsyDist_1", "Depress_9", "psydist", "depress", "alcohol", "trauma")

RV$DAT$Redflag <- apply(RV$DAT[redflag.vars], 1, function(x){
  
  +(x["psydist"] >= 12 |
      x["depress"] >= 5 |
      x["alcohol"] >= 16 |
      x["trauma"] >= 14 |
      x["PsyDist_1"] > 0 |
      x["Depress_9"] > 0)
  
})

# Decoding
RV$DAT$Demo_lg <- ifelse(RV$DAT$Demo_lg==1, "Mens", "Womens")

RV$DAT$Demo_relig2[is.na(RV$DAT$Demo_relig2)] <- "No Religion"

RV$DAT$Demo_reloc2 <- dplyr::case_when(
  RV$DAT$Demo_reloc1 == 1 & RV$DAT$Demo_reloc2 == 0 ~ "No Relocation",
  RV$DAT$Demo_reloc2 == 1 ~ "New Zealand",
  RV$DAT$Demo_reloc2 == 2 ~ "NSW",
  RV$DAT$Demo_reloc2 == 3 ~ "NSW Country",
  RV$DAT$Demo_reloc2 == 4 ~ "Queensland",
  RV$DAT$Demo_reloc2 == 5 ~ "Queensland Country",
  RV$DAT$Demo_reloc2 == 0 ~ RV$DAT$Demo_reloc3
)

eth.vars <- c("Demo_ethAU", "Demo_ethNZ", "Demo_ethAB", "Demo_ethM", "Demo_ethTS", "Demo_ethTI", "Demo_ethSM", "Demo_ethTG", "Demo_ethCIM", 
              "Demo_ethFJ", "Demo_ethNI")

RV$DAT[eth.vars] <- lapply(RV$DAT[eth.vars], function(x)
  dplyr::case_when(
    x == 1 ~ "Australian European",
    x == 2 ~ "NZ European",
    x == 3 ~ "Aboriginal",
    x == 4 ~ "Maori",
    x == 5 ~ "Torres Strait Islander",
    x == 6 ~ "Thursday Islander",
    x == 7 ~ "Samoan",
    x == 8 ~ "Tongan",
    x == 9 ~ "Cook Island Maori",
    x == 10 ~ "Fijian",
    x == 11 ~ "Niuean")
)

eth.varchar <- c(eth.vars, "Demo_ethOTH")

RV$DAT$Demo_ethOTH <- apply(RV$DAT[eth.varchar], 1, function(x)
  paste(x[!is.na(x)], collapse = " / ")
)

RV$DAT$Demo_club1 <- dplyr::case_when(
  RV$DAT$Demo_club1 == 1 ~ "Queensland Cowboys",
  RV$DAT$Demo_club1 == 2 ~ "Manly Sea Eagles",
  RV$DAT$Demo_club1 == 3 ~ "South Sydney Rabbitohs",
  RV$DAT$Demo_club1 == 4 ~ "Sydney City Roosters",
  RV$DAT$Demo_club1 == 5 ~ "Gold Coast Titans",
  RV$DAT$Demo_club1 == 6 ~ "Wests Tigers",
  RV$DAT$Demo_club1 == 7 ~ "Penrith Panthers",
  RV$DAT$Demo_club1 == 8 ~ "Cronulla Sharks",
  RV$DAT$Demo_club1 == 9 ~ "Parramatta Eels",
  RV$DAT$Demo_club1 == 10 ~ "New Zealand Warriors",
  RV$DAT$Demo_club1 == 11 ~ "Melbourne Storm",
  RV$DAT$Demo_club1 == 12 ~ "Canterbury Bankstown Bulldogs",
  RV$DAT$Demo_club1 == 13 ~ "Brisbane Broncos",
  RV$DAT$Demo_club1 == 14 ~ "Saint George Illawarra Dragons",
  RV$DAT$Demo_club1 == 15 ~ "Newcastle Knights",
  RV$DAT$Demo_club1 == 16 ~ "Canberra Raiders",
  RV$DAT$Demo_club1 == 0 ~ RV$DAT$Demo_club2
)


RV$DAT$Demo_dest <- dplyr::case_when(
  RV$DAT$Demo_club1 == "Queensland Cowboys" ~ "Queensland",
  RV$DAT$Demo_club1 == "Manly Sea Eagles" ~ "NSW",
  RV$DAT$Demo_club1 == "South Sydney Rabbitohs" ~ "NSW",
  RV$DAT$Demo_club1 == "Sydney City Roosters" ~ "NSW",
  RV$DAT$Demo_club1 == "Gold Coast Titans" ~ "Queensland",
  RV$DAT$Demo_club1 == "Wests Tigers" ~ "NSW",
  RV$DAT$Demo_club1 == "Penrith Panthers" ~ "NSW",
  RV$DAT$Demo_club1 == "Cronulla Sharks" ~ "NSW",
  RV$DAT$Demo_club1 == "Parramatta Eels" ~ "NSW",
  RV$DAT$Demo_club1 == "New Zealand Warriors" ~ "New Zealand",
  RV$DAT$Demo_club1 == "Melbourne Storm" ~ "Victoria",
  RV$DAT$Demo_club1 == "Canterbury Bankstown Bulldogs" ~ "NSW",
  RV$DAT$Demo_club1 == "Brisbane Broncos" ~ "Queensland",
  RV$DAT$Demo_club1 == "Saint George Illawarra Dragons" ~ "NSW",
  RV$DAT$Demo_club1 == "Newcastle Knights" ~ "NSW",
  RV$DAT$Demo_club1 == "Canberra Raiders" ~ "Canberra",
  RV$DAT$Demo_club1 == RV$DAT$Demo_club2 ~ "Other"
)

# Meta Data

sample.n <- length(unique(RV$DAT$Admin_rid))
cur.date <- as.Date(Sys.time())

RV$DAT$Res_age <- floor(as.numeric(RV$DAT$Admin_stdate - RV$DAT$Demo_dob) / 365.25)
RV$DAT$Res_selfeff <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Selfeff"]], 1, sum))
RV$DAT$Res_selfest <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Selfest"]], 1, sum))
RV$DAT$Res_satisf  <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Satisf"]], 1, sum))
RV$DAT$Res_psyDist <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "PsyDist"]], 1, sum))
RV$DAT$Res_depress <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Depress"]], 1, sum))
RV$DAT$Res_trauma  <- unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Trauma"]], 1, sum))

RV$DAT$Res_cope_to = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Coping_T"]], 1, sum))
RV$DAT$Res_cope_eo = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Coping_E"]], 1, sum))
RV$DAT$Res_cope_ao = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Coping_A"]], 1, sum))
RV$DAT$Res_ipip_ex  = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "IPIP_Ex"]], 1, sum))
RV$DAT$Res_ipip_ag  = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "IPIP_Ag"]], 1, sum))
RV$DAT$Res_ipip_co  = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "IPIP_Con"]], 1, sum))
RV$DAT$Res_ipip_em  = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "IPIP_Em"]], 1, sum))
RV$DAT$Res_ipip_op  = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "IPIP_Op"]], 1, sum))
RV$DAT$Res_alcohol  = unname(apply(RV$DAT[KEY$Varname[KEY$Subscale == "Alcohol"]], 1, sum))

METADAT_n <- data.frame(
  var = c("Respondents", "Last 7 Days", "Rookie", "Relocating", "Women"),
  n   = c(sample.n,
          length(unique(RV$DAT$Admin_rid[RV$DAT$Admin_stdate - cur.date >= -7])),
          sum(RV$DAT$Demo_rookie==2),
          sum(RV$DAT$Demo_reloc1==2),
          sum(RV$DAT$Demo_lg == "Womens")),
  pcn = round(c(sample.n / sample.n * 100,
                length(unique(RV$DAT$Admin_rid[RV$DAT$Admin_stdate - cur.date >= -7]) / sample.n * 100),
                sum(RV$DAT$Demo_rookie==2)/sample.n * 100,
                sum(RV$DAT$Demo_reloc1==2)/sample.n * 100,
                sum(RV$DAT$Demo_lg == "Womens")/sample.n * 100))
)

METADAT_n$pcn <- paste0(METADAT_n$pcn, "%")
names(METADAT_n)[1] <- " "


METADAT_MH <- data.frame(

  var = c("Age at Assessment", "Self-Efficacy", "Self-Esteem", "Life Satisfaction", "Psych. Distress", "Depressive Screen", "Trauma"),
  min = c(min(RV$DAT$Res_age), min(RV$DAT$Res_selfeff), min(RV$DAT$Res_selfest), min(RV$DAT$Res_satisf), min(RV$DAT$Res_psyDist), min(RV$DAT$Res_depress), min(RV$DAT$Res_trauma)),
  mean = round(c(mean(RV$DAT$Res_age), mean(RV$DAT$Res_selfeff), mean(RV$DAT$Res_selfest), mean(RV$DAT$Res_satisf), mean(RV$DAT$Res_psyDist), mean(RV$DAT$Res_depress), mean(RV$DAT$Res_trauma))),
  max = c(max(RV$DAT$Res_age), max(RV$DAT$Res_selfeff), max(RV$DAT$Res_selfest), max(RV$DAT$Res_satisf), max(RV$DAT$Res_psyDist), max(RV$DAT$Res_depress), max(RV$DAT$Res_trauma))

)
names(METADAT_MH)[1] <- " "

METADAT_COPE <- data.frame(

  var = c("Task/O Coping", "Emotion/O Coping", "Avoidance/O Coping", "Extraversion", "Agreeableness", "Conscientiousness", "Emotionality", "Openness", "Alcohol Use"),
  min = c(min(RV$DAT$Res_cope_to), min(RV$DAT$Res_cope_eo), min(RV$DAT$Res_cope_ao), min(RV$DAT$Res_ipip_ex), min(RV$DAT$Res_ipip_ag), min(RV$DAT$Res_ipip_co), min(RV$DAT$Res_ipip_em), min(RV$DAT$Res_ipip_op), min(RV$DAT$Res_alcohol)),
  mean = round(c(mean(RV$DAT$Res_cope_to), mean(RV$DAT$Res_cope_eo), mean(RV$DAT$Res_cope_ao), mean(RV$DAT$Res_ipip_ex), mean(RV$DAT$Res_ipip_ag), mean(RV$DAT$Res_ipip_co), mean(RV$DAT$Res_ipip_em), mean(RV$DAT$Res_ipip_op), mean(RV$DAT$Res_alcohol))),
  max = c(max(RV$DAT$Res_cope_to), max(RV$DAT$Res_cope_eo), max(RV$DAT$Res_cope_ao), max(RV$DAT$Res_ipip_ex), max(RV$DAT$Res_ipip_ag), max(RV$DAT$Res_ipip_co), max(RV$DAT$Res_ipip_em), max(RV$DAT$Res_ipip_op), max(RV$DAT$Res_alcohol))

)
names(METADAT_COPE)[1] <- " "


# Placeholder where needed
RV$EMPTY_DF <- data.frame(
  var = " "
)

names(RV$EMPTY_DF) <- " "