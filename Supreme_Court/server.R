# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Loading in Supreme Court nominee data, obtained from http://epstein.wustl.edu/research/justicesdata.html
justicesdata <- read.csv("http://epstein.wustl.edu/research/justicesdata.csv")
#load("justicesdata.rdata.txt")

# Cutting the Supreme Court nominee data down to the variables of interest
justices <- subset(justicesdata, select=c("name", "yrnom", "posit", "recess", "success", "id", "analu", "birthcit",
                                          "birthst", "famses", "nomrelig", "race", "gender", "datenom", "datesen",
                                          "agenom", "stnom", "parnom", "prparnom", "presname", "prespart", "senparty",
                                          "serve", "dateserb", "npart", "nops", "judgment", "ndis", "nconr", "ncons", 
                                          "percrim", "ncrim", "percr", "ncr", "perfir", "nfir", "perunn", "nunn", "perecon",
                                          "necon", "perfed", "nfed", "perftax", "nftax", "datesere", "reasdep", "deathd"))

# Subsetting the Supreme Court nominee data down to people who actually served on the court (1 = Nominee confirmed and served, 777 = reccess appointment)
justices <- subset(justices, serve == 1 | serve == 777)

# Scraping data from the Cornell Law page, looking at Historic Supreme Court Decisions - by Topic, https://www.law.cornell.edu/supct/cases/topic.htm
decisions <- data.frame(matrix(ncol = 5, nrow = 2530))
colnames(decisions) <- c("topic", "case", "argued", "decided", "opinion")
library(rvest)
library(dplyr)
library(readr)
library(stringr)

cornell_url <- "https://www.law.cornell.edu/supct/cases/topic.htm"
cornell_html <- paste(readLines(cornell_url), collapse="\n")
cornell_links <- str_match_all(cornell_html, "<a href=\"/supct/cases/topics/(.*?)\"")
topics_url <- paste0("https://www.law.cornell.edu/supct/cases/topics/", cornell_links[[1]][,2])
topics <- str_replace(topics_url, pattern = ".*/tog_(.*?).html", replacement = "\\1")

cases <- read.csv("Supreme Court Decisions.csv")

# TOPIC: ABORTION #
# Note: A different website had to be used for Roe vs. Wade and Hodgson vs. Minnesota, as the syllabus was missing from the Cornell site
abortion_links <- c("Doe v. Bolton 410 u.s. 179 (1973)", "Bigelow v. Virginia 421 u.s. 809 (1975)",
                    "Planned Parenthood of Central Missouri v. Danforth 428 u.s. 52 (1976)",
                    "Beal v. Doe 432 u.s. 438 (1977)", "Maher v. Roe 432 u.s. 464 (1977)",
                    "Poelker v. Doe 432 u.s. 519 (1977)", "Harris v. McRae 448 u.s. 297 (1980)",
                    "Akron v. Akron Center for Reproductive Health, Inc. 462 u.s. 416 (1983)",
                    "Thornburgh v. American College of Obstetricians & Gynecologists 476 u.s. 747 (1986)",
                    "Frisby v. Schultz 487 u.s. 474 (1988)", "Webster v. Reproductive Health Services 492 u.s. 490 (1989)",
                    "Rust v. Sullivan 500 u.s. 173 (1991)",
                    "Planned Parenthood of Southeastern Pennsylvania v. Casey 505 u.s. 833 (1992)")

for (i in 1:length(abortion_links)){
  decisions$topic[i] <- topics[1]
}

for (i in 1:length(abortion_links)){
  decisions$case[i] <- html_session(topics_url[1]) %>%
    follow_link(abortion_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(abortion_links)){
  decisions$argued[i] <- html_session(topics_url[1]) %>%
    follow_link(abortion_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(abortion_links)){
  decisions$decided[i] <- html_session(topics_url[1]) %>%
    follow_link(abortion_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(abortion_links)){
  decisions$opinion[i] <- html_session(topics_url[1]) %>%
    follow_link(abortion_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

roeVwade <- read_html("https://supreme.justia.com/cases/federal/us/410/113/")
hodgsonVminnesota <- read_html("https://supreme.justia.com/cases/federal/us/497/417/")

decisions$topic[14] <- topics[1]
decisions$topic[15] <- topics[1]

decisions$case[14] <- roeVwade %>% html_node("h3+ p b") %>% html_text()
decisions$case[15] <- hodgsonVminnesota %>% html_node("h3+ p b") %>% html_text()

decisions$argued[14] <- roeVwade %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[15] <- hodgsonVminnesota %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[14] <- roeVwade %>% html_node("p:nth-child(7) b") %>% html_text()
decisions$decided[15] <- hodgsonVminnesota %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[14] <- roeVwade %>% html_node("p:nth-child(29)") %>% html_text()
decisions$opinion[15] <- hodgsonVminnesota %>% html_node("p:nth-child(29)") %>% html_text()


# TOPIC: AFFIRMATIVE ACTION #
# Note: A different website had to be used for Wygant vs. Jackson Board of Education, United States vs. Paradise, and City of Richmond vs. J.A. Croson Co., as the syllabus was missing from the Cornell site
affirmative_action_links <- c("Regents of the Univ. of Cal. v. Bakke 438 u.s. 265 (1978)",
                              "United Steelworkers of America, AFL-CIO-CLC v. Weber 443 u.s. 193 (1979)",
                              "Fullilove v. Klutznick 448 u.s. 448 (1980)",
                              "Mississippi University for Women v. Hogan 458 u.s. 718 (1982)",
                              "Building Trades & Construction Trades Council of Camden County and Vicinity v. Mayor and Council of the City of Camden 465 u.s. 208 (1984)",
                              "Firefighters Local Union No. 1784 v. Stotts 467 u.s. 561 (1984)",
                              "Johnson v. Transportation Agency 480 u.s. 616 (1987)",
                              "Metro Broadcasting, Inc. v. Federal Communications Commission 497 u.s. 547 (1990)",
                              "Adarand Constructors, Inc. v. Pena 515 u.s. 200 (1995)")

for (i in 1:length(affirmative_action_links)){
  decisions$topic[i+15] <- topics[2]
}

for (i in 1:length(affirmative_action_links)){
  decisions$case[i+15] <- html_session(topics_url[2]) %>%
    follow_link(affirmative_action_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(affirmative_action_links)){
  decisions$argued[i+15] <- html_session(topics_url[2]) %>%
    follow_link(affirmative_action_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(affirmative_action_links)){
  decisions$decided[i+15] <- html_session(topics_url[2]) %>%
    follow_link(affirmative_action_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(affirmative_action_links)){
  decisions$opinion[i+15] <- html_session(topics_url[2]) %>%
    follow_link(affirmative_action_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

wygantVjbe <- read_html("https://supreme.justia.com/cases/federal/us/476/267/")
usVparadise <- read_html("https://supreme.justia.com/cases/federal/us/480/149/")
corVjacc <- read_html("https://supreme.justia.com/cases/federal/us/488/469/")

decisions$topic[25] <- topics[2]
decisions$topic[26] <- topics[2]
decisions$topic[27] <- topics[2]

decisions$case[25] <- wygantVjbe %>% html_node("h3+ p b") %>% html_text()
decisions$case[26] <- usVparadise %>% html_node("h3+ p") %>% html_text()
decisions$case[27] <- corVjacc %>% html_node("h3+ p b") %>% html_text()

decisions$argued[25] <- wygantVjbe %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[26] <- usVparadise %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[27] <- corVjacc %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[25] <- wygantVjbe %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[26] <- usVparadise %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[27] <- corVjacc %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[25] <- wygantVjbe %>% html_node("p:nth-child(22)") %>% html_text()
decisions$opinion[27] <- corVjacc %>% html_node("p:nth-child(44)") %>% html_text()

# TOPIC: ALIENS #
# Note: A different website had to be used for Graham vs. Dept. of Public Welfare, as the syllabus was missing from the Cornell site
aliens_links <- c("Truax v. Raich 239 u.s. 33 (1915)",
                  "United States v. Pink 315 u.s. 203 (1942)",
                  "Hampton v. Mow Sun Wong 426 u.s. 88 (1976)",
                  "Ambach v. Norwick 441 u.s. 68 (1979)",
                  "Fullilove v. Klutznick 448 u.s. 448 (1980)",
                  "Cabell v. Chavez-Salido 454 u.s. 432 (1982)",
                  "Plyler v. Doe 457 u.s. 202 (1982)",
                  "INS v. Chadha 462 u.s. 919 (1983)")

for (i in 1:length(aliens_links)){
  decisions$topic[i+27] <- topics[3]
}

for (i in 1:length(aliens_links)){
  decisions$case[i+27] <- html_session(topics_url[3]) %>%
    follow_link(aliens_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(aliens_links)){
  decisions$argued[i+27] <- html_session(topics_url[3]) %>%
    follow_link(aliens_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(aliens_links)){
  decisions$decided[i+27] <- html_session(topics_url[3]) %>%
    follow_link(aliens_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(aliens_links)){
  decisions$opinion[i+27] <- html_session(topics_url[3]) %>%
    follow_link(aliens_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

grahamVdpw <- read_html("https://supreme.justia.com/cases/federal/us/403/365/")

decisions$topic[36] <- topics[3]

decisions$case[36] <- grahamVdpw %>% html_node("h3+ p b") %>% html_text()

decisions$argued[36] <- grahamVdpw %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[36] <- grahamVdpw %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[36] <- grahamVdpw %>% html_node("p:nth-child(12)") %>% html_text()

# TOPIC: ARMED SERVICES #
# Note: A different website had to be used for Johnson vs. Robison, Schlesinger vs. Reservists Committee to Stop the War, and Schick vs. Reed, as the syllabus was missing from the Cornell site
armed_services_links <- c("Welsh v. United States 398 u.s. 333 (1970)",
                          "Rostker v. Goldberg 453 u.s. 57 (1981)")

for (i in 1:length(armed_services_links)){
  decisions$topic[i+36] <- topics[4]
}

for (i in 1:length(armed_services_links)){
  decisions$case[i+36] <- html_session(topics_url[4]) %>%
    follow_link(armed_services_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(armed_services_links)){
  decisions$argued[i+36] <- html_session(topics_url[4]) %>%
    follow_link(armed_services_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(armed_services_links)){
  decisions$decided[i+36] <- html_session(topics_url[4]) %>%
    follow_link(armed_services_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(armed_services_links)){
  decisions$opinion[i+36] <- html_session(topics_url[4]) %>%
    follow_link(armed_services_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

johnsonVrobison <- read_html("https://supreme.justia.com/cases/federal/us/415/361/")
schlesingerVrcsw <- read_html("https://supreme.justia.com/cases/federal/us/418/208/")
schickVreed <- read_html("https://supreme.justia.com/cases/federal/us/419/256/")

decisions$topic[39] <- topics[4]
decisions$topic[40] <- topics[4]
decisions$topic[41] <- topics[4]

decisions$case[39] <- johnsonVrobison %>% html_node("h3+ p b") %>% html_text()
decisions$case[40] <- schlesingerVrcsw %>% html_node("h3+ p b") %>% html_text()
decisions$case[41] <- schickVreed %>% html_node("h3+ p b") %>% html_text()

decisions$argued[39] <- johnsonVrobison %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[40] <- schlesingerVrcsw %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[41] <- schickVreed %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[39] <- johnsonVrobison %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[40] <- schlesingerVrcsw %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[41] <- schickVreed %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[39] <- johnsonVrobison %>% html_node("p:nth-child(23)") %>% html_text()
decisions$opinion[40] <- schlesingerVrcsw %>% html_node("p:nth-child(15)") %>% html_text()
decisions$opinion[41] <- schickVreed %>% html_node("p:nth-child(15)") %>% html_text()

# TOPIC: ATTAINDER #
# Note: A different website had to be used for U.S. vs. Lovett, American Communications Assn. vs. Douds, and Nixon vs. Administrator of General Services, as the syllabus was missing from the Cornell site
attainder_links <- c("Barenblatt v. United States 360 u.s. 109 (1959)",
                     "Communist Party of the United States v. Subversive Activities Control Bd. No. 12 367 u.s. 1 (1961)",
                     "Aptheker v. Secretary of State 378 u.s. 500 (1964)",
                     "United States v. Brown 381 u.s. 437 (1965)")

for (i in 1:length(attainder_links)){
  decisions$topic[i+41] <- topics[5]
}

for (i in 1:length(attainder_links)){
  decisions$case[i+41] <- html_session(topics_url[5]) %>%
    follow_link(attainder_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(attainder_links)){
  decisions$argued[i+41] <- html_session(topics_url[5]) %>%
    follow_link(attainder_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(attainder_links)){
  decisions$decided[i+41] <- html_session(topics_url[5]) %>%
    follow_link(attainder_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(attainder_links)){
  decisions$opinion[i+41] <- html_session(topics_url[5]) %>%
    follow_link(attainder_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

usVlovett <- read_html("https://supreme.justia.com/cases/federal/us/328/303/case.html")
acaVdouds <- read_html("https://supreme.justia.com/cases/federal/us/339/382/case.html")
nixonVags <- read_html("https://supreme.justia.com/cases/federal/us/433/425/")

decisions$topic[46] <- topics[5]
decisions$topic[47] <- topics[5]
decisions$topic[48] <- topics[5]

decisions$case[46] <- usVlovett %>% html_node("h3+ p b") %>% html_text()
decisions$case[47] <- acaVdouds %>% html_node("h3+ p b") %>% html_text()
decisions$case[48] <- nixonVags %>% html_node("h3+ p b") %>% html_text()

decisions$argued[46] <- usVlovett %>% html_node("p:nth-child(4) b") %>% html_text()
decisions$argued[47] <- acaVdouds %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[48] <- nixonVags %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[46] <- usVlovett %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$decided[47] <- acaVdouds %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[48] <- nixonVags %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[46] <- usVlovett %>% html_node("p:nth-child(22)") %>% html_text()
decisions$opinion[47] <- acaVdouds %>% html_node("p:nth-child(29)") %>% html_text()
decisions$opinion[48] <- nixonVags %>% html_node("p:nth-child(34)") %>% html_text()

# TOPIC: ATTORNEYS #
# Note: A different website had to be used for Butz vs. Economou, as the syllabus was missing from the Cornell site
attorneys_links <- c("Ex Parte Garland 71 u.s. 333 (1866)",
                     "In re Primus 436 u.s. 412 (1978)",
                     "Supreme Court of New Hampshire v. Piper 470 u.s. 274 (1985)",
                     "Nix v. Whiteside 475 u.s. 157 (1986)",
                     "Phillips v. Washington Legal Foundation 521 u.s. 117 (1998)")

for (i in 1:length(attorneys_links)){
  decisions$topic[i+48] <- topics[6]
}

for (i in 1:length(attorneys_links)){
  decisions$case[i+48] <- html_session(topics_url[6]) %>%
    follow_link(attorneys_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(attorneys_links)){
  decisions$argued[i+48] <- html_session(topics_url[6]) %>%
    follow_link(attorneys_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(attorneys_links)){
  decisions$decided[i+48] <- html_session(topics_url[6]) %>%
    follow_link(attorneys_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(attorneys_links)){
  decisions$opinion[i+48] <- html_session(topics_url[6]) %>%
    follow_link(attorneys_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

butzVeconomou <- read_html("https://supreme.justia.com/cases/federal/us/438/478/")

decisions$topic[54] <- topics[6]

decisions$case[54] <- butzVeconomou %>% html_node("h3+ p b") %>% html_text()

decisions$argued[54] <- butzVeconomou %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[54] <- butzVeconomou %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[54] <- butzVeconomou %>% html_node("p:nth-child(25)") %>% html_text()

# TOPIC: BANKRUPTCY #



































library(shiny)
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
