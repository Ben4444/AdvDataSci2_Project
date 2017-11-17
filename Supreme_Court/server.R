
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
decisions <- data.frame(matrix(ncol = 5, nrow = 2531))
colnames(decisions) <- c("topic", "case", "argued", "decided", "opinion")
library(rvest)
library(dplyr)
library(readr)
library(stringr)

cornell_url <- "https://www.law.cornell.edu/supct/cases/topic.htm"
cornell_html <- paste(readLines(cornell_url), collapse="\n")
cornell_links <- str_match_all(cornell_html, "<a href=\"/supct/cases/topics/(.*?)\"")
topics_url <- paste0("https://www.law.cornell.edu/supct/cases/topics/", cornell_links[[1]][,2])

cases <- read_csv("Supreme Court Decisions.csv")

decisions$topic <- cases$Topic

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
