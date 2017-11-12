
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Loading in Supreme Court nominee data, obtained from http://epstein.wustl.edu/research/justicesdata.html
load("justicesdata.rdata.txt")

# Cutting the Supreme Court nominee data down to the variables of interest
justices <- subset(justicesdata, select=c("name", "yrnom", "posit", "recess", "success", "id", "analu", "birthcit",
                                          "birthst", "famses", "nomrelig", "race", "gender", "datenom", "datesen",
                                          "agenom", "stnom", "parnom", "prparnom", "presname", "prespart", "senparty",
                                          "serve", "dateserb", "npart", "nops", "judgment", "ndis", "nconr", "ncons", 
                                          "percrim", "ncrim", "percr", "ncr", "perfir", "nfir", "perunn", "nunn", "perecon",
                                          "necon", "perfed", "nfed", "perftax", "nftax", "datesere", "reasdep", "deathd"))

# Subsetting the Supreme Court nominee data down to people who actually served on the court
justices <- subset(justices, serve == 1 | serve == 777)

# Scraping data from the Cornell Law page, looking at Historic Supreme Court Decisions - by Topic, https://www.law.cornell.edu/supct/cases/topic.htm
decisions <- data.frame(matrix(ncol = 5, nrow = 2531))
colnames(decisions) <- c("topic", "case", "argued", "decided", "opinion")
library(rvest)
library(dplyr)
library(readr)

cornell_url <- "https://www.law.cornell.edu/supct/cases/topic.htm"
topic_links <- c("Abortion", "Affirmative Action", "Aliens", "Armed Services", "Attainder", "Attorneys", "Bankruptcy",
                 "Bill of Rights", "Birth Control", "Borders", "Capital Punishment", "Censorship", "Children",
                 "Choice of Law", "Citizenship", "Civil Rights", "Commander in Chief", "Commerce Clause", 
                 "Commercial Speech", "Communism", "Confessions", "Conflict of Laws", "Congress", "Contract Clause", 
                 "Courts", "Criminal Law", "Criminal Procedure", "Cruel and Unusual Punishment", "Damages",
                 "Discrimination", "Discrimination Based on Nationality", "Double Jeopardy", "Due Process", "Education",
                 "Eighth Amendment", "Elections", "Eleventh Amendment", "Eminent Domain", "Employment", "Environment",
                 "Equal Protection", "Establishment of Religion", "Evidence", "Executive Power", "Executive Privilege",
                 "Extradition", "Federal Courts", "Federalism", "Fifth Amendment", "Fighting Words", "First Amendment",
                 "Flag Desecration", "Foreign Affairs", "Forum", "Fourteenth Amendment", "Fourth Amendment", 
                 "Freedom of Assembly", "Freedom of Association", "Freedom of Religion", "Freedom of Speech",
                 "Freedom of the Press", "Full Faith and Credit", "Gender", "Government Employment", "Habeas Corpus",
                 "Handicapped", "Housing", "Immunity", "Implied Powers", "Import Tariffs", "Incorporation", "Indians",
                 "Insanity", "International Law", "International Relations", "Internet", "Investigations", 
                 "Involuntary Servitude", "Judicial Review", "Jurisdiction", "Jury", "Justiciability", "Juveniles",
                 "Labor", "Legislative Policy", "Libel", "Marriage", "Mental Health", "Mental Retardation", 
                 "Minimum Contacts", "Monopoly", "National Power", "National Security", "Necessary and Proper", "New Deal",
                 "Ninth Amendment", "Obscenity", "Pardon", "Pensions", "Pledge of Loyalty", "Police Power",
                 "Political Questions", "Political Speech", "Power to Tax and Spend", "Precedent", "Presidency", "Prisons",
                 "Privacy", "Privileges and Immunities", "Property", "Race", "Racial Discrimination", "Reapportionment",
                 "Regulation", "Removal Power", "Reproduction", "Res Judicata", "Right to a Hearing", "Right to Bear Arms",
                 "Right to Confront Witnesses", "Right to Counsel", "Right to Travel", "Searches and Seizures", 
                 "Second Amendment", "Sedition", "Segregation", "Self-Incrimination", "Separation of Power",
                 "Sex Discrimination", "Sexuality", "Sixth Amendment", "Slavery", "Social Security", "Standing", 
                 "State Action", "States", "Sterilization", "Supremacy Clause", "Symbolic Speech", "Takings Clause",
                 "Taxation", "Tenth Amendment", "Testimony", "Thirteenth Amendment", "Trial by Jury", "Veto", "Voting",
                 "War Powers", "Welfare Benefits", "Wiretapping", "Witnesses")

cases <- read_csv("Supreme Court Decisions.csv")

decisions$topic <- cases$Topic

# TOPIC: ABORTION #
# Note: A different website had to be used for Roe vs. Wade and Hodgson vs. Minnesota, as the syllabus appears to have been removed from the Cornell site
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
  decisions$case[i] <- html_session(cornell_url) %>%
    follow_link(topic_links[1]) %>%
    follow_link(abortion_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(abortion_links)){
  decisions$argued[i] <- html_session(cornell_url) %>%
    follow_link(topic_links[1]) %>%
    follow_link(abortion_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(abortion_links)){
  decisions$decided[i] <- html_session(cornell_url) %>%
    follow_link(topic_links[1]) %>%
    follow_link(abortion_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(abortion_links)){
  decisions$opinion[i] <- html_session(cornell_url) %>%
    follow_link(topic_links[1]) %>%
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
