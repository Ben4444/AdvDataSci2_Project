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
topics <- cornell_links[[1]][,2]
topics <- unlist(strsplit(topics, split='tog_', fixed=TRUE))
toDelete <- seq(1, length(topics), 2)
topics <-  topics[-toDelete]
topics <- unlist(strsplit(topics, split='.html', fixed=TRUE))

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
# Note: A different website had to be used for Northern Pipeline Constr. Co. vs. Marathon Pipe Line Co., as the syllabus was missing from the Cornell site
npccVmplc <- read_html("https://supreme.justia.com/cases/federal/us/458/50/")

decisions$topic[55] <- topics[7]

decisions$case[55] <- npccVmplc %>% html_node("h3+ p b") %>% html_text()

decisions$argued[55] <- npccVmplc %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[55] <- npccVmplc %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[55] <- npccVmplc %>% html_node("p:nth-child(24)") %>% html_text()

# TOPIC: BILL-OF-RIGHTS #
bill_of_rights_links <- c("Barron v. Mayor & City Council of Baltimore 32 u.s. 243 (1833)")

for (i in 1:length(bill_of_rights_links)){
  decisions$topic[i+55] <- topics[8]
}

for (i in 1:length(bill_of_rights_links)){
  decisions$case[i+55] <- html_session(topics_url[8]) %>%
    follow_link(bill_of_rights_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(bill_of_rights_links)){
  decisions$argued[i+55] <- html_session(topics_url[8]) %>%
    follow_link(bill_of_rights_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(bill_of_rights_links)){
  decisions$decided[i+55] <- html_session(topics_url[8]) %>%
    follow_link(bill_of_rights_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(bill_of_rights_links)){
  decisions$opinion[i+55] <- html_session(topics_url[8]) %>%
    follow_link(bill_of_rights_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

# TOPIC: CONTRACEPTION #
contraception_links <- c("Buck v. Bell 274 u.s. 200 (1927)",
                         "Griswold v. Connecticut 381 u.s. 479 (1965)",
                         "Carey v. Population Services International 431 u.s. 678 (1977)",
                         "Bolger v. Youngs Drugs Prods. Corp. 463 u.s. 60 (1983)")

for (i in 1:length(contraception_links)){
  decisions$topic[i+56] <- topics[9]
}

for (i in 1:length(contraception_links)){
  decisions$case[i+56] <- html_session(topics_url[9]) %>%
    follow_link(contraception_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(contraception_links)){
  decisions$argued[i+56] <- html_session(topics_url[9]) %>%
    follow_link(contraception_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(contraception_links)){
  decisions$decided[i+56] <- html_session(topics_url[9]) %>%
    follow_link(contraception_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(contraception_links)){
  decisions$opinion[i+56] <- html_session(topics_url[9]) %>%
    follow_link(contraception_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

# TOPIC: BORDERS #
borders_links <- c("Foster & Elam v. Neilson 27 u.s. 253 (1829)")

for (i in 1:length(borders_links)){
  decisions$topic[i+60] <- topics[10]
}

for (i in 1:length(borders_links)){
  decisions$case[i+60] <- html_session(topics_url[10]) %>%
    follow_link(borders_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(borders_links)){
  decisions$argued[i+60] <- html_session(topics_url[10]) %>%
    follow_link(borders_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(borders_links)){
  decisions$decided[i+60] <- html_session(topics_url[10]) %>%
    follow_link(borders_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(borders_links)){
  decisions$opinion[i+60] <- html_session(topics_url[10]) %>%
    follow_link(borders_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

# TOPIC: CAPITAL PUNISHMENT #
# Note: A different website had to be used for Witherspoon vs. Illinois, Bumper vs. North Carolina, Schick vs. Reed, Jurek vs. Texas, Eddings vs. Oklahoma, Booth vs. Maryland, and Atkins vs. Virginia, as the syllabus was missing from the Cornell site
capital_punishment_links <- c("Palko v. Connecticut 302 u.s. 319 (1937)",
                              "Louisiana ex rel. Francis v. Resweber 329 u.s. 459 (1947)",
                              "United States v. Jackson 390 u.s. 570 (1968)",
                              "Furman v. Georgia 408 u.s. 238 (1972)",
                              "Gregg v. Georgia 428 u.s. 153 (1976)",
                              "Proffitt v. Florida 428 u.s. 242 (1976)",
                              "Woodson v. North Carolina 428 u.s. 280 (1976)",
                              "Coker v. Georgia 433 u.s. 584 (1977)",
                              "Lockett v. Ohio 438 u.s. 586 (1978)",
                              "Bell v. Ohio 438 u.s. 637 (1978)",
                              "Enmund v. Florida 458 u.s. 782 (1982)",
                              "Pulley v. Harris 465 u.s. 37 (1984)",
                              "Lockhart v. McCree 476 u.s. 162 (1986)",
                              "Ford v. Wainwright 477 u.s. 399 (1986)",
                              "McCleskey v. Kemp 481 u.s. 279 (1987)",
                              "Thompson v. Oklahoma 487 u.s. 815 (1988)",
                              "South Carolina v. Gathers 490 u.s. 805 (1989)",
                              "Penry v. Lynaugh 492 u.s. 302 (1989)",
                              "Stanford v. Kentucky 492 u.s. 361 (1989)",
                              "Dawson v. Delaware 503 u.s. 159 (1992)",
                              "Gomez v. United States District Court for the Northern District of California 503 u.s. 653 (1992)")

for (i in 1:length(capital_punishment_links)){
  decisions$topic[i+61] <- topics[11]
}

for (i in 1:length(capital_punishment_links)){
  decisions$case[i+61] <- html_session(topics_url[11]) %>%
    follow_link(capital_punishment_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(capital_punishment_links)){
  decisions$argued[i+61] <- html_session(topics_url[11]) %>%
    follow_link(capital_punishment_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(capital_punishment_links)){
  decisions$decided[i+61] <- html_session(topics_url[11]) %>%
    follow_link(capital_punishment_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(capital_punishment_links)){
  decisions$opinion[i+61] <- html_session(topics_url[11]) %>%
    follow_link(capital_punishment_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

witherspoonVillinois <- read_html("https://supreme.justia.com/cases/federal/us/391/510/case.html")
bumperVnc <- read_html("https://supreme.justia.com/cases/federal/us/391/543/case.html")
schickVreed <- read_html("https://supreme.justia.com/cases/federal/us/419/256/")
jurekVtexas <- read_html("https://supreme.justia.com/cases/federal/us/428/262/")
eddingsVoklahoma <- read_html("https://supreme.justia.com/cases/federal/us/455/104/")
boothVmaryland <- read_html("https://supreme.justia.com/cases/federal/us/482/496/")
atkinsVvirginia <- read_html("https://supreme.justia.com/cases/federal/us/536/304/case.html")

decisions$topic[83] <- topics[11]
decisions$topic[84] <- topics[11]
decisions$topic[85] <- topics[11]
decisions$topic[86] <- topics[11]
decisions$topic[87] <- topics[11]
decisions$topic[88] <- topics[11]
decisions$topic[89] <- topics[11]

decisions$case[83] <- witherspoonVillinois %>% html_node("h3+ p b") %>% html_text()
decisions$case[84] <- bumperVnc %>% html_node("h3+ p b") %>% html_text()
decisions$case[85] <- schickVreed %>% html_node("h3+ p b") %>% html_text()
decisions$case[86] <- jurekVtexas %>% html_node("h3+ p b") %>% html_text()
decisions$case[87] <- eddingsVoklahoma %>% html_node("h3+ p b") %>% html_text()
decisions$case[88] <- boothVmaryland %>% html_node("h3+ p b") %>% html_text()
decisions$case[89] <- atkinsVvirginia %>% html_node("#opinion > p:nth-child(2)") %>% html_text()

decisions$argued[83] <- witherspoonVillinois %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[84] <- bumperVnc %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[85] <- schickVreed %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[86] <- jurekVtexas %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[87] <- eddingsVoklahoma %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[88] <- boothVmaryland %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[89] <- atkinsVvirginia %>% html_node("p:nth-child(3)") %>% html_text()

decisions$decided[83] <- witherspoonVillinois %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[84] <- bumperVnc %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[85] <- schickVreed %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[86] <- jurekVtexas %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[87] <- eddingsVoklahoma %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[88] <- boothVmaryland %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[89] <- atkinsVvirginia %>% html_node("p:nth-child(3)") %>% html_text()

decisions$opinion[83] <- witherspoonVillinois %>% html_node("p:nth-child(22)") %>% html_text()
decisions$opinion[84] <- bumperVnc %>% html_node("p:nth-child(19)") %>% html_text()
decisions$opinion[85] <- schickVreed %>% html_node("p:nth-child(15)") %>% html_text()
decisions$opinion[86] <- jurekVtexas %>% html_node("p:nth-child(25)") %>% html_text()
decisions$opinion[87] <- eddingsVoklahoma %>% html_node("p:nth-child(17)") %>% html_text()
decisions$opinion[88] <- boothVmaryland %>% html_node("p:nth-child(17)") %>% html_text()
decisions$opinion[89] <- atkinsVvirginia %>% html_node("p:nth-child(4)") %>% html_text()

# TOPIC: CENSORSHIP #
# Note: A different website had to be used for Erznoznik vs. City of Jacksonville and Ashcroft vs. Free Speech Coalition, as the syllabus was missing from the Cornell site
censorship_links <- c("Near v. Minnesota 283 u.s. 697 (1931)")

for (i in 1:length(censorship_links)){
  decisions$topic[i+89] <- topics[12]
}

for (i in 1:length(censorship_links)){
  decisions$case[i+89] <- html_session(topics_url[12]) %>%
    follow_link(censorship_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(censorship_links)){
  decisions$argued[i+89] <- html_session(topics_url[12]) %>%
    follow_link(censorship_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(censorship_links)){
  decisions$decided[i+89] <- html_session(topics_url[12]) %>%
    follow_link(censorship_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(censorship_links)){
  decisions$opinion[i+89] <- html_session(topics_url[12]) %>%
    follow_link(censorship_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

erznoznikVjacksonville <- read_html("https://supreme.justia.com/cases/federal/us/422/205/")
ashcroftVfsc <- read_html("https://supreme.justia.com/cases/federal/us/535/234/case.html")

decisions$topic[91] <- topics[12]
decisions$topic[92] <- topics[12]

decisions$case[91] <- erznoznikVjacksonville %>% html_node("h3+ p b") %>% html_text()
decisions$case[92] <- ashcroftVfsc %>% html_node(".col--three-fourths .heading-1") %>% html_text()

decisions$argued[91] <- erznoznikVjacksonville %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[92] <- ashcroftVfsc %>% html_node("blockquote:nth-child(6) p") %>% html_text()

decisions$decided[91] <- erznoznikVjacksonville %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[92] <- ashcroftVfsc %>% html_node("blockquote:nth-child(6) p") %>% html_text()

decisions$opinion[91] <- erznoznikVjacksonville %>% html_node("p:nth-child(16)") %>% html_text()
decisions$opinion[92] <- ashcroftVfsc %>% html_node("p:nth-child(31)") %>% html_text()

# TOPIC: CHILDREN #
# Note: A different website had to be used for Ingraham vs. Wright, Hazelwood School Dist vs. Kuhlmeier, Massachusetts vs. Oakes, United States vs. Lopez, Ashcroft vs. Free Speech Coalition, and Nguyen vs. INS, as the syllabus was missing from the Cornell site
children_links <- c("Hammer v. Dagenhart 247 u.s. 251 (1918)",
                    "Bailey v. Drexel Furniture Company 259 u.s. 20 (1922)",
                    "Meyer v. State of Nebraska 262 u.s. 390 (1923)",
                    "Pierce v. Society of Sisters 268 u.s. 510 (1925)",
                    "Minersville School District v. Board of Education 310 u.s. 586 (1940)",
                    "West Virginia State Board of Education v. Barnette 319 u.s. 624 (1943)",
                    "Prince v. Massachusetts 321 u.s. 158 (1944)",
                    "Everson v. Board of Education of the Township of Ewing 330 u.s. 1 (1947)",
                    "In re Gault 387 u.s. 1 (1967)",
                    "Tinker v. Des Moines Independent Community School Dist. 393 u.s. 503 (1969)",
                    "Santosky v. Kramer 455 u.s. 745 (1982)",
                    "Palmore v. Sidoti 466 u.s. 429 (1984)",
                    "DeShaney v. Winnebago County Department of Social Services 489 u.s. 189 (1989)",
                    "Osborne v. Ohio 495 u.s. 103 (1990)",
                    "Maryland v. Craig 497 u.s. 836 (1990)",
                    "Troxel v. Granville 530 u.s. 57 (2000)")

for (i in 1:length(children_links)){
  decisions$topic[i+92] <- topics[13]
}

for (i in 1:length(children_links)){
  decisions$case[i+92] <- html_session(topics_url[13]) %>%
    follow_link(children_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(children_links)){
  decisions$argued[i+92] <- html_session(topics_url[13]) %>%
    follow_link(children_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(children_links)){
  decisions$decided[i+92] <- html_session(topics_url[13]) %>%
    follow_link(children_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(children_links)){
  decisions$opinion[i+92] <- html_session(topics_url[13]) %>%
    follow_link(children_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

ingrahamVwright <- read_html("https://supreme.justia.com/cases/federal/us/430/651/")
hsdVkuhlmeier <- read_html("https://supreme.justia.com/cases/federal/us/484/260/")
massachusettsVoakes <- read_html("https://supreme.justia.com/cases/federal/us/491/576/")
usVlopez <- read_html("https://supreme.justia.com/cases/federal/us/514/549/")
ashcroftVfsc <- read_html("https://supreme.justia.com/cases/federal/us/535/234/case.html")
nguyenVins <- read_html("https://supreme.justia.com/cases/federal/us/533/53/case.html")

decisions$topic[109] <- topics[13]
decisions$topic[110] <- topics[13]
decisions$topic[111] <- topics[13]
decisions$topic[112] <- topics[13]
decisions$topic[113] <- topics[13]
decisions$topic[114] <- topics[13]

decisions$case[109] <- ingrahamVwright %>% html_node("h3+ p b") %>% html_text()
decisions$case[110] <- hsdVkuhlmeier %>% html_node("h3+ p b") %>% html_text()
decisions$case[111] <- massachusettsVoakes %>% html_node("h3+ p b") %>% html_text()
decisions$case[112] <- usVlopez %>% html_node(".col--three-fourths .heading-1") %>% html_text()
decisions$case[113] <- ashcroftVfsc %>% html_node(".col--three-fourths .heading-1") %>% html_text()
decisions$case[114] <- nguyenVins %>% html_node(".col--three-fourths .heading-1") %>% html_text()

decisions$argued[109] <- ingrahamVwright %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[110] <- hsdVkuhlmeier %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[111] <- massachusettsVoakes %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[112] <- usVlopez %>% html_node("#opinion p:nth-child(5)") %>% html_text()
decisions$argued[113] <- ashcroftVfsc %>% html_node("blockquote:nth-child(6) p") %>% html_text()
decisions$argued[114] <- nguyenVins %>% html_node("blockquote:nth-child(5) p") %>% html_text()

decisions$decided[109] <- ingrahamVwright %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[110] <- hsdVkuhlmeier %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[111] <- massachusettsVoakes %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[112] <- usVlopez %>% html_node("#opinion p:nth-child(5)") %>% html_text()
decisions$decided[113] <- ashcroftVfsc %>% html_node("blockquote:nth-child(6) p") %>% html_text()
decisions$decided[114] <- nguyenVins %>% html_node("blockquote:nth-child(5) p") %>% html_text()

decisions$opinion[109] <- ingrahamVwright %>% html_node("p:nth-child(23)") %>% html_text()
decisions$opinion[110] <- hsdVkuhlmeier %>% html_node("p:nth-child(19)") %>% html_text()
decisions$opinion[111] <- massachusettsVoakes %>% html_node("p:nth-child(20)") %>% html_text()
decisions$opinion[112] <- usVlopez %>% html_node("p:nth-child(14)") %>% html_text()
decisions$opinion[113] <- ashcroftVfsc %>% html_node("p:nth-child(31)") %>% html_text()
decisions$opinion[114] <- nguyenVins %>% html_node("p:nth-child(28)") %>% html_text()

# TOPIC: CHOICE OF LAW #
choice_of_law_links <- c("Swift v. Tyson 41 u.s. 1 (1842)",
                         "Erie Railroad Co. v. Tompkins 304 u.s. 64 (1938)",
                         "Ex Parte Quirin 317 u.s. 1 (1942)",
                         "Smith v. Allwright 321 u.s. 649 (1944)",
                         "Evans v. Newton 382 u.s. 296 (1966)",
                         "Arizona v. Fulminante 499 u.s. 279 (1991)")

for (i in 1:length(choice_of_law_links)){
  decisions$topic[i+114] <- topics[14]
}

for (i in 1:length(choice_of_law_links)){
  decisions$case[i+114] <- html_session(topics_url[14]) %>%
    follow_link(choice_of_law_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(choice_of_law_links)){
  decisions$argued[i+114] <- html_session(topics_url[14]) %>%
    follow_link(choice_of_law_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(choice_of_law_links)){
  decisions$decided[i+114] <- html_session(topics_url[14]) %>%
    follow_link(choice_of_law_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(choice_of_law_links)){
  decisions$opinion[i+114] <- html_session(topics_url[14]) %>%
    follow_link(choice_of_law_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

# TOPIC: CITIZENSHIP #
# Note: A different website had to be used for Graham vs. Department of Public Welfare and Nguyen vs. INS, as the syllabus was missing from the Cornell site
citizenship_links <- c("Talbot v. Janson 3 u.s. 133 (1795)",
                       "Ableman v. Booth 62 u.s. 506 (1858)",
                       "Minor v. Happersett 88 u.s. 162 (1875)",
                       "United States v. Wong Kim Ark 169 u.s. 649 (1898)",
                       "Twining v. State 211 u.s. 78 (1908)",
                       "Hague v. Committee for Industrial Organization 307 u.s. 496 (1939)",
                       "Perez v. Brownell 356 u.s. 44 (1958)",
                       "Trop v. Dulles 356 u.s. 86 (1958)",
                       "Schneider v. Rusk 377 u.s. 163 (1964)",
                       "Afroyim v. Rusk 387 u.s. 253 (1967)",
                       "Hampton v. Mow Sun Wong 426 u.s. 88 (1976)",
                       "Ambach v. Norwick 441 u.s. 68 (1979)",
                       "Building Trades & Construction Trades Council of Camden County and Vicinity v. Mayor and Council of the City of Camden 465 u.s. 208 (1984)")

for (i in 1:length(citizenship_links)){
  decisions$topic[i+120] <- topics[15]
}

for (i in 1:length(citizenship_links)){
  decisions$case[i+120] <- html_session(topics_url[15]) %>%
    follow_link(citizenship_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(citizenship_links)){
  decisions$argued[i+120] <- html_session(topics_url[15]) %>%
    follow_link(citizenship_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(citizenship_links)){
  decisions$decided[i+120] <- html_session(topics_url[15]) %>%
    follow_link(citizenship_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(citizenship_links)){
  decisions$opinion[i+120] <- html_session(topics_url[15]) %>%
    follow_link(citizenship_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

grahamVdpw <- read_html("https://supreme.justia.com/cases/federal/us/403/365/")
nguyenVins <- read_html("https://supreme.justia.com/cases/federal/us/533/53/case.html")

decisions$topic[134] <- topics[15]
decisions$topic[135] <- topics[15]

decisions$case[134] <- grahamVdpw %>% html_node("h3+ p b") %>% html_text()
decisions$case[135] <- nguyenVins %>% html_node(".col--three-fourths .heading-1") %>% html_text()

decisions$argued[134] <- grahamVdpw %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[135] <- nguyenVins %>% html_node("blockquote:nth-child(5) p") %>% html_text()

decisions$decided[134] <- grahamVdpw %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[135] <- nguyenVins %>% html_node("blockquote:nth-child(5) p") %>% html_text()

decisions$opinion[134] <- grahamVdpw %>% html_node("p:nth-child(12)") %>% html_text()
decisions$opinion[135] <- nguyenVins %>% html_node("p:nth-child(28)") %>% html_text()

# TOPIC: COMMANDER IN CHIEF #
commander_in_chief_links <- c("Ex Parte Quirin 317 u.s. 1 (1942)",
                              "Korematsu v. United States 323 u.s. 214 (1944)",
                              "Woods v. Cloyd W. Miller Co. 333 u.s. 138 (1948)",
                              "United States v. Quarles 350 u.s. 11 (1955)",
                              "Reid v. Covert 354 u.s. 1 (1957)")

for (i in 1:length(commander_in_chief_links)){
  decisions$topic[i+135] <- topics[16]
}

for (i in 1:length(commander_in_chief_links)){
  decisions$case[i+135] <- html_session(topics_url[16]) %>%
    follow_link(commander_in_chief_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(commander_in_chief_links)){
  decisions$argued[i+135] <- html_session(topics_url[16]) %>%
    follow_link(commander_in_chief_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(commander_in_chief_links)){
  decisions$decided[i+135] <- html_session(topics_url[16]) %>%
    follow_link(commander_in_chief_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(commander_in_chief_links)){
  decisions$opinion[i+135] <- html_session(topics_url[16]) %>%
    follow_link(commander_in_chief_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

# TOPIC: COMMERCE CLAUSE #
# Note: A different website had to be used for Baldwin vs. GAF Seelig Inc., American Communications Assn. vs. Douds, National League of Cities vs. Usery, United States Trust Company of New York vs. New Jersey, Quill Corp. vs. North Dakota by and Through Heitkamp, United States vs. Lopez, and Reno vs. Condon, as the syllabus was missing from the Cornell site
commerce_clause_links <- c("Gibbons v. Ogden 22 u.s. 1 (1824)",
                           "Willson v. Black Bird Creek Marsh Company 27 u.s. 245 (1829)",
                           "Cooley v. Board of Wardens 53 u.s. 299 (1851)",
                           "Slaughterhouse Cases 83 u.s. 36 (1872)",
                           "United States v. E. C. Knight Company 156 u.s. 1 (1895)",
                           "In re Debs 158 u.s. 564 (1895)",
                           "Northern Securities Co. v. United States 193 u.s. 197 (1904)",
                           "Swift and Company v. United States 196 u.s. 375 (1905)",
                           "Adair v. United States 208 u.s. 161 (1908)",
                           "Loewe v. Lawlor 208 u.s. 274 (1908)",
                           "Standard Oil Co. of New Jersey v. United States 221 u.s. 1 (1911)",
                           "Houston East and West Texas Railway Company v. United States 234 u.s. 342 (1914)",
                           "Wilson v. New 243 u.s. 332 (1917)",
                           "Hammer v. Dagenhart 247 u.s. 251 (1918)",
                           "Adkins v. Children's Hospital 261 u.s. 525 (1923)",
                           "A. L. A. Schechter Poultry Corp. v. United States 295 u.s. 495 (1935)",
                           "Carter v. Carter Coal Co. 298 u.s. 238 (1936)",
                           "National Labor Relations Board v. Jones & Laughlin Steel Corp. 301 u.s. 1 (1937)",
                           "South Carolina State Highway Department v. Barnwell Brothers, Inc. 303 u.s. 177 (1938)",
                           "United States v. Carolene Products Co. 304 u.s. 144 (1938)",
                           "Mulford v. Smith 307 u.s. 38 (1939)",
                           "United States v. Darby 312 u.s. 100 (1941)",
                           "Wickard v. Filburn 317 u.s. 111 (1942)",
                           "Southern Pacific Co. v. Arizona 325 u.s. 761 (1945)",
                           "Dean Milk Co. v. City of Madison 340 u.s. 349 (1951)",
                           "Bibb v. Navajo Freight Lines, Inc. 359 u.s. 520 (1959)",
                           "Huron Portland Cement Co. v. City of Detroit 362 u.s. 440 (1960)",
                           "Heart of Atlanta Motel, Inc. v. United States 379 u.s. 241 (1964)",
                           "Katzenbach v. McClung 379 u.s. 294 (1964)",
                           "United States v. Brown 381 u.s. 437 (1965)",
                           "Hunt v. Washington State Apple Advertising Commission 432 u.s. 333 (1977)",
                           "City of Philadelphia v. New Jersey 437 u.s. 617 (1978)",
                           "Fullilove v. Klutznick 448 u.s. 448 (1980)",
                           "Minnesota v. Clover Leaf Creamery Co. 449 u.s. 456 (1981)",
                           "Kassel v. Consolidated Freightways Corporation of Delaware 450 u.s. 662 (1981)",
                           "Building Trades & Construction Trades Council of Camden County and Vicinity v. Mayor and Council of the City of Camden 465 u.s. 208 (1984)",
                           "Garcia v. San Antonio Transit Authority 469 u.s. 528 (1985)",
                           "C & A Carbone, Inc. v. Town of Clarkstown 511 u.s. 383 (1994)",
                           "United States v. Morrison 529 u.s. 598 (2000)")

for (i in 1:length(commerce_clause_links)){
  decisions$topic[i+140] <- topics[17]
}

for (i in 1:length(commerce_clause_links)){
  decisions$case[i+140] <- html_session(topics_url[17]) %>%
    follow_link(commerce_clause_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(commerce_clause_links)){
  decisions$argued[i+140] <- html_session(topics_url[17]) %>%
    follow_link(commerce_clause_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(commerce_clause_links)){
  decisions$decided[i+140] <- html_session(topics_url[17]) %>%
    follow_link(commerce_clause_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(commerce_clause_links)){
  decisions$opinion[i+140] <- html_session(topics_url[17]) %>%
    follow_link(commerce_clause_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

baldwinVgafsinc <- read_html("https://supreme.justia.com/cases/federal/us/294/511/case.html")
acaVdouds <- read_html("https://supreme.justia.com/cases/federal/us/339/382/case.html")
nlcVusery <- read_html("https://supreme.justia.com/cases/federal/us/426/833/")
ustcVnj <- read_html("https://supreme.justia.com/cases/federal/us/431/1/")
qcVnd <- read_html("https://supreme.justia.com/cases/federal/us/504/298/")
usVlopez <- read_html("https://supreme.justia.com/cases/federal/us/514/549/")
renoVcondon <- read_html("https://supreme.justia.com/cases/federal/us/528/141/")

decisions$topic[180] <- topics[17]
decisions$topic[181] <- topics[17]
decisions$topic[182] <- topics[17]
decisions$topic[183] <- topics[17]
decisions$topic[184] <- topics[17]
decisions$topic[185] <- topics[17]
decisions$topic[186] <- topics[17]

decisions$case[180] <- baldwinVgafsinc %>% html_node("h3+ p b") %>% html_text()
decisions$case[181] <- acaVdouds %>% html_node("h3+ p b") %>% html_text()
decisions$case[182] <- nlcVusery %>% html_node("h3+ p b") %>% html_text()
decisions$case[183] <- ustcVnj %>% html_node("h3+ p b") %>% html_text()
decisions$case[184] <- qcVnd %>% html_node("p:nth-child(3)") %>% html_text()
decisions$case[185] <- usVlopez %>% html_node(".col--three-fourths .heading-1") %>% html_text()
decisions$case[186] <- renoVcondon %>% html_node("p:nth-child(3)") %>% html_text()

decisions$argued[180] <- baldwinVgafsinc %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[181] <- acaVdouds %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[182] <- nlcVusery %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[183] <- ustcVnj %>% html_node("p:nth-child(5) b") %>% html_text()
decisions$argued[184] <- qcVnd %>% html_node("p:nth-child(4)") %>% html_text()
decisions$argued[185] <- usVlopez %>% html_node("#opinion p:nth-child(5)") %>% html_text()
decisions$argued[186] <- renoVcondon %>% html_node("#opinion p:nth-child(5)") %>% html_text()

decisions$decided[180] <- baldwinVgafsinc %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[181] <- acaVdouds %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[182] <- nlcVusery %>% html_node("p:nth-child(7) b") %>% html_text()
decisions$decided[183] <- ustcVnj %>% html_node("p:nth-child(6) b") %>% html_text()
decisions$decided[184] <- qcVnd %>% html_node("p:nth-child(4)") %>% html_text()
decisions$decided[185] <- usVlopez %>% html_node("#opinion p:nth-child(5)") %>% html_text()
decisions$decided[186] <- renoVcondon %>% html_node("#opinion p:nth-child(5)") %>% html_text()

decisions$opinion[180] <- baldwinVgafsinc %>% html_node("p:nth-child(20)") %>% html_text()
decisions$opinion[181] <- acaVdouds %>% html_node("p:nth-child(29)") %>% html_text()
decisions$opinion[182] <- nlcVusery %>% html_node("p:nth-child(18)") %>% html_text()
decisions$opinion[183] <- ustcVnj %>% html_node("p:nth-child(19)") %>% html_text()
decisions$opinion[184] <- qcVnd %>% html_node("p:nth-child(17)") %>% html_text()
decisions$opinion[185] <- usVlopez %>% html_node("p:nth-child(14)") %>% html_text()
decisions$opinion[186] <- renoVcondon %>% html_node("p:nth-child(14)") %>% html_text()

# TOPIC: COMMERCIAL SPEECH #
commercial_speech_links <- c("Grosjean v. American Press Co., Inc. 297 u.s. 233 (1936)",
                             "Roth v. United States 354 u.s. 476 (1957)",
                             "New York Times Co. v. Sullivan 376 u.s. 254 (1964)",
                             "Bates v. State Bar of Arizona 433 u.s. 350 (1977)",
                             "Central Hudson Gas & Elec. Corp. v. Public Service Comm'n 447 u.s. 557 (1980)",
                             "Bolger v. Youngs Drugs Prods. Corp. 463 u.s. 60 (1983)")

for (i in 1:length(commercial_speech_links)){
  decisions$topic[i+186] <- topics[18]
}

for (i in 1:length(commercial_speech_links)){
  decisions$case[i+186] <- html_session(topics_url[18]) %>%
    follow_link(commercial_speech_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(commercial_speech_links)){
  decisions$argued[i+186] <- html_session(topics_url[18]) %>%
    follow_link(commercial_speech_links[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(commercial_speech_links)){
  decisions$decided[i+186] <- html_session(topics_url[18]) %>%
    follow_link(commercial_speech_links[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(commercial_speech_links)){
  decisions$opinion[i+186] <- html_session(topics_url[18]) %>%
    follow_link(commercial_speech_links[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

# TOPIC: COMMUNISM #
communism_page1_links <- c("Kalman J. BERENYI, Petitioner, v. DISTRICT DIRECTOR, IMMIGRATION AND NATURALIZATION SERVICE.",
                           "W. E. B. DuBOIS CLUBS OF AMERICA et al. v. CLARK et al.",
                           "Charles ROWOLDT, Petitioner, v. J. D. PERFETTO, Acting Officer in Charge, Immigration and Naturalization Service, Department of Justice, St. Paul, Minnesota.",
                           "Willia NIUKKANEN, etc., Petitioner, v. E. D. McALEXANDER, etc.",
                           "Barenblatt v. United States",
                           "Stanislaw NOWAK, Petitioner, v. UNITED STATES of America.",
                           "Bond v. Floyd",
                           "DENNIS v. UNITED STATES.",
                           "SCHNEIDERMAN v. UNITED STATES.",
                           "TERMINIELLO v. CITY OF CHICAGO.")

for (i in 1:length(communism_page1_links)){
  decisions$topic[i+192] <- "communism"
}

for (i in 1:length(communism_page1_links)){
  decisions$case[i+192] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
    follow_link(communism_page1_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page1_links)){
  decisions$argued[i+192] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
    follow_link(communism_page1_links[i]) %>%
    html_node('.docketno+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page1_links)){
  decisions$decided[i+192] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
    follow_link(communism_page1_links[i]) %>%
    html_node('.date+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page1_links)){
  decisions$opinion[i+192] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
    follow_link(communism_page1_links[i]) %>%
    html_node('li:nth-child(1) > .writnav') %>% 
    html_text()
}

decisions$decided[194] <- decisions$argued[194]
decisions$argued[194] <- NA

decisions$argued[197] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
  follow_link("Barenblatt v. United States") %>%
  html_node('.toccaption:nth-child(5) b') %>% 
  html_text()

decisions$decided[197] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
  follow_link("Barenblatt v. United States") %>%
  html_node('.toccaption:nth-child(6) b') %>% 
  html_text()

decisions$opinion[197] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
  follow_link("Barenblatt v. United States") %>%
  html_node('#block-supremecourt-text li+ li') %>% 
  html_text()

decisions$argued[199] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
  follow_link("Bond v. Floyd") %>%
  html_node('.toccaption:nth-child(5) b') %>% 
  html_text()

decisions$decided[199] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
  follow_link("Bond v. Floyd") %>%
  html_node('.toccaption:nth-child(6) b') %>% 
  html_text()

decisions$opinion[199] <- html_session("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]") %>%
  follow_link("Bond v. Floyd") %>%
  html_node('#block-supremecourt-text li+ li') %>% 
  html_text()

communism_page2_links <- c("Hugo DeGREGORY, Appellant, v. ATTORNEY GENERAL OF the STATE OF NEW HAMPSHIRE.",
                           "Mabel BLACK and T. Y. Wulff et al., Petitioners, v. CUTTER LABORATORIES, a Corporation.",
                           "John Francis NOTO, Petitioner, v. UNITED STATES.",
                           "Willard UPHAUS, Appellant, v. Louis C. WYMAN, Attorney General, State of New Hampshire.",
                           "Bernhard DEUTCH, Petitioner, v. UNITED STATES.",
                           "HERNDON v. LOWRY, Sheriff.",
                           "Carl BRADEN, Petitioner, v. UNITED STATES.")

for (i in 1:length(communism_page2_links)){
  decisions$topic[i+202] <- "communism"
}

for (i in 1:length(communism_page2_links)){
  decisions$case[i+202] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page2_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page2_links)){
  decisions$argued[i+202] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page2_links[i]) %>%
    html_node('.docketno+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page2_links)){
  decisions$decided[i+202] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page2_links[i]) %>%
    html_node('.date+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page2_links)){
  decisions$opinion[i+202] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page2_links[i]) %>%
    html_node('li:nth-child(1) > .writnav') %>% 
    html_text()
}

decisions$topic[210:212] <- "communism"

decisions$case[210] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Dennis v. United States") %>%
  html_node('#page-title') %>% 
  html_text()

decisions$argued[210] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Dennis v. United States") %>%
  html_node('.toccaption:nth-child(5) b') %>% 
  html_text()

decisions$decided[210] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Dennis v. United States") %>%
  html_node('.toccaption:nth-child(6) b') %>% 
  html_text()

decisions$opinion[210] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Dennis v. United States") %>%
  html_node('#block-supremecourt-text li+ li') %>% 
  html_text()

decisions$case[211] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Watkins v. United States") %>%
  html_node('#page-title') %>% 
  html_text()

decisions$argued[211] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Watkins v. United States") %>%
  html_node('.toccaption:nth-child(5) b') %>% 
  html_text()

decisions$decided[211] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Watkins v. United States") %>%
  html_node('.toccaption:nth-child(6) b') %>% 
  html_text()

decisions$opinion[211] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Watkins v. United States") %>%
  html_node('#block-supremecourt-text li+ li') %>% 
  html_text()

decisions$case[212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Whitney v. California") %>%
  html_node('#page-title') %>% 
  html_text()

decisions$argued[212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Whitney v. California") %>%
  html_node('.toccaption:nth-child(5) b') %>% 
  html_text()

decisions$decided[212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Whitney v. California") %>%
  html_node('.toccaption:nth-child(6) b') %>% 
  html_text()

decisions$opinion[212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Whitney v. California") %>%
  html_node('#block-supremecourt-text li+ li') %>% 
  html_text()

communism_page3_links <- c("Theodore R. GIBSON, Petitioner, v. FLORIDA LEGISLATIVE INVESTIGATION COMMITTEE.",
                           "Jose Maria GASTELUM-QUINONES, Petitioner, v. Robert F. KENNEDY, Attorney General of the United States (two cases).",
                           "Rudolph SCHWARE, Petitioner, v. BOARD OF BAR EXAMINERS OF THE STATE OF NEW MEXICO.",
                           "Herbert SCHNEIDER, Appellant, v. Willard SMITH, Commandant, United States Coast Guard.",
                           "AMERICAN COMMITTEE FOR PROTECTION OF FOREIGN BORN, Petitioner, v. SUBVERSIVE ACTIVITIES CONTROL BOARD.",
                           "Sara BAIRD, Petitioner, v. STATE BAR OF ARIZONA.",
                           "Otho G. BELL et al., Petitioners, v. UNITED STATES.",
                           "Brian V. HUNTER and Jeffrey Jordan v. James V. BRYANT, Jr.",
                           "Frank BONETTI, Petitioner, v. William P. ROGERS, Attorney General of the United States, et al.")

for (i in 1:length(communism_page3_links)){
  decisions$topic[i+212] <- "communism"
}

for (i in 1:length(communism_page3_links)){
  decisions$case[i+212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=2&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page3_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page3_links)){
  decisions$argued[i+212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=2&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page3_links[i]) %>%
    html_node('.docketno+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page3_links)){
  decisions$decided[i+212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=2&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page3_links[i]) %>%
    html_node('.date+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page3_links)){
  decisions$opinion[i+212] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=2&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page3_links[i]) %>%
    html_node('li:nth-child(1) > .writnav') %>% 
    html_text()
}

decisions$decided[220] <- decisions$argued[220]
decisions$argued[220] <- NA

communism_page4_links <- c("GALVAN v. PRESS.",
                           "HARTZEL v. UNITED STATES.",
                           "Paul M. SWEEZY, Appellant, v. STATE OF NEW HAMPSHIRE by Louis C. WYMAN, Attorney General.",
                           "Julius EMSPAK, Petitioner, v. UNITED STATES of America.",
                           "Frank DYSON, Chief of Police, City of Dallas, et al., Appellants, v. Brent STEIN.",
                           "RAILWAY EMPLOYES' DEPARTMENT, American Federation of Labor, International Association of Machinists, et al., Appellants, v. Robert L. HANSON, Horace A. Cameron, Harold J. Grau, et al.",
                           "COMMUNIST PARTY OF THE UNITED STATES of America, Petitioner, v. SUBVERSIVE ACTIVITIES CONTROL BOARD.")

for (i in 1:length(communism_page4_links)){
  decisions$topic[i+221] <- "communism"
}

for (i in 1:length(communism_page4_links)){
  decisions$case[i+221] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page4_links[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page4_links)){
  decisions$argued[i+221] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page4_links[i]) %>%
    html_node('.docketno+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page4_links)){
  decisions$decided[i+221] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page4_links[i]) %>%
    html_node('.date+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page4_links)){
  decisions$opinion[i+221] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page4_links[i]) %>%
    html_node('li:nth-child(1) > .writnav') %>% 
    html_text()
}

decisions$topic[229:230] <- "communism"

decisions$case[229] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Yates v. United States") %>%
  html_node('#page-title') %>% 
  html_text()

decisions$argued[229] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Yates v. United States") %>%
  html_node('.toccaption:nth-child(5) b') %>% 
  html_text()

decisions$decided[229] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Yates v. United States") %>%
  html_node('.toccaption:nth-child(6) b') %>% 
  html_text()

decisions$opinion[229] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Yates v. United States") %>%
  html_node('#block-supremecourt-text li+ li') %>% 
  html_text()

decisions$case[230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Slochower v. Board of Higher Education of New York City") %>%
  html_node('#page-title') %>% 
  html_text()

decisions$argued[230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Slochower v. Board of Higher Education of New York City") %>%
  html_node('.toccaption:nth-child(5) b') %>% 
  html_text()

decisions$decided[230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Slochower v. Board of Higher Education of New York City") %>%
  html_node('.toccaption:nth-child(6) b') %>% 
  html_text()

decisions$opinion[230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
  follow_link("Slochower v. Board of Higher Education of New York City") %>%
  html_node('#block-supremecourt-text li+ li') %>% 
  html_text()

communism_page5_links_1 <- c("CAFETERIA AND RESTAURANT WORKERS UNION, LOCAL 473, AFL-CIO, et al., Petitioners, v. Neil H. McELROY et al.",
                             "SHAUGHNESSY, District Director of Immigration and Naturalization, v. UNITED STATES ex rel. MEZEI.",
                             "Raphael KONIGSBERG, Petitioner, v. The STATE BAR OF CALIFORNIA and the Committee of Bar Examiners of the State Barof California.")

communism_page5_links_2 <- c("Clark v. Community for Creative Nonviolence",
                             "Board of Education v. Allen",
                             "Kent v. Dulles",
                             "Scales v. United States",
                             "Pennsylvania v. Nelson",
                             "Gitlow v. People",
                             "Communist Party of the United States v. Subversive Activities Control Bd. No. 12")

for (i in 1:length(communism_page5_links_1)){
  decisions$topic[i+230] <- "communism"
}

for (i in 1:length(communism_page5_links_1)){
  decisions$case[i+230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_1[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page5_links_1)){
  decisions$argued[i+230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_1[i]) %>%
    html_node('.docketno+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page5_links_1)){
  decisions$decided[i+230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_1[i]) %>%
    html_node('.date+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page5_links_1)){
  decisions$opinion[i+230] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_1[i]) %>%
    html_node('li:nth-child(1) > .writnav') %>% 
    html_text()
}

for (i in 1:length(communism_page5_links_2)){
  decisions$topic[i+233] <- "communism"
}

for (i in 1:length(communism_page5_links_2)){
  decisions$case[i+233] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_2[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page5_links_2)){
  decisions$argued[i+233] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_2[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(communism_page5_links_2)){
  decisions$decided[i+233] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_2[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(communism_page5_links_2)){
  decisions$opinion[i+233] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page5_links_2[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

communism_page6_links_1 <- c("Richard L. THORNBURGH, Attorney General of the United States, et al., Petitioners v. Jack ABBOTT, et al.",
                             "Oswald ZSCHERNIG et al., Appellants, v. William J. MILLER, Administrator et al.",
                             "FLEMMING, Secretary of Health, Education, and Welfare, Appellant, v. Ephram NESTOR.",
                             "Thomas QUINN, Petitioner, v. UNITED STATES of America.",
                             "KEDROFF et al. v. ST. NICHOLAS CATHEDRAL OF RUSSIAN ORTHODOX CHURCH IN NORTH AMERICA.")

communism_page6_links_2 <- c("Aptheker v. Secretary of State",
                             "Perez v. Brownell",
                             "KERRY v. DIN",
                             "Rust v. Sullivan")

for (i in 1:length(communism_page6_links_1)){
  decisions$topic[i+240] <- "communism"
}

for (i in 1:length(communism_page6_links_1)){
  decisions$case[i+240] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_1[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page6_links_1)){
  decisions$argued[i+240] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_1[i]) %>%
    html_node('.docketno+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page6_links_1)){
  decisions$decided[i+240] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_1[i]) %>%
    html_node('.date+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page6_links_1)){
  decisions$opinion[i+240] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_1[i]) %>%
    html_node('li:nth-child(1) > .writnav') %>% 
    html_text()
}

for (i in 1:length(communism_page6_links_2)){
  decisions$topic[i+245] <- "communism"
}

for (i in 1:length(communism_page6_links_2)){
  decisions$case[i+245] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_2[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page6_links_2)){
  decisions$argued[i+245] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_2[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(communism_page6_links_2)){
  decisions$decided[i+245] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_2[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(communism_page6_links_2)){
  decisions$opinion[i+245] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page6_links_2[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

decisions$opinion[248] <- "Scalia"

communism_page7_links_1 <- c("CARLSON et al. v. LANDON, District Director of Immigration & Naturalization, United States Department of Justice. BUTTERFIELD, Director of Immigration & Naturalization Service, Detroit, Mich. v. ZYDOK.",
                             "BRIDGES v. WIXON, District Director, Immigration and Naturalization Service.",
                             "Raphael KONIGSBERG, Petitioner, v. STATE BAR OF CALIFORNIA and the Committee of Bar Examiners of the State of California.")

communism_page7_links_2 <- c("United States v. United States District Court",
                             "Skinner v. Railway Labor Executives' Association",
                             "School District of Abington Township, Pennsylvania v. Schempp")

for (i in 1:length(communism_page7_links_1)){
  decisions$topic[i+249] <- "communism"
}

for (i in 1:length(communism_page7_links_1)){
  decisions$case[i+249] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_1[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page7_links_1)){
  decisions$argued[i+249] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_1[i]) %>%
    html_node('.docketno+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page7_links_1)){
  decisions$decided[i+249] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_1[i]) %>%
    html_node('.date+ .date') %>% 
    html_text()
}

for (i in 1:length(communism_page7_links_1)){
  decisions$opinion[i+249] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_1[i]) %>%
    html_node('li:nth-child(1) > .writnav') %>% 
    html_text()
}

for (i in 1:length(communism_page7_links_2)){
  decisions$topic[i+252] <- "communism"
}

for (i in 1:length(communism_page7_links_2)){
  decisions$case[i+252] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_2[i]) %>%
    html_node('#page-title') %>% 
    html_text()
}

for (i in 1:length(communism_page7_links_2)){
  decisions$argued[i+252] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_2[i]) %>%
    html_node('.toccaption:nth-child(5) b') %>% 
    html_text()
}

for (i in 1:length(communism_page7_links_2)){
  decisions$decided[i+252] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_2[i]) %>%
    html_node('.toccaption:nth-child(6) b') %>% 
    html_text()
}

for (i in 1:length(communism_page7_links_2)){
  decisions$opinion[i+252] <- html_session("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D") %>%
    follow_link(communism_page7_links_2[i]) %>%
    html_node('#block-supremecourt-text li+ li') %>% 
    html_text()
}

# TOPIC: CONFESSIONS #
confessions_links <- c("Chambers v. Florida 309 u.s. 227 (1940)")