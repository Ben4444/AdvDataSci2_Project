library(rvest)
library(dplyr)
library(readr)
library(stringr)

# Loading in Supreme Court nominee data, obtained from http://epstein.wustl.edu/research/justicesdata.html
justicesdata <- read.csv("http://epstein.wustl.edu/research/justicesdata.csv")

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

cornell_url <- "https://www.law.cornell.edu/supct/cases/topic.htm"
cornell_html <- readLines(cornell_url)
cornell_links <- na.omit(str_extract(cornell_html, "/supct/cases/topics/(.*?).html"))
topics_url <- paste0("https://www.law.cornell.edu", cornell_links)
topics <- str_replace(topics_url, pattern = ".*/tog_(.*?).html", replacement = "\\1")

cases <- read.csv("Supreme Court Decisions.csv", sep = "\t")

decisions <- data.frame(matrix(ncol = 5, nrow = 2530))
colnames(decisions) <- c("topic", "case", "argued", "decided", "opinion")
x = 1

# Does not grab info for communism
for(i in seq_along(topics_url)){
  topic_html <- readLines(topics_url[i])
  links <- na.omit(str_extract(topic_html, "/supct/html/historics/USSC_.*.html"))
  links <- paste0("https://www.law.cornell.edu",links)
  for(j in seq_along(links)){
    session <- html_session(links[j])
    decisions$topic[x] <- topics[i]
    decisions$case[x] <- html_node(session, '#page-title') %>% html_text()
    if(is.na(decisions$case[x])){
      decisions$case[x] <- html_node(session, '.sylcta') %>% html_text()
    }
    decisions$argued[x] <- html_node(session, '.toccaption:nth-child(5) b') %>% html_text()
    decisions$decided[x] <- html_node(session, '.toccaption:nth-child(6) b') %>% html_text()
    decisions$opinion[x] <- html_node(session, '#block-supremecourt-text li+ li') %>% html_text()
    x = x + 1
  }
}

decisions$opinion<-str_extract(decisions$opinion, pattern = "Opinion, .*[a-z]")

#decisions[which(!is.na(decisions$topic) & (is.na(decisions$argued) | is.na(decisions$decided) | is.na(decisions$opinion))),]

indices <- as.numeric(row.names(decisions[str_detect(decisions$case, pattern = "[0-9]{3} U.S. [0-9]{3}") & !is.na(decisions$topic) & (is.na(decisions$argued) | is.na(decisions$decided) | is.na(decisions$opinion)),]))

links <- str_replace(decisions[indices, "case"], pattern = "([0-9]+) U.S. ([0-9]+)", replacement = "https://supreme.justia.com/cases/federal/us/\\1/\\2/")

#for(i in seq_along(links)){
  link_html <- readLines(links[i])
  decisions[indices[i], "case"] <- str_replace(link_html[str_detect(link_html, "<title>")], ".*<title> (.*?),.*", replacement = "\\1")
  decisions[indices[i], "argued"] <- paste("Argued:", str_replace(link_html[str_detect(link_html, "<p><b>Argued")], pattern = ".*<p><b>Argued (.*?)<.*", replacement = "\\1"))
  decisions[indices[i], "decided"] <- paste("Decided:", str_replace(link_html[str_detect(link_html, "<p><b>Decided")], pattern = ".*<p><b>Decided (.*?)<.*", replacement = "\\1"))
}

# TOPIC: ABORTION #
# Note: A different website had to be used for Roe vs. Wade and Hodgson vs. Minnesota, as the syllabus was missing from the Cornell site


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
# Note: A different website had to be used for Graham vs. Dept. of Public Welfare, as the syllabus was missing from the Cornell site

grahamVdpw <- read_html("https://supreme.justia.com/cases/federal/us/403/365/")


decisions$case[36] <- grahamVdpw %>% html_node("h3+ p b") %>% html_text()

decisions$argued[36] <- grahamVdpw %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[36] <- grahamVdpw %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[36] <- grahamVdpw %>% html_node("p:nth-child(12)") %>% html_text()

# TOPIC: ARMED SERVICES #
# Note: A different website had to be used for Johnson vs. Robison, Schlesinger vs. Reservists Committee to Stop the War, and Schick vs. Reed, as the syllabus was missing from the Cornell site

johnsonVrobison <- read_html("https://supreme.justia.com/cases/federal/us/415/361/")
schlesingerVrcsw <- read_html("https://supreme.justia.com/cases/federal/us/418/208/")
schickVreed <- read_html("https://supreme.justia.com/cases/federal/us/419/256/")

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

usVlovett <- read_html("https://supreme.justia.com/cases/federal/us/328/303/case.html")
acaVdouds <- read_html("https://supreme.justia.com/cases/federal/us/339/382/case.html")
nixonVags <- read_html("https://supreme.justia.com/cases/federal/us/433/425/")

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

butzVeconomou <- read_html("https://supreme.justia.com/cases/federal/us/438/478/")

decisions$case[54] <- butzVeconomou %>% html_node("h3+ p b") %>% html_text()

decisions$argued[54] <- butzVeconomou %>% html_node("p:nth-child(5) b") %>% html_text()

decisions$decided[54] <- butzVeconomou %>% html_node("p:nth-child(6) b") %>% html_text()

decisions$opinion[54] <- butzVeconomou %>% html_node("p:nth-child(25)") %>% html_text()