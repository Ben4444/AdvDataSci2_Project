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

indices <- as.numeric(row.names(decisions[str_detect(decisions$case, pattern = "[0-9]{3} U.S. [0-9]{3}") & !is.na(decisions$topic) & (is.na(decisions$argued) | is.na(decisions$decided) | is.na(decisions$opinion)),]))

links <- str_replace(decisions[indices, "case"], pattern = "([0-9]+) U.S. ([0-9]+)", replacement = "https://supreme.justia.com/cases/federal/us/\\1/\\2/")

for(i in seq_along(links)){
  link_html <- readLines(links[i])
  decisions[indices[i], "case"] <- str_replace(link_html[str_detect(link_html, "<title>")], ".*<title> (.*?),.*", replacement = "\\1")
  decisions[indices[i], "argued"] <- ifelse(sum(str_detect(link_html, "<p><b>Argued")) == 0,
                                            NA,
                                            str_replace(link_html[str_detect(link_html, "<p><b>Argued")], pattern = ".*<p><b>Argued (.*?)<.*", replacement = "\\1"))
  decisions[indices[i], "decided"] <- ifelse(sum(str_detect(link_html, "<p><b>Decided")) == 0,
                                             NA,
                                             str_replace(link_html[str_detect(link_html, "<p><b>Decided")], pattern = ".*<p><b>Decided (.*?)<.*", replacement = "\\1"))
  decisions[indices[i], "opinion"] <- str_replace(link_html[str_detect(link_html, "delivered the opinion")][1], pattern = ".*Justice (.*) delivered the opinion.*", replacement = "\\1")
  if(!is.na(decisions[indices[i], "opinion"]) & nchar(decisions[indices[i], "opinion"]) > 30){
    decisions[indices[i], "opinion"] <- str_replace(link_html[str_detect(link_html, "delivered the opinion")][1], pattern = ".*JUSTICE (.*) delivered the opinion.*", replacement = "\\1")
  }
  if(!is.na(decisions[indices[i], "opinion"]) & nchar(decisions[indices[i], "opinion"]) > 30){
    decisions[indices[i], "opinion"] <- str_replace(link_html[str_detect(link_html, "delivered the opinion")][1], pattern = "<p>(.*) delivered the opinion.*", replacement = "\\1")
  }
  if(is.na(decisions[indices[i], "opinion"])){
    link_html <- readLines(paste0(links[i], "case.html"))
    decisions[indices[i], "opinion"] <- str_replace(link_html[str_detect(link_html, "delivered the opinion")][1], pattern = ".*Justice (.*) delivered the opinion.*", replacement = "\\1")
    if(!is.na(decisions[indices[i], "opinion"]) & nchar(decisions[indices[i], "opinion"]) > 30){
      decisions[indices[i], "opinion"] <- str_replace(link_html[str_detect(link_html, "delivered the opinion")][1], pattern = ".*JUSTICE (.*) delivered the opinion.*", replacement = "\\1")
    }
    if(!is.na(decisions[indices[i], "opinion"]) & nchar(decisions[indices[i], "opinion"]) > 30){
      decisions[indices[i], "opinion"] <- str_replace(link_html[str_detect(link_html, "delivered the opinion")][1], pattern = "<p>(.*) delivered the opinion.*", replacement = "\\1")
    }
  }
  if(!is.na(decisions[indices[i], "opinion"]) & nchar(decisions[indices[i], "opinion"]) > 30){
    decisions[indices[i], "opinion"] <- NA
  }
}

str_replace(decisions[,"argued",drop = F])

# TOPIC: AFFIRMATIVE ACTION #
# Note: A different website had to be used for Wygant vs. Jackson Board of Education, United States vs. Paradise, and City of Richmond vs. J.A. Croson Co., as the syllabus was missing from the Cornell site

wygantVjbe <- read_html("https://supreme.justia.com/cases/federal/us/476/267/")

decisions$opinion[25] <- wygantVjbe %>% html_node("p:nth-child(22)") %>% html_text()