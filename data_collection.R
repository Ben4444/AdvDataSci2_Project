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

decisions <- data.frame(matrix(ncol = 5, nrow = 2528))
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

#decisions$opinion<-str_extract(tail(decisions$opinion,100), pattern = "[Oo]pinion, .*[a-z]")

indices <- as.numeric(row.names(decisions[str_detect(decisions$case, pattern = "[0-9]+ U.S. [0-9]+") & !is.na(decisions$topic) & (is.na(decisions$argued) | is.na(decisions$decided) | is.na(decisions$opinion)),]))

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


# TOPIC: COMMUNISM #

scrape_communism_data_1 <- function(main_page, links, n){
  for (i in seq_along(links)){
    decisions$topic[i+n] <- "communism"
    session <- html_session(main_page) %>% follow_link(links[i])
    decisions$case[i+n] <- html_node(session, '#page-title') %>% html_text()
    decisions$argued[i+n] <- html_node(session, '.docketno+ .date') %>% html_text()
    decisions$decided[i+n] <- html_node(session, '.date+ .date') %>% html_text()
    decisions$opinion[i+n] <- html_node(session, 'li:nth-child(1) > .writnav') %>% html_text()
  }
  decisions
}

scrape_communism_data_2 <- function(main_page, links, n){
  for (i in seq_along(links)){
    decisions$topic[i+n] <- "communism"
    session <- html_session(main_page) %>% follow_link(links[i])
    decisions$case[i+n] <- html_node(session, '#page-title') %>% html_text()
    decisions$argued[i+n] <- html_node(session, '.toccaption:nth-child(5) b') %>% html_text()
    decisions$decided[i+n] <- html_node(session, '.toccaption:nth-child(6) b') %>% html_text()
    decisions$opinion[i+n] <- html_node(session, '#block-supremecourt-text li+ li') %>% html_text()
  }
  decisions
}

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

decisions <- scrape_communism_data_1("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]", communism_page1_links, n=2465)

decisions$decided[2467] <- decisions$argued[2467]
decisions$argued[2467] <- NA

decisions <- scrape_communism_data_2("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]", links = "Barenblatt v. United States", n=2469)

decisions <- scrape_communism_data_2("https://www.law.cornell.edu/search/site/[communism]?f[0]=bundle%3Asupct_node&&query=[communism]", links = "Bond v. Floyd", n=2471)

communism_page2_links <- c("Hugo DeGREGORY, Appellant, v. ATTORNEY GENERAL OF the STATE OF NEW HAMPSHIRE.",
                           "Mabel BLACK and T. Y. Wulff et al., Petitioners, v. CUTTER LABORATORIES, a Corporation.",
                           "John Francis NOTO, Petitioner, v. UNITED STATES.",
                           "Willard UPHAUS, Appellant, v. Louis C. WYMAN, Attorney General, State of New Hampshire.",
                           "Bernhard DEUTCH, Petitioner, v. UNITED STATES.",
                           "HERNDON v. LOWRY, Sheriff.",
                           "Carl BRADEN, Petitioner, v. UNITED STATES.")

decisions <- scrape_communism_data_1("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page2_links, n=2475)

communism_extra_links <- c("Dennis v. United States",
           "Watkins v. United States",
           "Whitney v. California")

decisions <- scrape_communism_data_2("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=1&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_extra_links, n=2482)

communism_page3_links <- c("Theodore R. GIBSON, Petitioner, v. FLORIDA LEGISLATIVE INVESTIGATION COMMITTEE.",
                           "Jose Maria GASTELUM-QUINONES, Petitioner, v. Robert F. KENNEDY, Attorney General of the United States (two cases).",
                           "Rudolph SCHWARE, Petitioner, v. BOARD OF BAR EXAMINERS OF THE STATE OF NEW MEXICO.",
                           "Herbert SCHNEIDER, Appellant, v. Willard SMITH, Commandant, United States Coast Guard.",
                           "AMERICAN COMMITTEE FOR PROTECTION OF FOREIGN BORN, Petitioner, v. SUBVERSIVE ACTIVITIES CONTROL BOARD.",
                           "Sara BAIRD, Petitioner, v. STATE BAR OF ARIZONA.",
                           "Otho G. BELL et al., Petitioners, v. UNITED STATES.",
                           "Brian V. HUNTER and Jeffrey Jordan v. James V. BRYANT, Jr.",
                           "Frank BONETTI, Petitioner, v. William P. ROGERS, Attorney General of the United States, et al.")

decisions <- scrape_communism_data_1("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=2&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page3_links, n=2485)

decisions$decided[2493] <- decisions$argued[2493]
decisions$argued[2493] <- NA

communism_page4_links <- c("GALVAN v. PRESS.",
                           "HARTZEL v. UNITED STATES.",
                           "Paul M. SWEEZY, Appellant, v. STATE OF NEW HAMPSHIRE by Louis C. WYMAN, Attorney General.",
                           "Julius EMSPAK, Petitioner, v. UNITED STATES of America.",
                           "Frank DYSON, Chief of Police, City of Dallas, et al., Appellants, v. Brent STEIN.",
                           "RAILWAY EMPLOYES' DEPARTMENT, American Federation of Labor, International Association of Machinists, et al., Appellants, v. Robert L. HANSON, Horace A. Cameron, Harold J. Grau, et al.",
                           "COMMUNIST PARTY OF THE UNITED STATES of America, Petitioner, v. SUBVERSIVE ACTIVITIES CONTROL BOARD.")

decisions <- scrape_communism_data_1("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page4_links, n=2494)

decisions <- scrape_communism_data_2("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=3&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", links = c("Yates v. United States", "Slochower v. Board of Higher Education of New York City"), n=2501)

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

decisions <- scrape_communism_data_1("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page5_links_1, n=2503)

decisions <- scrape_communism_data_2("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=4&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page5_links_2, n=2506)

communism_page6_links_1 <- c("Richard L. THORNBURGH, Attorney General of the United States, et al., Petitioners v. Jack ABBOTT, et al.",
                             "Oswald ZSCHERNIG et al., Appellants, v. William J. MILLER, Administrator et al.",
                             "FLEMMING, Secretary of Health, Education, and Welfare, Appellant, v. Ephram NESTOR.",
                             "Thomas QUINN, Petitioner, v. UNITED STATES of America.",
                             "KEDROFF et al. v. ST. NICHOLAS CATHEDRAL OF RUSSIAN ORTHODOX CHURCH IN NORTH AMERICA.")

communism_page6_links_2 <- c("Aptheker v. Secretary of State",
                             "Perez v. Brownell",
                             "KERRY v. DIN",
                             "Rust v. Sullivan")

decisions <- scrape_communism_data_1("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page6_links_1, n=2513)

decisions <- scrape_communism_data_2("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=5&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page6_links_2, n=2518)

decisions$opinion[2521] <- "Scalia"

communism_page7_links_1 <- c("CARLSON et al. v. LANDON, District Director of Immigration & Naturalization, United States Department of Justice. BUTTERFIELD, Director of Immigration & Naturalization Service, Detroit, Mich. v. ZYDOK.",
                             "BRIDGES v. WIXON, District Director, Immigration and Naturalization Service.",
                             "Raphael KONIGSBERG, Petitioner, v. STATE BAR OF CALIFORNIA and the Committee of Bar Examiners of the State of California.")

communism_page7_links_2 <- c("United States v. United States District Court",
                             "Skinner v. Railway Labor Executives' Association",
                             "School District of Abington Township, Pennsylvania v. Schempp")

decisions <- scrape_communism_data_1("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page7_links_1, n=2522)

decisions <- scrape_communism_data_2("https://www.law.cornell.edu/search/site/%5Bcommunism%5D?page=6&f%5B0%5D=bundle%3Asupct_node&query=%5Bcommunism%5D", communism_page7_links_2, n=2525)


######################
# Data Cleaning
######################

decisions$argued <- str_replace(decisions$argued, pattern = "Argued: ", replacement = "")
decisions$argued <- str_replace(decisions$argued, pattern = "Argued ", replacement = "")
decisions$argued <- str_replace(decisions$argued, pattern = "[Rr]eargued.*", replacement = "")
decisions[!is.na(decisions$argued) & decisions$argued == "", "argued"] <- NA

decisions$decided <- str_replace(decisions$decided, pattern = "[ ]+\\[\\*\\]", replacement = "")
decisions$decided <- str_replace(decisions$decided, pattern = "\\*$", replacement = "")
decisions$decided <- str_replace(decisions$decided, pattern = "Decided: ", replacement = "")
decisions$decided <- str_replace(decisions$decided, pattern = "Opinion and judgments announced ", replacement = "")
decisions[!is.na(decisions$decided) & decisions$decided == "", "decided"] <- NA

decisions$opinion <- str_replace(decisions$opinion, pattern = "[Oo]pinion, ", replacement = "")
decisions$opinion <- str_replace(decisions$opinion, pattern = ",.*", replacement = "")
decisions[!is.na(decisions$opinion) & decisions$opinion == "", "opinion"] <- NA
decisions$opinion <- str_to_title(decisions$opinion)

decisions[41, "argued"] <- "March 24, 1981"
decisions[41, "decided"] <- "June 25, 1981"

decisions$topic <- str_replace(decisions$topic, pattern = "_", replacement = " ")

decisions$case[50] <- "In re Primus"
decisions$case[117] <- "Ex Parte Quirin"
decisions$case[136] <- "Ex Parte Quirin"
decisions$case[203] <- "The Pocket Veto Case"
decisions$case[247] <- "In re Neagle"
decisions$case[256] <- "Ex Parte Grossman"
decisions$case[272] <- "Ex Parte Quirin"
decisions$case[426] <- "Chicago v. Morales"
decisions$case[434] <- "Florida v. J.L."
decisions$case[477] <- "Brown v. Board of Education (No. 1)"
decisions$case[478] <- "Brown v. Board of Education (No. 2)"
decisions$case[505] <- "Bragdon v. Abbott"

decisions$argued[decisions$case == "Ex Parte Garland"] <- "December 15, 1865"
decisions$argued[decisions$case == "Barron v. Mayor & City Council of Baltimore"] <- "February 11, 1833"
decisions$argued[decisions$case == "Gomez v. United States District Court for the Northern District of California"] <- "April 24, 1989"
decisions$argued[decisions$case == "Atkins v. Virginia"] <- "February 20, 2002"
decisions$argued[decisions$case == "Ashcroft v. Free Speech Coalition"] <- "October 30, 2001"
decisions$argued[decisions$case == "United States v. Lopez"] <- "November 8, 1994"
decisions$argued[decisions$case == "Nguyen v. INS"] <- "January 9, 2001"
decisions$argued[decisions$case == "Ableman v. Booth"] <- "January 19, 1859"
decisions$argued[decisions$case == "Gibbons v. Ogden"] <- "February 5, 1824"
decisions$argued[decisions$case == "Willson v. Black Bird Creek Marsh Company"] <- "March 17, 1829"
decisions$argued[decisions$case == "Cooley v. Board of Wardens"] <- "February 9-11, 1852"
decisions$argued[decisions$case == "Slaughterhouse Cases"] <- "January 11, 1872"
decisions$argued[decisions$case == "Quill Corp. v. North Dakota"] <- "January 22, 1992"
decisions$argued[decisions$case == "C & A Carbone, Inc. v. Town of Clarkstown"] <- "December 7, 1993"
decisions$argued[decisions$case == "Reno v. Condon"] <- "November 10, 1999"
decisions$argued[decisions$case == "Ex parte McCardle"] <- "March 2-4, 9, 1868"
decisions$argued[decisions$case == "Humphrey's Executor v. United States"] <- "May 1, 1935"
decisions$argued[decisions$case == "Printz v. United States"] <- "December 3, 1996"
decisions$argued[decisions$case == "Fletcher v. Peck"] <- "February 15, 1810"
decisions$argued[decisions$case == "Trustees of Dartmouth College v. Woodward"] <- "March 10-12, 1818"
decisions$argued[decisions$case == "Proprietors of Charles River Bridge v. Proprietors of Warren Bridge"] <- "January 20-26, 1837"
decisions$argued[decisions$case == "Cohens v. Virginia"] <- "February, 13, 19-20, 1821"
decisions$argued[decisions$case == "Kentucky v. Dennison"] <- "February 20, 1861"
decisions$argued[decisions$case == "Ex parte Milligan"] <- "March 5, 1866"
decisions$argued[decisions$case == "Munn v. Illinois"] <- "January 14-18, 1876"
decisions$argued[decisions$case == "Strauder v. West Virginia"] <- "October 20-21, 1879"
decisions$argued[decisions$case == "Ex parte Siebold"] <- "October 24, 1879"
decisions$argued[decisions$case == "Hurtado v. California"] <- "January 22-23, 1884"
decisions$argued[decisions$case == "Georgia v. McCollum"] <- "February 26, 1992"
decisions$argued[decisions$case == "United States v. Armstrong"] <- "February 26, 1996"
decisions$argued[decisions$case == "Ohio v. Robinette"] <- "October 8, 1996"
decisions$argued[decisions$case == "Chicago v. Morales"] <- "December 9, 1998"
decisions$argued[decisions$case == "Florida v. J.L."] <- "February 29, 2000"
decisions$argued[decisions$case == "Brown v. Board of Education (No. 2)"] <- "April 11-14, 1955"
decisions$argued[decisions$case == "Bragdon v. Abbott"] <- "March 30, 1998"
decisions$argued[decisions$case == "Boy Scouts of America v. Dale"] <- "April 26, 2000"

decisions$decided[decisions$case == "Adarand Constructors, Inc. v. Pena"] <- "June 12, 1995"
decisions$decided[decisions$case == "Communist Party of the United States v. Subversive Activities Control Bd. No. 12"] <- "April 30, 1956"
decisions$decided[decisions$case == "Ex Parte Garland"] <- "January 14, 1867"
decisions$decided[decisions$case == "Barron v. Mayor & City Council of Baltimore"] <- "February 16, 1833"
decisions$decided[decisions$case == "Foster & Elam v. Neilson"] <- "1829"
decisions$decided[decisions$case == "Atkins v. Virginia"] <- "June 20, 2002"
decisions$decided[decisions$case == "Ashcroft v. Free Speech Coalition"] <- "April 16, 2002"
decisions$decided[decisions$case == "Maryland v. Craig"] <- "June 27, 1990"
decisions$decided[decisions$case == "United States v. Lopez"] <- "April 26, 1995"
decisions$decided[decisions$case == "Nguyen v. INS"] <- "June 11, 2001"
decisions$decided[decisions$case == "Swift v. Tyson"] <- "January 25, 1842"
decisions$decided[decisions$case == "Talbot v. Janson"] <- "1795"
decisions$decided[decisions$case == "Ableman v. Booth"] <- "March 7, 1859"
decisions$decided[decisions$case == "Gibbons v. Ogden"] <- "March 2, 1824"
decisions$decided[decisions$case == "Willson v. Black Bird Creek Marsh Company"] <- "March 20, 1829"
decisions$decided[decisions$case == "Cooley v. Board of Wardens"] <- "March 2, 1852"
decisions$decided[decisions$case == "Slaughterhouse Cases"] <- "April 14, 1873"
decisions$decided[decisions$case == "Quill Corp. v. North Dakota"] <- "May 26, 1992"
decisions$decided[decisions$case == "C & A Carbone, Inc. v. Town of Clarkstown"] <- "May 16, 1994"
decisions$decided[decisions$case == "Reno v. Condon"] <- "January 12, 2000"
decisions$decided[decisions$case == "Ex parte McCardle"] <- "April 12, 1869"
decisions$decided[decisions$case == "United States v. Klein"] <- "January 29, 1872"
decisions$decided[decisions$case == "Kilbourn v. Thompson"] <- "February 28, 1881"
decisions$decided[decisions$case == "Printz v. United States"] <- "June 27, 1997"
decisions$decided[decisions$case == "Fletcher v. Peck"] <- "March 16, 1810"
decisions$decided[decisions$case == "Trustees of Dartmouth College v. Woodward"] <- "February 25, 1819"
decisions$decided[decisions$case == "Proprietors of Charles River Bridge v. Proprietors of Warren Bridge"] <- "February 14, 1837"
decisions$decided[decisions$case == "Cohens v. Virginia"] <- "March 5, 1821"
decisions$decided[decisions$case == "Kentucky v. Dennison"] <- "March 14, 1861"
decisions$decided[decisions$case == "Ex parte Milligan"] <- "April 3, 1866"
decisions$decided[decisions$case == "Munn v. Illinois"] <- "March 1, 1877"
decisions$decided[decisions$case == "Strauder v. West Virginia"] <- "March 1, 1880"
decisions$decided[decisions$case == "Ex parte Siebold"] <- "March 8, 1880"
decisions$decided[decisions$case == "Hurtado v. California"] <- "March 3, 1884"
decisions$decided[decisions$case == "Georgia v. McCollum"] <- "June 18, 1992"
decisions$decided[decisions$case == "United States v. Armstrong"] <- "May 13, 1996"
decisions$decided[decisions$case == "Ohio v. Robinette"] <- "November 18, 1996"
decisions$decided[decisions$case == "Chicago v. Morales"] <- "June 10, 1999"
decisions$decided[decisions$case == "Florida v. J.L."] <- "March 28, 2000"
decisions$decided[decisions$case == "Franklin v. Gwinnett County Public Schools"] <- "February 26, 1992"
decisions$decided[decisions$case == "Brown v. Board of Education (No. 1)"] <- "May 17, 1954"
decisions$decided[decisions$case == "Bragdon v. Abbott"] <- "June 25, 1998"
decisions$decided[decisions$case == "Boy Scouts of America v. Dale"] <- "June 28, 2000"

decisions$opinion[decisions$case == "Poelker v. Doe"] <- "Burger"
decisions$opinion[decisions$case == "Wygant v. Jackson Bd. of Educ."] <- "Powell"
decisions$opinion[decisions$case == "United States v. Paradise"] <- "Brennan"
decisions$opinion[decisions$case == "Northern Pipeline v. Marathon Pipe Line"] <- "Brennan"
decisions$opinion[decisions$case == "Furman v. Georgia"] <- "Per curiam"
decisions$opinion[decisions$case == "Jurek v. Texas"] <- "Stewart"
decisions$opinion[decisions$case == "Gomez v. United States District Court for the Northern District of California"] <- "Per curiam"
decisions$opinion[decisions$case == "Atkins v. Virginia"] <- "Stevens"
decisions$opinion[decisions$case == "Ashcroft v. Free Speech Coalition"] <- "Kennedy"
decisions$opinion[decisions$case == "Massachusetts v. Oakes"] <- "O'connor"
decisions$opinion[decisions$case == "United States v. Lopez"] <- "Rehnquist"
decisions$opinion[decisions$case == "Nguyen v. INS"] <- "Kennedy"
decisions$opinion[decisions$case == "Quill Corp. v. North Dakota"] <- "Stevens"
decisions$opinion[decisions$case == "Reno v. Condon"] <- "Rehnquist"
decisions$opinion[decisions$case == "Gravel v. United States"] <- "White"
decisions$opinion[decisions$case == "Buckley v. Valeo"] <- "Per curiam"
decisions$opinion[decisions$case == "Printz v. United States"] <- "Scalia"
decisions$opinion[decisions$case == "Holden v. Hardy"] <- "Brown"
decisions$opinion[decisions$case == "Brandenburg v. Ohio"] <- "Per curiam"
decisions$opinion[decisions$case == "Apodaca v. Oregon"] <- "White"
decisions$opinion[decisions$case == "Spence v. Washington"] <- "Per curiam"
decisions$opinion[decisions$case == "Moore v. City of East Cleveland"] <- "Powell"
decisions$opinion[decisions$case == "Richmond Newspapers"] <- "Burger"
decisions$opinion[decisions$case == "Florida v. Meyers"] <- "Per curiam"
decisions$opinion[decisions$case == "Georgia v. McCollum"] <- "Blackmun"
decisions$opinion[decisions$case == "United States v. Armstrong"] <- "Rehnquist"
decisions$opinion[decisions$case == "Ohio v. Robinette"] <- "Rehnquist"
decisions$opinion[decisions$case == "Chicago v. Morales"] <- "Stevens"
decisions$opinion[decisions$case == "Florida v. J.L."] <- "Ginsburg"
decisions$opinion[decisions$case == "Michael M. v. Superior Ct."] <- "Rehnquist"
decisions$opinion[decisions$case == "Bragdon v. Abbott"] <- "Kennedy"
decisions$opinion[decisions$case == "Boy Scouts of America v. Dale"] <- "Rehnquist"


saveRDS(justices, "justices.rds")
saveRDS(decisions, "decisions.rds")

justices <- readRDS("justices.rds")
decisions <- readRDS("decisions.rds")