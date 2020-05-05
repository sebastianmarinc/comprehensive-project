# r studio api code ------------------------------------------------
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# libraries
library(tidyverse)
library(xml2)
library(rvest)

# data importing and cleaning --------------------------------------

# number of hits from search
read_html("https://scholar.google.com/scholar?start=0&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24") %>%
  html_nodes("#gs_ab_md .gs_ab_mdw") %>%
  html_text %>%
  str_split(" ") %>%
  unlist -> hits
hits = as.numeric(hits[[2]])

# set page numbers from hit number assuming 10 hits per page
page = seq(0,floor(hits/10)*10, 10)

# create url's
url = paste("https://scholar.google.com/scholar?start=",page,
            "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24", sep = "")

# web scrape all pages from search on May 4, 2020 @ 5:03 pm
scholar = list()
for(i in seq_along(url)) {
  Sys.sleep(5)
  scholar[[i]] = read_html(url[[i]], )
}
#saveRDS(scholar, "scholar.rds") 

# titles
title = list()
for (i in seq_along(scholar)) {
  title[[i]] = str_remove_all(html_text(html_nodes(scholar[[i]], ".gs_rt")), 
                               "\\[.*?\\] ")
}
title = unlist(title)

# links
link = list()
for (i in seq_along(scholar)) {
  link[[i]] = html_attr(html_nodes(scholar[[i]], "h3 a, .gs_ctu"), "href")
}
link = unlist(link)

# author list, journal title, year
author_year_journal = list()
author = list()
year = list()
journal = list()

for(i in seq_along(scholar)) {
  author_year_journal[[i]] = html_nodes(scholar[[i]], ".gs_a") %>% html_text
  author[[i]] = gsub("^(.*?)\\W+-\\W+.*", "\\1", author_year_journal[[i]])
  year[[i]] = gsub("^.*(\\d{4}).*", "\\1", author_year_journal[[i]])
  journal[[i]] = ifelse(grepl(".*\\-\\W*(\\b.*)\\,+\\W+.*", 
                               author_year_journal[[i]]),
                         gsub(".*\\-\\W*(\\b.*)\\,+\\W+.*","\\1", 
                              author_year_journal[[i]]), NA
  )
}
author_year_journal = unlist(author_year_journal)
author = unlist(author)
year   = unlist(year)
journal = unlist(journal)

# cleaning up journal titles
journal = str_to_title(journal) %>% 
  str_replace_all(".*?(Traffic Psychology).*",
                  "Traffic Psychology and Behaviour") %>% 
  str_replace_all(".*?(Economic Psychology).*",
                  "Journal of Economic Psychology") %>% 
  str_replace_all(".*?(Mathematical Psychology).*",
                  "Journal of Mathematical Psychology") %>% 
  str_replace_all(".*?(Industrial Psychology).*",
                  "SA Journal of Industrial Psychology") %>% 
  str_replace_all(".*?(Of Clinical Psychology).*",
                  "Journal of Clinical Psychology") %>% 
  str_replace_all(".*?(In Experimental Social Psychology).*",
                  "Advances in Experimental Social Psychology") %>% 
  str_replace_all(".*?(Of Experimental Social Psychology).*",
                  "Journal Of Experimental Social Psychology") %>% 
  str_replace_all(".*?(Of Psychiatry and Clinical Psychology).*",
                  "Journal Of Psychiatry and Clinical Psychology") %>% 
  str_replace_all(".*?(Opinion In Psychology).*",
                  "Current Opinion in Psychology") %>% 
  str_replace_all(".*?(Psychology Of Sport).*",
                  "Psychology Of Sport And Exercise") %>% 
  str_replace_all("^(Clinical Psychology).*",
                  "Clinical Psychology Review") %>% 
  str_replace_all(".*(Psychiatry And Clinical).*",
                  "Journal of Psychiatry and Clinical Psychology") %>% 
  str_replace_all(".*(Experimental Child).*",
                  "Journal of Experimental Child Psychology") %>% 
  str_replace("And","and ") %>% 
  str_replace("Of ","of ") %>% 
  str_replace("In ","in ") %>% 
  str_remove("…") %>% 
  str_trim()
  
# scholar tibble
scholar_tbl = tibble(
  title = title,
  author = author,
  journal = fct_explicit_na(journal),
  year = as.numeric(year),
  link = link
)

# analysis ---------------------------------------------------------
top10 = scholar_tbl %>% 
  group_by(journal) %>%
  summarise(count = length(journal)) %>% 
  arrange(desc(count)) %>% 
  filter(journal != "(Missing)") %>% 
  top_n(10, wt = count)
top10

# visualization ----------------------------------------------------
ggplot(top10, aes(x = reorder(journal, count), y = count)) + 
  geom_col() + 
  coord_flip() +
  labs(title = "Top 10 Most Popular Journals",
       x = "Number of Publications",
       y = "Publication Outlet")
