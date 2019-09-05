library(tidyverse)

# The raw data files are exported from the full database tables, using the 
# August 30, 2019 snapshot from https://github.com/blekhmanlab/rxivist

articles <- read_csv("prod_articles.csv")
traffic <- read_csv("prod_article_traffic.csv")
pub_dates <- read_csv("prod_publication_dates.csv") %>%
    rename(pub_date = date)

db <- left_join(
    articles %>% 
        select(id, doi, collection, posted, author_vector, title), 
    traffic %>% 
        select(id = article, month, year, views = abstract, downloads = pdf), 
    by = "id")

# identify last possible day on which traffic data was collected
db <- db %>%
    mutate(traffic_month = (month %% 12) + 1, 
           traffic_year = ifelse(traffic_month == 1, year + 1, year), 
           traffic_date = as.Date(paste(traffic_year, traffic_month, 01, sep = "-")) - 1, 
           posted_plus_3 = posted + months(3), 
           posted_plus_4 = posted + months(4))

saveRDS(db, file = "raw_stats.RDS")

# compute paper stats
paper_stats <- db %>%
    filter(traffic_date >= posted, 
           traffic_date <= posted_plus_3) %>% 
    group_by(id) %>%
    summarize(downloads = sum(downloads), 
              views = sum(views), 
              traffic_date = max(traffic_date), 
              posted = first(posted), 
              collection = first(collection)) %>%
    mutate(duration = as.numeric(traffic_date - posted + 1), 
           downloads_per_day = downloads / duration, 
           views_per_day = views / duration) %>%
    rename(posted_date = posted)

#### check calculations for paper id == 386
# db %>% filter(id == 386) %>% 
#     select(id, downloads, views, traffic_date, posted) %>% 
#     arrange(desc(traffic_date))
# 
# paper_stats %>% filter(id == 386)

# include publication dates if available
paper_stats <- paper_stats %>%
    left_join(pub_dates, by = c("id" = "article"))

saveRDS(paper_stats, file = "paper_stats.RDS")
