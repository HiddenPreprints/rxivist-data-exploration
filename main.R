library(tidyverse)



views_quantiles <- paper_stats %>%
    filter(is.na(pub_date) | pub_date > traffic_date) %>%
    select(collection, views_per_day, downloads_per_day) %>%
    nest(-collection) %>%
    mutate(views_q = map(data, ~ quantile(.$views_per_day, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))), 
           #           downloads_q = map(data, ~ quantile(.$downloads_per_day, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)))
           views_q = map(views_q, ~ bind_rows(.) %>% gather())) %>%
    select(-data) %>% 
    unnest()

ggplot(views_quantiles, 
       aes(x = 1, y = value, color = key)) + 
    geom_point() + 
    facet_wrap(~collection) + 
    theme_bw()
# summarize_at(vars(views_per_day, downloads_per_day), 
#              quantile, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
    # # summarize(views_q = quantile(views, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)), 
    #           downloads_q = quantile(views, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)))



selected_ids <- sample(to_plot$id, 100)
temp <- db %>%
    filter(id %in% selected_ids) %>%
    mutate(id = factor(id)) %>%
    group_by(id) %>%
    mutate(views_scaled = views / max(views))
ggplot(temp, 
       aes(x = traffic_date - posted, y = views, color = id)) + 
    geom_line() + 
    theme_bw() + 
    guides(color = "none")






ggplot(dat %>% filter(collection == "bioengineering"), 
       aes(x = posted, y = views_per_day)) + 
    geom_point() + 
    geom_quantile() + 
    coord_cartesian(ylim = c(0, 200)) + 
    theme_bw()





views_month_1 <- db %>% 
    group_by(collection) %>%
    filter(month == posted_month, 
           year == posted_year)

ggplot(to_plot, 
       aes(x = views_per_day, y = downloads_per_day)) + 
    geom_point() + 
    facet_wrap(~ collection, scales = "free") + 
    theme_bw(base_size = 20)
