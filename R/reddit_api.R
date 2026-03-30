# Script Settings and Resources
library(tidyverse) # for data analysis functions
library(RedditExtractoR) # Utilized RedditExtractoR and found two helpful functions

# Data Import and Cleaning
rstats_urls <- find_thread_urls(subreddit="rstats", period = "month") # uses RedditExtractoR to grab a month's worth of posts from the `rstats` subreddit.
rstats_contents <- get_thread_content(rstats_urls$url) # parses the urls and grabs the metadata associated with each thread
str(rstats_contents$threads) # this call allowed me to search for the right variables `title` (chr), `upvotes` (num), and `comments` (num) to build my tibble

rstats_tbl <- rstats_contents$threads %>% # use tidyverse to `select` my columns of interest
  select(title, upvotes, comments) %>% 
  rename(post = title) # renaming title to post IAW line 4.1.1

# Visualization
ggplot(rstats_tbl, aes(x=upvotes, y = comments)) + #ggplot created scatterplot (jittered - but inconsequential) with a linear model to estimate the strength of the relationship
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "#0000FF") +
  scale_x_continuous("Upvotes") +
  scale_y_continuous("# of Comments") + 
  labs(title = "30 Days of /rstats Subreddit Upvotes and Comments") +
  theme_minimal()

# Analysis
cor_results <- cor.test(rstats_tbl$upvotes, rstats_tbl$comments) # cor.test() stores the results of the two variables of interest to call for later as a cor_result variable in the environment
cor_results$estimate # correlation
cor_results$p.value # p-value

# Publication
str_c(
  "The correlation between upvotes and comments was r(",
  round(cor_results$parameter, 0), #selects df
  ") = ", 
  str_remove(formatC(cor_results$estimate, format = "f", digits = 2), "^0"), # selects r value; researched sprintf function in <base>; also experimented with formatC; I did this because without some kind of formatting rules, the p.value was returning a " " empty value when the zero was removed from str_remove
  ", p = ", 
  str_remove(formatC(cor_results$p.value, format = "f", digits = 2), "^0"),  # selects p.value
  ". This test ", 
  ifelse(cor_results$p.value < 0.05, "was", "was not"), # runs a logical test evaluating p.value at p < .05; would need to specify this significance value in publication
  " statistically significant.", 
  sep = ""
  )
# "The correlation between upvotes and comments was r(70) = .34, p = .00. This test was statistically significant."

