# Script Settings and Resources
library(tidyverse) # Data Science tools
library(rvest) # Web scraping tool

# Data Import and Cleaning
pages <- c(
  "https://www.cnbc.com/business/" = "Business", # Defining the urls of interest and the `source` name of each page for the cnbc_tbl
  "https://www.cnbc.com/investing/" = "Investing",
  "https://www.cnbc.com/technology/" = "Tech",
  "https://www.cnbc.com/politics/" = "Politics"
)

cnbc_tbl <- tibble( # this creates an empty tibble for the for loop to run through and populate based on variables of interest
  headline = character(),
  length = integer(),
  source = character()
)

for (page in names(pages)) { # the for loop is going to cycle through my four `pages` one `page` at a time
  source_name <- pages[page]
  
  #scrape headline
  headlines <- read_html(page) %>% 
    html_elements(".Card-title") %>% # I used SelectorGadget in Chrome to find this css style of the headlines (it worked on all four pages)
    html_text()
  
  # create a tibble for each page
  page_tbl <- tibble(
    headline = headlines,
    length = str_count(headline, "\\S+"), #I used regex to count words based on if there was a space between them.
    source = source_name
  )
  
  #add to cnbc_tbl
  cnbc_tbl <- bind_rows(cnbc_tbl, page_tbl)
}

# Visualization
ggplot(cnbc_tbl, aes(x = source, y = length)) + #Using ggplot visualization of boxplot for 4x categorical and 1x continuous variable.
  geom_boxplot() +
  scale_x_discrete("CNBC Source Page") +
  scale_y_continuous("Headline Length (words)") +
  labs(title = "CNBC Headline Lengths by Source Page")

# Analysis
cnbc_tbl <- cnbc_tbl %>% # I made sure that the source was factored before doing any ANOVAs
  mutate(source = factor(source, levels = c("Business", "Investing", "Tech", "Politics")))

cnbc_aov <- aov(length ~ source, data = cnbc_tbl) # First needed to create the formula to inform the ANOVA
summary(cnbc_aov) # This function prints out the results of the ANOVA in an a Table.

# Publication
str_c(
  "The results of an ANOVA comparing lengths across sources was F(",
  round(summary(cnbc_aov)[[1]]$Df[1], 0), #selects df
  ", ",
  round(cnbc_aov$df.residual, 0), #select residual df
  ") = ", 
  str_remove(formatC(summary(cnbc_aov)[[1]]$`F value`[1], format = "f", digits = 2), "^0"), # selects F test value; used sprintf as in Part 2
  ", p = ", 
  str_remove(formatC(summary(cnbc_aov)[[1]]$`Pr(>F)`[1], format = "f", digits = 2), "^0"),  # selects p.value
  ". This test ", 
  ifelse(summary(cnbc_aov)[[1]]$`Pr(>F)`[1] < 0.05, "was", "was not"), # runs a logical test evaluating p.value at p < .05; would need to specify this significance value in publication
  " statistically significant.", 
  sep = ""
)
# "The results of an ANOVA comparing lengths across sources was F(3, 130) = 2.12, p = .10. This test was not statistically significant."



