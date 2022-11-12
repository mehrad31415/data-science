# this script just contains the codes. For details and descriptions please see the html which contains the codes and descriptions all together.

library (tidyverse)
library (ggrepel)
cdc <- read_delim("Data/CDC.txt", delim = "\t", show_col_types = FALSE)
dummy <- as.data.frame(cdc)
df_cdc  <- data.frame(t(dummy [,-1]))
colnames(df_cdc) <- dummy[,1]
df_cdc <- df_cdc %>% rownames_to_column(var = "year")
df_cdc <- df_cdc %>% 
  rowwise(year) %>% 
  mutate (cancer = sum(c_across(cancer_all:cancer_breast))) %>%
  rename (respiratory= loresp, pneumonia = influpneu) %>%
  mutate (homicide = sum(c_across(homicide_all:homicide_legmil)))


cdc <- df_cdc %>% 
  select (- (starts_with("homicide_") | starts_with("cancer_") )) %>% 
  mutate (label = "cdc") 
cdc <- as_tibble(cdc)
google <- read_csv("Data/Google-trends.csv", show_col_types = FALSE)
google <- google %>%
  rename(year=Words, alzheimer = `alzheimer's`, accident = `car accidents`, heart = `heart disease`, kidney = `kidney disease`, 
         respiratory = `respiratory disease`) %>%
  filter (year != "Average") %>%
  mutate (label = "google")
google <- as_tibble(google)
nyt <-
  list.files(path = "Data/NYT", full.names = TRUE) %>%
  map_df(read_csv, show_col_types = FALSE)
nyt <- nyt %>% group_by(year) %>% mutate (percentage = count/ sum (count)) %>% select (Words, year, percentage)
nyt <- nyt %>% pivot_wider(names_from = year, values_from = percentage)
dummy <- as.data.frame(nyt)
df_nyt  <- data.frame(t(dummy [,-1]))
colnames(df_nyt) <- dummy[,1]
nyt <- df_nyt %>% rownames_to_column(var = "year")
nyt <- nyt %>% rowwise(year) %>% 
  mutate (heart = sum(c_across(`heart disease`:`cardiovascular disease`))) %>%
  mutate (cancer = sum(c_across(`cancer`:`malignant neoplasms`))) %>%
  mutate (respiratory = sum(c_across(`respiratory disease`:`asthma`))) %>%
  mutate (accident = sum(c_across(`unintentional injuries`:`car crash`))) %>%
  mutate (alzheimer = sum(c_across(`stroke`:`alzheimer's disease`))) %>%
  mutate (pneumonia= sum(c_across(Influenza:flu))) %>%
  mutate (kidney= sum(c_across(`kidney disease`:`nephrotic syndrome`))) %>%
  mutate (suicide= sum(c_across(`suicide`:`self-harm`))) %>%
  mutate (homicide= sum(c_across(homicide:assassination)) + sum(c_across(shootings:lynching))) %>% 
  mutate (terrorism= sum(c_across(terrorism:`terror attack`))) %>%
  rename (overdose= `drug overdose`)

nyt <- nyt%>% 
  select (- (contains("disease") | ends_with("failure") | starts_with("malignant") | contains("bronchitis") |
               contains("emphysema") | contains("asthma") | contains("unintentional") | contains("car ") |
               contains("pileup") | contains("flu") | contains("neph") | contains("self") | contains("man") | 
               contains("terrorist") | contains("terror attack") | contains("murder") | contains("assass") |
               contains("shoot") | contains("knif") | contains("gun") | contains("lynch") | contains("drug"))) %>% 
  mutate (label = "NYT")
final <- rbind(google,nyt,cdc)
(nyt)
View (cdc)
View (google)
View (final)
final %>%
  pivot_longer(!label & !year, names_to = "cause", values_to = "count") %>%
  mutate (year=as.numeric(year)) %>%
  ggplot (mapping=aes(x=year,y=count, color= cause)) + 
  geom_line() + 
  facet_grid(vars(label), vars(cause))





