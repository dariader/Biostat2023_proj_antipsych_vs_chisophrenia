library(dplyr)
library(here)
path <- here("scripts/0_preprocessing", "preprocessing.R")
source(path)
path <- here("data", "Data_SAS.xlsx")
df <- preprocessing(path)

rows <- c(
  "arm dropping",
  "shoulder shaking",
  "elbow rigidity",
  "wrist rigidity",
  "head rotation",
  "glabella tap",
  "tremor",
  "salivation",
  "akathisia"
)
df[,rows] = sapply(df[,rows], as.factor)
rows <-c(rows, "Total score SAS")

tbl_summary(
    df,
    include = rows,
    by = `antipsychotic generation`, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_gt() %>%
  gt::gtsave(filename = "generation_table.png")



library(dplyr)
library(gt)
library(ggstatsplot)

ggstatsplot::ggbarstats(df, y = `antipsychotic generation`, x = `akathisia`, type = "nonparametric")