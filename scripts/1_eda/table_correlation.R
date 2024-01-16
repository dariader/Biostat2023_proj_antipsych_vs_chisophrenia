# Create a simple dataframe
library(dplyr)
path <- here("scripts/0_preprocessing", "preprocessing.R")
source(path)
path <- here("data", "Data_SAS.xlsx")
df <- preprocessing(path)
df$course <- df$course %>% as.factor() %>% as.numeric()
rows <- c("ZVM", "ZDS", "ZMT", "ZVF", "ZSC", "ZToL", "Comp Z")
get_str_correlations <- function(df,colname,visit_col,rows){
  res <- c()
  print('###')
  print(colname)
  if(visit_col == 'all'){
    for(i in rows){
    for(visit_row in c(1,2)){
       rw <- df %>% filter(visit == {{ visit_row }} ) %>% pull({{ i }}) %>% as.character() %>%  as.numeric()
       cl <- df %>% filter(visit == {{ visit_row }} ) %>% pull({{ colname }}) %>% as.character() %>%  as.numeric()
        print(i)
       # print(length(rw))
       # print(typeof(rw))
       cv <- corci(cl, rw, method = "spearman", nboot = 1000)
      if (!is.na(cv$p.value) & cv$p.value < 0.05){
        res <- c(res, paste0(as.character(round(cv$estimate,3)), "*"))
         #print(paste0(colname,visit_col,"vs",i,as.character(round(cv$estimate,3)), ": significant ",cv$p.value))
      }
         else {
        if(is.na(cv$p.value)){
          res <- c(res, 'NA')
        }
        else {
          res <- c(res, paste0(as.character(round(cv$estimate,3))))
        }
         #print(paste0(colname,visit_col,"vs",i,as.character(round(cv$estimate,3)), ": not significant ",cv$p.value))
      }

    }
  }
  } else{
    cl <- df %>% filter(visit == {{ visit_col }} ) %>% pull({{ colname }}) %>% as.character() %>%  as.numeric()
    for(i in rows){
      for(visit_row in c(1,2)){
         rw <- df %>% filter(visit == {{ visit_row }} ) %>% pull({{ i }}) %>% as.character() %>%  as.numeric()
         # print(i)
         # print(length(rw))
         # print(typeof(rw))
         cv <- corci(cl, rw, method = "spearman", nboot = 1000)
        if (!is.na(cv$p.value) & cv$p.value < 0.05){
          res <- c(res, paste0(as.character(round(cv$estimate,3)), "*"))
           print(paste0(colname,visit_col,"vs",i,as.character(round(cv$estimate,3)), ": significant ",cv$p.value))
          } else {
            if(is.na(cv$p.value)){
            res <- c(res, 'NA')
          }
          else {
            res <- c(res, paste0(as.character(round(cv$estimate,3))))
          }
        }

      }
    }
  }
  print(length(res))
  return(res)
}

your_data <- data.frame(
  row_names = c("visit", rep(c("ZVM", "ZDS", "ZMT", "ZVF", "ZSC", "ZToL", "Comp Z"), each = 2)),
  visits = c("", rep(c("week 2", "week 8"), times = 7)),
  gait = c("week 2", get_str_correlations(df, 'gait', 1, rows)),
  gait =  c("week 8",get_str_correlations(df, 'gait', 2, rows)),
  arm_dropping =c("week 2", get_str_correlations(df, 'arm dropping', 1, rows)),
  arm_dropping = c("week 8", get_str_correlations(df, 'arm dropping', 2, rows)),
  depression = c("week 2", get_str_correlations(df, 'depression', 1, rows)),
  depression =  c("week 8", get_str_correlations(df, 'depression', 2, rows)),
  shoulder_shaking = c("week 2", get_str_correlations(df, 'shoulder shaking', 1, rows)),
  shoulder_shaking = c("week 8", get_str_correlations(df, 'shoulder shaking', 2, rows)),
  elbow_rigidity =  c("week 2", get_str_correlations(df, 'elbow rigidity', 1, rows)),
  elbow_rigidity = c("week 8", get_str_correlations(df, 'elbow rigidity', 2, rows)),
  wrist_rigidity = c("week 2", get_str_correlations(df, 'wrist rigidity', 1, rows)),
  wrist_rigidity = c("week 8", get_str_correlations(df, 'wrist rigidity', 2, rows)),
  head_rotation = c("week 2", get_str_correlations(df, 'head rotation', 1, rows)),
  head_rotation = c("week 8", get_str_correlations(df, 'head rotation', 2, rows)),
  glabella_tap = c("week 2", get_str_correlations(df, 'glabella tap', 1, rows)),
  glabella_tap = c("week 8", get_str_correlations(df, 'glabella tap', 2, rows)),
  tremor = c("week 2", get_str_correlations(df, 'tremor', 1, rows)),
  tremor = c("week 8", get_str_correlations(df, 'tremor', 2, rows)),
  salivation = c("week 2", get_str_correlations(df, 'salivation', 1, rows)),
  salivation = c("week 8", get_str_correlations(df, 'salivation', 2, rows)),
  akathisia = c("week 2", get_str_correlations(df, 'akathisia', 1, rows)),
  akathisia = c("week 8", get_str_correlations(df, 'akathisia', 2, rows)),
  Total_score_SAS = c("week 2", get_str_correlations(df, 'Total score SAS', 1, rows)),
  total_score_SAS = c("week 8", get_str_correlations(df, 'Total score SAS', 2, rows))
)
# Print the dataframe
print(gt(your_data))
write.csv(your_data, 'table1.csv', quote = FALSE)

your_data <- data.frame(
  row_names = c("visit", rep(c("ZVM", "ZDS", "ZMT", "ZVF", "ZSC", "ZToL", "Comp Z"), each = 2)),
  visits = c("", rep(c("week 2", "week 8"), times = 7)),
 #positive		negative		depression		cognition		excitment		Total score PANSS
  positive = c("week 2", get_str_correlations(df, 'positive', 1, rows)),
  positive =  c("week 8",get_str_correlations(df, 'positive', 2, rows)),
  negative = c("week 2", get_str_correlations(df, 'negative', 1, rows)),
  negative =  c("week 8",get_str_correlations(df, 'negative', 2, rows)),
  depression = c("week 2", get_str_correlations(df, 'depression', 1, rows)),
  depression =  c("week 8",get_str_correlations(df, 'depression', 2, rows)),
  cognitive = c("week 2", get_str_correlations(df, 'cognitive', 1, rows)),
  cognitive =  c("week 8",get_str_correlations(df, 'cognitive', 2, rows)),
  excitment = c("week 2", get_str_correlations(df, 'excitment', 1, rows)),
  excitment =  c("week 8",get_str_correlations(df, 'excitment', 2, rows)),
  'Total score PANSS' = c("week 2", get_str_correlations(df, 'Total score PANSS', 1, rows)),
  'Total score PANSS'  =  c("week 8",get_str_correlations(df, 'Total score PANSS', 2, rows)),
  # CPZE	antipsychotic generation	THF dose	age	disease duration	course	smoke
   CPZE = c("week 2", get_str_correlations(df, 'CPZE', 'all', rows)),
  'antipsychotic generation' = c("all visits", get_str_correlations(df, 'antipsychotic generation', 'all', rows)),
  'THF dose' = c("within visits", get_str_correlations(df, 'THF dose', 'all', rows)),
  'age' = c("within visits", get_str_correlations(df, 'age', 'all', rows)),
  'disease duration' = c("within visits", get_str_correlations(df, 'disease duration', 'all', rows)),
  'course' = c("within visits", get_str_correlations(df, 'course', 'all', rows)),
  'smoke' = c("within visits", get_str_correlations(df, 'smoke', 'all', rows))
)
print(gt(your_data))
write.csv(your_data, 'table2.csv', quote = FALSE)
rows <- c(
  "arm dropping",
  "shoulder shaking",
  "elbow rigidity",
  "wrist rigidity",
  "head rotation",
  "glabella tap",
  "tremor",
  "salivation",
  "akathisia",
  "Total score SAS"
)
your_data <- data.frame(
  row_names = c("visit", rep(rows, each = 2)),
  visits = c("", rep(c("week 2", "week 8"), times = length(rows))),
 #positive		negative		depression		cognition		excitment		Total score PANSS
  positive = c("week 2", get_str_correlations(df, 'positive', 1, rows)),
  positive =  c("week 8",get_str_correlations(df, 'positive', 2, rows)),
  negative = c("week 2", get_str_correlations(df, 'negative', 1, rows)),
  negative =  c("week 8",get_str_correlations(df, 'negative', 2, rows)),
  depression = c("week 2", get_str_correlations(df, 'depression', 1, rows)),
  depression =  c("week 8",get_str_correlations(df, 'depression', 2, rows)),
  cognitive = c("week 2", get_str_correlations(df, 'cognitive', 1, rows)),
  cognitive =  c("week 8",get_str_correlations(df, 'cognitive', 2, rows)),
  excitment = c("week 2", get_str_correlations(df, 'excitment', 1, rows)),
  excitment =  c("week 8",get_str_correlations(df, 'excitment', 2, rows)),
  'Total score PANSS' = c("week 2", get_str_correlations(df, 'Total score PANSS', 1, rows)),
  'Total score PANSS'  =  c("week 8",get_str_correlations(df, 'Total score PANSS', 2, rows)),
  # CPZE	antipsychotic generation	THF dose	age	disease duration	course	smoke
   CPZE = c("week 2", get_str_correlations(df, 'CPZE', 'all', rows)),
  'antipsychotic generation' = c("within visits", get_str_correlations(df, 'antipsychotic generation', 'all', rows)),
  'THF dose' = c("within visits", get_str_correlations(df, 'THF dose', 'all', rows)),
  'age' = c("within visits", get_str_correlations(df, 'age', 'all', rows)),
  'disease duration' = c("within visits", get_str_correlations(df, 'disease duration', 'all', rows)),
  'course' = c("within visits", get_str_correlations(df, 'course', 'all', rows)),
  'smoke' = c("within visits", get_str_correlations(df, 'smoke', 'all', rows))
)
print(gt(your_data))
write.csv(your_data, 'table3.csv', quote = FALSE)
