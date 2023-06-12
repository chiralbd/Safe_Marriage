# Load packages 
library(tidyverse)
library(gtsummary)
library(officer)
library(flextable)
library(readxl)
library(forcats) 

# read data 
data <- read_excel("data/Safe_Marriages_Clean.xlsx")

# Table 1. Demographic characteristics of the participants
table1 <- data %>% 
  select(5:13) %>% 
  tbl_summary(type = all_categorical() ~ "categorical") %>% 
  as_flex_table()
table1
save_as_docx(table1, path = "tables/Table1.docx")

# Filter data 
df <- filter(data, `Have you previously heard of thalassemia?` == "Yes")


# Table 2. Sources of information regarding thalassemia 


# Table 3. Association between demographic variables and level of thalassaemia knowledge
table3 <- df %>% 
  select(KnowledgeLevel, 5:12) %>% 
  tbl_summary(type = all_categorical() ~ "categorical", 
              by = "KnowledgeLevel") %>% 
  add_overall() %>% 
  add_p() %>% 
  bold_p() %>% 
  as_flex_table()
table3
save_as_docx(table3, path = "tables/Table3.docx")

# Table 4. Knowledge related questions among students who have heard about thalassaemia 
table4 <- df %>% 
  select(`Field of Study`, 19:30) %>% 
  tbl_summary(type = all_categorical() ~ "categorical", 
              by = "Field of Study") %>% 
  add_p()%>% 
  bold_p() %>% 
  as_flex_table()
table4
save_as_docx(table4, path = "tables/Table4.docx")


# Table 5. Attitudes towards thalassemia awareness among participants 
table5 <- df %>% 
  select(31:41) %>% 
  tbl_summary(type = all_categorical() ~ "categorical") %>% 
  as_flex_table()

table5
save_as_docx(table5, path = "tables/Table5.docx")


# Table 6. Opinion of the respondents regarding pre-marital thalassemia screening
table6 <- df %>% 
  select(55:59) %>% 
  tbl_summary(type = all_categorical() ~ "categorical", 
              missing = "no")
table6
save_as_docx(table6, path = "tables/Table6.docx")


# Table 7. Opinion of the respondents regarding safe marriage  
table7 <- df %>% 
  select(60:66) %>% 
  tbl_summary(type = all_categorical() ~ "categorical", 
              missing = "no")
table7
save_as_docx(table7, path = "tables/Table7.docx")


# Table 8. Factors associated with knowledge 
df$KnowledgeLevel <- as.factor(df$KnowledgeLevelCoded)
table8 <- df %>% 
  select(KnowledgeLevelCoded, Age, Gender, `Marital Status`, `Where are you from?`,
       `Type of Institute`, 
       `Year of Study`,
       `Economic Status`) %>%
  tbl_uvregression(
    method = glm,
    y = KnowledgeLevelCoded,
    method.args = list(family = binomial(link = "logit")),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)
  ) %>%
  bold_p() %>%        # bold p-values under a given threshold (default 0.05)
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")
table8
save_as_docx(table8, path = "tables/Table8.docx")
