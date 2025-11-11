## Bringing data in
library(readr)
library(ggplot2)
library(hexbin)
library(tidyverse)
library(tidyquant)
library(ggcorrplot)
library(patchwork)
postings <- read_csv("C:\\Users\\tjqho\\Downloads\\postings.csv.zip")
companies <- read_csv("C:\\Users\\tjqho\\Downloads\\companies.csv.zip")
company_industries <- read_csv("C:\\Users\\tjqho\\Downloads\\company_industries.csv")
company_specialities <- read_csv("C:\\Users\\tjqho\\Downloads\\company_specialities.csv")
employee_counts <- read_csv("C:\\Users\\tjqho\\Downloads\\employee_counts.csv")
benefits <- read_csv("C:\\Users\\tjqho\\Downloads\\benefits.csv")
job_industries <- read_csv("C:\\Users\\tjqho\\Downloads\\job_industries.csv")
job_skills <- read_csv("C:\\Users\\tjqho\\Downloads\\job_skills.csv")
salaries <- read_csv("C:\\Users\\tjqho\\Downloads\\salaries.csv")
industries <- read_csv("C:\\Users\\tjqho\\Downloads\\industries.csv")
skills <- read_csv("C:\\Users\\tjqho\\Downloads\\skills.csv")

target_skills <- c("ADVR", "PRDM", "DIST", "EDU", "TRNG", "PRJM", "CNSL", "PRCH",
                   "SUPL", "ANLS", "RSCH", "SCI", "GENB", "STRA", "FIN", "LGL",
                   "ENG", "BD", "IT", "ADM", "PROD", "MRKT", "ACCT", "HR",
                   "MNFC", "SALE", "MGMT")

job_skills_filtered <- job_skills %>%
  filter(skill_abr %in% target_skills)

job_skill_counts <- job_skills_filtered |>
  group_by(job_id) |>
  summarise(num_relevant_skills = n())

job_skill_counts <- postings |>
  select(job_id, company_id) |>
  left_join(job_skill_counts, by = "job_id") |>
  mutate(num_target_skills = replace_na(num_relevant_skills, 0)) |>
  select(job_id,num_target_skills)

postings_named <- postings %>%
  left_join(job_industries, by = "job_id", relationship = "many-to-many") %>%
  left_join(industries, by = "industry_id", relationship = "many-to-many") %>%
  left_join(companies, by = "company_id", relationship = "many-to-many") %>%
  left_join(
    employee_counts %>%
      dplyr::select(
        company_id,
        employee_count,
        follower_count
      ),
    by = "company_id",
    relationship = "many-to-many"
  ) %>%
  left_join(job_skill_counts, by = "job_id") %>%
  mutate(num_target_skills = tidyr::replace_na(num_target_skills, 0))

top_sectors <- c("Financial Services", "IT Services and IT Consulting",
                 "Biotechnology Research")

# filtering all postings (joined) by top 2 sectors
postings_top_sec <- postings_named %>%
  filter(industry_name %in% top_sectors)


sector_thresholds <- postings_top_sec %>%
  group_by(industry_name) %>%
  dplyr::summarise(
    salary_threshold = quantile(normalized_salary, 0.20, na.rm = TRUE),
    .groups = 'drop'
  )

postings_top_sec <- postings_top_sec %>%
  left_join(
    sector_thresholds %>%
      dplyr::rename(sector_threshold = salary_threshold),
    by = "industry_name"
  )

postings_top_sec_NO_NA <- postings_top_sec %>%
  mutate(
    skilled_job = ifelse(
      num_target_skills >= 1,
      1, # TRUE: is skilled job
      0  # FALSE: is not a skilled job
    )
  )
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

postings_top_sec_NO_NA_unique <- postings_top_sec_NO_NA %>%
  group_by(job_id) %>%
  summarise(
    across(
      .cols = -c(follower_count, employee_count),
      .fns = ~ first(.x)
    ),
    follower_count = safe_max(follower_count),
    employee_count = safe_max(employee_count),
    .groups = "drop"
  )
company_summary_NO_NA <- postings_top_sec_NO_NA_unique %>%
  group_by(company_id, company_name, industry_name) %>%
  summarise(
    total_postings = n(),
    skilled_postings = sum(skilled_job, na.rm = TRUE),
    employee_count = safe_max(employee_count),
    follower_count = safe_max(follower_count),
    .groups = "drop"
  )

company_summary_NO_NA <- company_summary_NO_NA %>%
  na.omit()

company_summary_NO_NA_clean <- company_summary_NO_NA %>%
  filter(employee_count != 0)
company_summary_NO_NA_clean <- company_summary_NO_NA_clean %>%
  filter(skilled_postings != 0)

postings_summary <- postings |>
  filter(is.na(company_id) == FALSE) |>
  left_join(employee_counts, by = "company_id") |>
  left_join(company_industries, by = "company_id") |>
  group_by(company_id) |>
  summarise(company_name = first(company_name),
            employee_count = max(employee_count),
            follower_count = max(follower_count),
            num_postings = n(),
            sector = max(industry)
  )

postings_summary |>
  ggplot(aes(x = log(employee_count),y = log(num_postings))) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(x = "log(Employee Count)",y = "log(Number of Postings)",
       title = "Scatter Plot of # Postings by Employee Count")
plot_postings <- postings_summary |>
  filter(sector %in% c("Financial Services",
                       "IT Services and IT Consulting"))



test <- postings |>
  group_by(company_id) |>
  summarise(count = n(),name = min(company_name))

test2 <- postings |>
  filter(str_detect(company_name,"Thermo Fisher"))
sum(is.na(postings$description))


















## PLOTS










## 2 Color Scatter Plot
p5 <- company_summary_NO_NA_clean |>
  ggplot(aes(x = log(employee_count),y = log(skilled_postings),color = factor(industry_name))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",se = FALSE) +
  annotate("text",label = 
             "Lines fitted using\n Ordinary Least Squares",
           xmin = 0, xmax = 5,x = 3,
           y = 3.5) +
  labs(x = "Employee Count (Log)",y = "Number of Skilled Postings (Log)",
       title = "Log-Transformed Number of Skilled Postings \nby Log-Transformed Employee Count",
       color = "Sector") +
  theme_bw()

p6 <- company_summary_NO_NA_clean |>
  ggplot(aes(x = follower_count,y = log(skilled_postings),color = factor(industry_name))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",se = FALSE) +
  annotate("text",label = 
             "Smooth lines fitted\nusing LOESS regression",
           xmin = 0, xmax = 5,x = 3,
           y = 3.5) +
  labs(x = "Follower Count",y = "Number of Skilled Postings",
       title = "Log-Transformed Number of Skilled Postings \nby Follower Count",
       color = "Sector") +
  theme_bw()

p5 + p6
## Number Postings Hist
p1 <- company_summary_NO_NA_clean |>
  ggplot(aes(x = skilled_postings)) +
  geom_histogram(aes(y = after_stat(density)),fill = "darkorange3") +
  labs(x = "Number of Skilled Postings",
       y = "Density",
       title = "Distribution of Number of Skilled Postings") +
  theme_bw()

## Employee Count Hist
p2 <- company_summary_NO_NA_clean |>
  ggplot(aes(x = employee_count)) +
  geom_histogram(aes(y = after_stat(density)),fill = "darkolivegreen") +
  labs(x = "Employee Count",
       y = "Density",
       title = "Distribution of Employee Count") +
  theme_bw()
p3 <- company_summary_NO_NA_clean |>
  ggplot(aes(x = log(skilled_postings))) +
  geom_histogram(aes(y = after_stat(density)),fill = "darkorange3",bins = 10) +
  labs(x = "log(Number of Skilled Postings)",
       y = "Density",
       title = "Distribution of log(Number of Skilled Postings)") +
  theme_bw()
p4 <- company_summary_NO_NA_clean |>
  ggplot(aes(x = log(employee_count))) +
  geom_histogram(aes(y = after_stat(density)),fill = "darkolivegreen") +
  labs(x = "log(Employee Count)",
       y = "Density",
       title = "Distribution of log(Employee Count)") +
  theme_bw()
(p1 + p2) / (p3 + p4)
