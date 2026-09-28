library(tidyverse)
library(nhanesA)

dir.create("data", showWarnings = FALSE)

check <- function(x, label) {
  n <- if (is.null(x)) 0 else nrow(x)
  cat(sprintf("%-32s %6d rows\n", label, n))
  if (n == 0) stop("Step '", label, "' has 0 rows. Stop and check it.")
  x
}
get_nhanes <- function(table) {
  path <- file.path("data", paste0(table, ".Rds"))
  if (file.exists(path)) return(check(readRDS(path), paste(table, "(saved)")))
  x <- nhanes(table)
  x <- check(if (is.null(x)) NULL else as_tibble(x), paste(table, "(downloaded)"))
  saveRDS(x, path)
  x
}

demo <- get_nhanes("P_DEMO")  
bmx  <- get_nhanes("P_BMX")   
bpx  <- get_nhanes("P_BPXO")   
smq  <- get_nhanes("P_SMQ")    

demo <- demo %>% select(SEQN, RIDSTATR, RIDAGEYR, RIAGENDR, RIDRETH3)
bmx  <- bmx  %>% select(SEQN, BMXBMI)
bpx  <- bpx  %>% select(SEQN, BPXOSY1, BPXOSY2, BPXOSY3)
smq  <- smq  %>% select(SEQN, SMQ020, SMQ040)

combined <- demo %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(bpx, by = "SEQN") %>%
  left_join(smq, by = "SEQN") %>%
  check("After merging")

combined <- combined %>%
  filter(RIDSTATR == "Both interviewed and MEC examined" | as.character(RIDSTATR) == "2") %>%
  check("Interviewed + examined") %>%
  filter(RIDAGEYR >= 21, RIDAGEYR < 80) %>%
  check("Adults aged 21-79")

dashboard_data <- combined %>%
  mutate(
    across(c(RIDAGEYR, BMXBMI, BPXOSY1, BPXOSY2, BPXOSY3), as.numeric),
    sex  = factor(as.character(RIAGENDR)),
    race = factor(as.character(RIDRETH3)),
    smoking = case_when(
      as.character(SMQ020) == "No"                           ~ "Never",
      as.character(SMQ040) %in% c("Every day", "Some days")  ~ "Current",
      as.character(SMQ040) == "Not at all"                   ~ "Former",
      TRUE                                                   ~ NA_character_   # incl. Refused / Don't know
    ),
    smoking = factor(smoking, levels = c("Never", "Former", "Current")),
    systolic_bp = rowMeans(across(c(BPXOSY1, BPXOSY2, BPXOSY3)), na.rm = TRUE),
    systolic_bp = if_else(is.nan(systolic_bp), NA_real_, systolic_bp)
  ) %>%
  select(SEQN, RIDSTATR, RIDAGEYR, sex, race, smoking, BMXBMI,
         BPXOSY1, BPXOSY2, BPXOSY3, systolic_bp) %>%
  droplevels() %>%
  check("Final analytic data")


cat("\nMissing values per column:\n");  print(colSums(is.na(dashboard_data)))
cat("\nSex:\n");                        print(table(dashboard_data$sex, useNA = "ifany"))
cat("\nRace/ethnicity:\n");             print(table(dashboard_data$race, useNA = "ifany"))
cat("\nSmoking:\n");                    print(table(dashboard_data$smoking, useNA = "ifany"))
cat("\nAge range:", range(dashboard_data$RIDAGEYR), "\n")
cat("Complete cases (BMI, BP, sex, smoking, age):",
    sum(complete.cases(dashboard_data[, c("BMXBMI", "systolic_bp", "sex", "smoking", "RIDAGEYR")])), "\n")

saveRDS(dashboard_data, "data/nhanes_dashboard.Rds")
write_csv(dashboard_data, "data/nhanes_dashboard.csv")
cat("\nSaved data/nhanes_dashboard.csv and .Rds\n")
