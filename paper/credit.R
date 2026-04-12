# generate CREDIT
library(tidyverse)
library(stringr)
library(glue)
library(here)

# 1. Read data
df <- read_csv(here("paper","Peekbank Development informal CREDIT spreadsheet - Sheet1.csv"))

# 2. Function: full name → initials
get_initials <- function(name) {
  name %>%
    str_split(" ") %>%
    unlist() %>%
    str_replace_all("\\.", "") %>%
    str_sub(1, 1) %>%
    toupper() %>%
    paste0(collapse = "")
}

# 3. Add initials and preserve authorship order
df <- df %>%
  mutate(
    initials = map_chr(Name, get_initials),
    author_order = row_number()
  )

# 4. CRediT role columns (from your file)
role_cols <- c(
  "Conceptualization",
  "Data curation",
  "Formal analysis",
  "Funding acquisition",
  "Investigation",
  "Methodology",
  "Project administration",
  "Software",
  "Resources",
  "Supervision",
  "Validation",
  "Visualization",
  "Writing – original draft",
  "Writing – review & editing"
)

# 5. Pivot longer + filter contributions
df_long <- df %>%
  select(initials, author_order, all_of(role_cols)) %>%
  pivot_longer(
    cols = all_of(role_cols),
    names_to = "role",
    values_to = "contribution"
  ) %>%
  filter(!is.na(contribution) & contribution == "X") %>%
  mutate(
    role = str_to_lower(role)  # ← change 1: lowercase roles
  )

# 6. Oxford comma helper
collapse_roles <- function(x) {
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

# 7. Build statements in authorship order
credit_statements <- df_long %>%
  group_by(initials, author_order) %>%
  summarise(
    roles = collapse_roles(role),
    .groups = "drop"
  ) %>%
  arrange(author_order) %>%   # ← change 2: enforce author order
  mutate(
    statement = glue("{initials} contributed to {roles}.")
  )

# 8. Combine into single paragraph
final_paragraph <- paste(credit_statements$statement, collapse = " ")

# Output
final_paragraph