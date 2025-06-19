# Load required library
library(dplyr)
# ANSI color codes
green <- "\033[32m"
red <- "\033[31m"
reset <- "\033[0m"

cat_color <- function(txt, TF = TRUE) {
  if(TF) cat(paste0(green, txt)) else cat(paste0(red, txt))
}

check_answers_1 <- function() {
    if (!is.null(colnames(df)) && sum(colnames(df) %in% c("mpg", "cyl", "disp")) == 3) {
      cat_color("Data byla správně načtena.\n")
    } else {
      cat_color("Data nejsou ve správném formátu. Zkontrolujte proměnnou 'data'.", FALSE)
    }
}

check_answers_2 <- function() {
    TF <- !is.null(n_rows) && !is.null(max_hp) && !is.null(med_cyl)
    if(!TF) return(cat_color("Hodnoty nebyly přiřazeny do správných proměnných.", FALSE))
    TF <- n_rows == nrow(df) && round(max_hp,0) == round(max(df$hp),0) && round(med_cyl,0) == round(median(df$cyl))
    if (TF) {
        return(cat_color("Vše přiřazeno správně.\n")) 
    } else return(cat_color("Někde se stala chyba.", FALSE))
}

check_answers_3 <- function() {
    TF <- max_mpg == max(df[df$cyl==6, "mpg"]) & ncol(df_fil_sel_2) == 3 & nrow(df_fil_sel_2) == 7 & nrow(df_fil_sel_3) == 4
    if(TF) cat_color("Všechny testy v pořádku.") else cat_color("Někde je chyba.", FALSE)
}

check_answers_4 <- function() {
    if(is.null(cars_converted)) return(cat_color("Nebyl správně vytvořen dataset: cars_converted"), FALSE)
    if(!"stop_time_round" %in% names(cars_converted)) return(cat_color("Data neobsahují sloupec: stop_time_round."), FALSE)
    
    x <- cars_converted[10,]
    if(x$stop_time_s != 2 * x$dist_m / x$speed_mps) {
        cat_color("Někde ve výpočtu je chyba.", FALSE) 
    } else cat_color("Pravděpodobně vše v pořádku")
}

check_answers_5 <- function() {
  
  # Add computed columns
  df <- mtcars %>%
    mutate(
      model = rownames(mtcars),
      weight_kg = wt * 1000 * 0.453592,       # Convert weight from 1000 lbs to kg
      l_per_100km = 235.2 / mpg,              # Convert mpg to liters/100km
      power_per_ton = hp / (weight_kg / 1000) # Calculate power per ton
    )

  # Get correct answers
  correct_q1 <- df %>% filter(weight_kg == max(weight_kg)) %>% pull(model)
  correct_q2 <- df %>% filter(l_per_100km == max(l_per_100km)) %>% pull(model)
  correct_q3 <- df %>% filter(hp == max(hp)) %>% pull(model)

  lightest <- df %>% filter(weight_kg == min(weight_kg))
  correct_q4 <- ifelse(lightest$l_per_100km == min(df$l_per_100km), "ano", "ne")

  # Normalize user answers (convert to lowercase and trim spaces)
  user_q1 <- tolower(trimws(q1_model_max_weight))
  user_q2 <- tolower(trimws(q2_model_max_consumption))
  user_q3 <- tolower(trimws(q3_model_max_hp))
  user_q4 <- tolower(trimws(q4_yes_no))

  # Normalize correct answers
  correct_q1 <- tolower(correct_q1)
  correct_q2 <- tolower(correct_q2)
  correct_q3 <- tolower(correct_q3)

  # Output
  cat("Vyhodnocení:\n\n")

  cat("1. Model s nejvyšší hmotností: ",
      if (user_q1 %in% correct_q1) paste0(green, "OK", reset, "\n")
      else paste0(red, "chyba", reset, "\n"))

  cat("2. Nejvyšší spotřeba na tunu: ",
      if (user_q2 %in% correct_q2) paste0(green, "OK", reset, "\n")
      else paste0(red, "chyba", reset, "\n"))

  cat("3. Nejvyšší výkon: ",
      if (user_q3 %in% correct_q3) paste0(green, "OK", reset, "\n")
      else paste0(red, "chyba", reset, "\n"))

  cat("4. Má nejlehčí model nejnižší spotřebu? ",
      if (user_q4 == correct_q4) paste0(green, "Odpověď je správná.", reset, "\n")
      else paste0(red, "Odpověď je chybná.", reset, "\n"))
}

check_answers_6 <- function() {
  # Compute correct answers
  group_summary <- df %>%
    group_by(cyl, am) %>%
    summarise(n = n(), mean_mpg = mean(mpg), .groups = "drop")

  am0 <- group_summary %>% filter(am == 0) %>% rename(mpg_am0 = mean_mpg)
  am1 <- group_summary %>% filter(am == 1) %>% rename(mpg_am1 = mean_mpg)
  mpg_compare <- inner_join(am0, am1, by = "cyl") %>%
    mutate(auto_lower = mpg_am0 < mpg_am1)

  correct_q1 <- nrow(group_summary)
  correct_q2 <- max(group_summary$n)
  correct_q3 <- if (all(mpg_compare$auto_lower)) "ano" else "ne"

  # User answers
  user_q1 <- q1_n_groups
  user_q2 <- q2_n_max_groups
  user_q3 <- tolower(trimws(q3_yes_no))

  # Output
  cat("Vyhodnocení:\n\n")

  cat("1. Počet skupin: ",
      if (user_q1 == correct_q1) paste0(green, "OK", reset, "\n")
      else paste0(red, "chyba", reset, " (", user_q1, ")\n"))

  cat("2. Počet modelů v nejpočetnější skupině: ",
      if (user_q2 == correct_q2) paste0(green, "OK", reset, "\n")
      else paste0(red, "chyba", reset, " (", user_q2, ")\n"))

  cat("3. Měly automaty vždy nižší spotřebu při stejném počtu válců? ",
      if (user_q3 == correct_q3) paste0(green, "Odpověď je správná.", reset, "\n")
      else paste0(red, "Odpověď je chybná.", reset, "\n"))
}

      
