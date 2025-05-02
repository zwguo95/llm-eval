library(tidyverse)

clamp <- function(p) { pmax(0, pmin(1, p)) }

create_and_simulate_design <- function(
    agency_data,
    n_agencies,
    baseline_easy = 0.7,
    baseline_medium = 0.5,
    baseline_hard = 0.3,
    effect_size = 0.1,
    icc = 0.1,
    sims = 500
) {
  power_results <- replicate(sims, {
    # Assign agencies to treatment groups using greedy balancing
    sampled_agencies <- agency_data %>% 
      sample_n(n_agencies) %>%
      arrange(desc(Case_Workers))
    
    group1 <- c()
    group2 <- c()
    total_group1 <- 0
    total_group2 <- 0
    
    for (i in 1:nrow(sampled_agencies)) {
      if (total_group1 <= total_group2) {
        group1 <- c(group1, sampled_agencies$agency_id[i])
        total_group1 <- total_group1 + sampled_agencies$Case_Workers[i]
      } else {
        group2 <- c(group2, sampled_agencies$agency_id[i])
        total_group2 <- total_group2 + sampled_agencies$Case_Workers[i]
      }
    }
    sampled_agencies <- sampled_agencies %>%
      mutate(Z = ifelse(agency_id %in% group1, "human_llm", "human_only"))
    
    # Expand to caseworker level
    caseworker_data <- sampled_agencies %>%
      rowwise() %>%
      mutate(caseworkers = list(1:Case_Workers)) %>%
      unnest(caseworkers) %>%
      rename(caseworker_id = caseworkers) %>%
      mutate(
        individual_experience = rnorm(n(), mean = 0, sd = 0.05),
        person_random_effect = rnorm(1, mean = 0, sd = sqrt(icc)),  # Individual ICC random effect
        Z = Z
      )
    
    # Question bank setup and fixed LLM responses
    question_bank <- data.frame(
      question_id = 1:300,
      difficulty = c(rep("easy", 100), rep("medium", 100), rep("hard", 100))
    )
    
    # Assign questions and fixed LLM responses
    question_assignments <- caseworker_data %>%
      mutate(
        sampled_questions = list(
          data.frame(
            question_id = c(sample(1:100, 15), sample(101:200, 15), sample(201:300, 15)),
            difficulty = rep(c("easy", "medium", "hard"), each = 15)
          )
        )
      ) %>%
      unnest(sampled_questions) %>%
      mutate(
        llm_responses = ifelse(
          Z == "human_llm",
          case_when(
            difficulty == "easy" ~ rbinom(n(), 1, 0.9),   
            difficulty == "medium" ~ rbinom(n(), 1, 0.7), 
            difficulty == "hard" ~ rbinom(n(), 1, 0.5)    
          ),
          0 
        )
        
      )
    
    # Calculate probabilities and generate responses
    question_assignments <- question_assignments %>%
      mutate(
        base_prob = case_when(
          difficulty == "easy" & Z == "human_only" ~ clamp(baseline_easy + 0.01 * individual_experience + person_random_effect),
          difficulty == "medium" & Z == "human_only" ~ clamp(baseline_medium + 0.01 * individual_experience + person_random_effect),
          difficulty == "hard" & Z == "human_only" ~ clamp(baseline_hard + 0.01 * individual_experience + person_random_effect),
          
          difficulty == "easy" & Z == "human_llm" ~ clamp(baseline_easy + 0.01 * individual_experience + effect_size * llm_responses + person_random_effect),
          difficulty == "medium" & Z == "human_llm" ~ clamp(baseline_medium + 0.01 * individual_experience + effect_size * llm_responses + person_random_effect),
          difficulty == "hard" & Z == "human_llm" ~ clamp(baseline_hard + 0.01 * individual_experience + effect_size * llm_responses + person_random_effect)
        ),
        response = rbinom(n(), 1, prob = base_prob)
      )
    
    # Fit model and compute power
    question_assignments <- question_assignments %>%
      mutate(Z = factor(Z, levels = c("human_only", "human_llm")))
    model <- glm(response ~ Z, family = binomial, data = question_assignments)
    coef_names <- names(coef(model))
    
    if ("Zhuman_llm" %in% coef_names) {
      p_value <- coef(summary(model))["Zhuman_llm", "Pr(>|z|)"]
      return(p_value < 0.05)
    }
    return(FALSE)
  })
  
  power <- mean(power_results, na.rm = TRUE)
  cat("Computed power:", power, "\n")
  return(power)
}

compute_power_for_agencies <- function(
    agency_data, n_agencies_list, baseline_easy, baseline_medium, baseline_hard, effect_size, icc, sims
) {
  results <- lapply(n_agencies_list, function(n_agencies) {
    cat("\nRunning simulation for", n_agencies, "agencies...\n")
    power <- create_and_simulate_design(
      agency_data = agency_data,
      n_agencies = n_agencies,
      baseline_easy = baseline_easy,
      baseline_medium = baseline_medium,
      baseline_hard = baseline_hard,
      effect_size = effect_size,
      icc = icc,
      sims = sims
    )
    return(data.frame(n_agencies = n_agencies, power = power))
  })
  results_df <- do.call(rbind, results)
  return(results_df)
}

agency_data <- data.frame(
  agency_id = 1:42,
  Case_Workers = c(11, 3, 4, 8, 7, 5, 5, 9, 7, 7, 6, 13, 7, 13, 18, 11, 
                   25, 11, 19, 10, 10, 26, 1, 7, 3, 3, 13, 13, 2, 11, 
                   7, 16, 12, 9, 10, 10, 3, 10, 50, 40, 7, 10)
)

results <- compute_power_for_agencies(
  agency_data = agency_data,
  n_agencies_list = 3:20,
  baseline_easy = 0.7,
  baseline_medium = 0.5,
  baseline_hard = 0.3,
  effect_size = 0.1,
  icc = 0.1,
  sims = 500
)

results_30_questions <- results %>%
  mutate(type = "30 Questions")
results_45_questions <- results %>%
  mutate(type = "45 Questions")

results_all <- do.call(rbind, list(results_30_questions,
                                   results_45_questions))

ggplot(results_all, aes(x = n_agencies, y = power, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  labs(
    x = "Total Number of Agencies",
    y = "Estimated Power",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
