set.seed(123)
library(tidyverse)
library(parallel)
library(furrr)
library(progressr)
library(foreach)
library(doParallel)

version <- ""
effect_size <- 0.1
sims <- 1000

clamp <- function(p) {
  pmax(0, pmin(1, p))
}

create_and_simulate_design <- function(
    n_per_group,
    n_questions = 45,
    experience_multiplier = 0.01,
    effect_size = 0.1,
    baseline_easy = 0.7,
    baseline_medium = 0.6,
    baseline_hard = 0.5,
    icc = 0.05,
    sims = sims) {
  n_participants <- 2 * n_per_group
  power_results <- numeric(sims)

  power_results <- foreach(sim = 1:sims) %dopar% {
    participant_data <- data.frame(
      participant_id = 1:n_participants,
      gender = sample(c("male", "female"), n_participants, replace = TRUE),
      age_group = sample(c("18-25", "26-35", "36-50", "50+"), n_participants, replace = TRUE),
      experience = rgamma(n_participants, shape = 2, scale = 1),
      group = rep(c("human_only", "human_llm"), each = n_per_group),
      random_effect = rnorm(n_participants, mean = 0, sd = sqrt(icc))
    )

    question_bank <- data.frame(
      question_id = 1:300,
      difficulty = c(rep("easy", 100), rep("medium", 100), rep("hard", 100))
    )

    question_data <- participant_data %>%
      rowwise() %>%
      mutate(
        sampled_questions = list(
          data.frame(
            question_id = c(
              sample(1:100, 15),
              sample(101:200, 15),
              sample(201:300, 15)
            ),
            difficulty = rep(c("easy", "medium", "hard"), each = 15)
          ) %>%
            arrange(sample(1:45)) %>%
            mutate(
              llm_responses = ifelse(
                group == "human_llm",
                rbinom(45, 1, runif(1, 0.5, 1)),
                0
              )
            )
        )
      ) %>%
      unnest(sampled_questions)

    question_data <- question_data %>%
      mutate(
        base_prob = case_when(
          difficulty == "easy" & group == "human_only" ~ clamp(baseline_easy + experience_multiplier * experience + random_effect),
          difficulty == "medium" & group == "human_only" ~ clamp(baseline_medium + experience_multiplier * experience + random_effect),
          difficulty == "hard" & group == "human_only" ~ clamp(baseline_hard + experience_multiplier * experience + random_effect),
          difficulty == "easy" & group == "human_llm" ~ clamp(baseline_easy + experience_multiplier * experience + random_effect + effect_size * llm_responses),
          difficulty == "medium" & group == "human_llm" ~ clamp(baseline_medium + experience_multiplier * experience + random_effect + effect_size * llm_responses),
          difficulty == "hard" & group == "human_llm" ~ clamp(baseline_hard + experience_multiplier * experience + random_effect + effect_size * llm_responses)
        ),
        response = rbinom(n(), 1, prob = base_prob)
      )

    # print(str_glue('Mean base prob: {mean(question_data$base_prob)}'))
    # print(str_glue('SD base prob: {sd(question_data$base_prob)}'))

    question_data <- question_data %>%
      mutate(
        group = factor(group, levels = c("human_only", "human_llm")),
        gender = factor(gender, levels = c("male", "female")),
        age_group = factor(age_group, levels = c("18-25", "26-35", "36-50", "50+"))
      )

    model <- glm(response ~ group + gender + age_group + experience, family = binomial, data = question_data)

    p_value <- coef(summary(model))["grouphuman_llm", "Pr(>|z|)"]
    (p_value < 0.05)
  }

  power_results <- unlist(power_results)
  power <- mean(power_results, na.rm = TRUE)
  cat("Computed power:", power, "for n_per_group =", n_per_group, "and icc=", icc, "and experience multiplier=", experience_multiplier, "\n")

  return(tibble(power = power, group_size = n_per_group, effect_size = effect_size))
}


compute_power_for_group_sizes <- function(group_sizes, sims = 1000, icc = 0.05, experience_multiplier = 0.01, effect_size = 0.1) {
  power_df <- map_dfr(group_sizes, ~ create_and_simulate_design(
    n_per_group = ., sims = sims,
    icc = icc,
    experience_multiplier = experience_multiplier
  ))

  power_df <- power_df %>%
    mutate(
      icc = icc,
      experience_multplier = experience_multiplier,
      sims = sims
    )
  return(power_df)
}

icc_multiplier_sizes <- c(0.005, 0.01, seq(0.05, 0.4, by = 0.1))
icc_multiplier_sizes
group_sizes <- c(25, seq(50, 300, by = 50))
group_sizes

power_results_sims <- map_dfr(
  c(100, 200, 500, 1000),
  ~ compute_power_for_group_sizes(group_sizes,
    sims = .,
    effect_size = effect_size
  )
)

power_results_sims %>%
  write_csv(str_glue("power_results_sims{version}.csv"))

ggplot(power_results_sims, aes(
  x = group_size,
  y = power, color = factor(sims)
)) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0.8, linetype = "dashed",
    color = "gray"
  ) +
  labs(
    x = "Number of Participants per Arm", y = "Power",
    color = "Simulation #",
    title = str_glue("Power analysis results (Effect size={effect_size}, icc={unique(power_results_sims$icc)}, experience multiplier={unique(power_results_sims$experience_multplier)})")
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_brewer(palette = "Blues") +
  theme_minimal()
ggsave(str_glue("power_curve_sims{version}.png"), width = 10, height = 6)
