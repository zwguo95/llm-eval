set.seed(123)
library(tidyverse)

simulate_llm_presence <- function(n_batches, n_rep = 1000, effect_size = 0.10, icc = 0.05, 
                                  sd_total = 0.1, trust_threshold = 15) {
  n_per_batch <- 10
  n_questions <- 45
  questions_per_difficulty <- 15
  baseline_acc <- c(easy = 0.7, medium = 0.6, hard = 0.5)
  
  sd_total <- 0.15
  sd_between <- sqrt(icc) * sd_total
  sd_within <- sqrt(1 - icc) * sd_total
  
  power <- numeric(n_rep)
  
  for (r in 1:n_rep) {
    df <- data.frame()
    
    for (b in 1:n_batches) {
      for (i in 1:n_per_batch) {
        id <- paste0("b", b, "_p", i)
        group <- ifelse(i == 1, "control", "treatment")
        
        if (group == "control") {
          num_wrong <- 0
          trust <- FALSE
        } else {
          assign_prob <- runif(1)
          if (assign_prob <= 0.15) {
            num_wrong <- 1
          } else if (assign_prob <= 0.30) {
            num_wrong <- 2
          } else {
            num_wrong <- sample(c(0, 3, 6, 9, 12, 15, 21), 1)
          }
          trust <- num_wrong <= trust_threshold
        }
        
        person_effect <- rnorm(1, 0, sd_between)
        acc <- c()
        
        n_wrong_each <- floor(num_wrong / 3)
        wrong_indices <- list(
          easy = if (group == "treatment") sample(1:15, n_wrong_each) else integer(0),
          medium = if (group == "treatment") sample(1:15, n_wrong_each) else integer(0),
          hard = if (group == "treatment") sample(1:15, n_wrong_each) else integer(0)
        )
        
        for (d in names(baseline_acc)) {
          base <- baseline_acc[d]
          for (q in 1:questions_per_difficulty) {
            q_effect <- rnorm(1, 0, sd_within)
            
            if (group == "control") {
              llm_effect <- 0
            } else {
              correct_llm <- !(q %in% wrong_indices[[d]])
              llm_effect <- if (correct_llm && trust) effect_size else 0
            }
            
            acc_q <- base + person_effect + q_effect + llm_effect
            acc <- c(acc, acc_q)
          }
        }
        
        acc_score <- mean(pmin(pmax(acc, 0), 1))
        df <- rbind(df, data.frame(id, group, num_wrong, acc_score))
      }
    }
    
    df$group_bin <- ifelse(df$group == "control", 0, 1)
    if (sum(df$group_bin == 0) >= 2 && sum(df$group_bin == 1) >= 2) {
      p_val <- t.test(acc_score ~ group_bin, data = df)$p.value
      power[r] <- p_val < 0.05
    } else {
      power[r] <- NA
    }
  }
  
  return(mean(power, na.rm = TRUE))
}


effect_sizes <- c(0.05, 0.1)
sd_values <- c(0.15, 0.2)
threshold_values <- c(15, 18)

results <- data.frame()

for (effect_size in effect_sizes) {
  for (sd_total in sd_values) {
    for (trust_threshold in threshold_values) {
      for (b in 2:10) {
        
        pwr <- simulate_llm_presence(
          n_batches = b,
          n_rep = 1000,
          effect_size = effect_size,
          icc = 0.05,
          sd_total = sd_total,
          trust_threshold = trust_threshold
        )
        
        results <- rbind(results, data.frame(
          batches = b,
          effect_size = effect_size,
          sd_total = sd_total,
          trust_threshold = trust_threshold,
          power = pwr
        ))
        
        cat("Effect =", effect_size,
            "SD =", sd_total,
            "Threshold =", trust_threshold,
            "Batches =", b,
            "→ Power =", round(pwr, 3), "\n")
      }
    }
  }
}


ggplot(results, aes(x = batches, y = power, color = factor(sd_total))) +
  geom_line(size = 1) +
  facet_grid(effect_size ~ trust_threshold, labeller = label_both) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "darkred") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Power Curves by Effect Size, SD, and Trust Threshold",
    x = "Number of Batches (n = 10 per batch)",
    y = "Estimated Power",
    color = "SD Total"
  ) +
  theme_minimal(base_size = 13)

ggsave("power_curve_batch.png")
