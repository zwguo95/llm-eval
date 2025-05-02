library(openai)
library(tidyverse)
openai_api_key <- Sys.getenv("OPENAI_API_KEY")
if (openai_api_key == "") stop("API key not found. Please set OPENAI_API_KEY in your .Renviron file.")

df <- readxl::read_excel("snap_qc_errors.xlsx")

generate_preamble <- function(employment, age, gender, citizenship, disability, program) {
  return(sprintf(
    "Employment: %s\nAge: %s\nGender: %s\nCitizenship: %s\nHealthcare/Disability Status: %s\nProgram: %s",
    employment, age, gender, citizenship, disability, program
  ))
}

generate_questions <- function(error, detail, model = "gpt-4o-mini") {
  message(sprintf("Processing Error: '%s' and Detail: '%s'", error, detail))
  
  # Few-shot examples with strict formatting
  examples <- "
Example 1:
Error: Student status
Detail: Ineligible persons included

Question 1: A client is a full-time college student and cares for a 3-year-old child while attending school. Is the client eligible to be included in the household? [Select one correct answer.]
A. No – full-time students are automatically ineligible
B. Yes – she is exempt from the student rule because she cares for a child under 6
C. No – she is eligible only if she is employed
D. Yes – all college students qualify if they have dependents

Question 2: A client’s 19-year-old dependent lives at home, is unemployed, and attends online classes full-time at a university. Is the dependent an eligible household member? [Select one correct answer.]

A. Yes – they are unemployed and live at home
B. Yes – they are taking classes online and not attending in person
C. No – they are considered an ineligible student
D. Yes – dependents under 22 are always eligible

Question 3: A client is a full-time college student and works 15 hours per week at a part-time job. Does this client qualify for benefits? [Select one correct answer.]

A. Yes – they meet the work requirement for students
B. No – full-time students are always ineligible
C. Yes – but only if their income is below the threshold
D. No – students must work at least 20 hours per week to qualify

Question 4: A client indicates they are enrolled in a part-time program but also works full-time. How does this affect their eligibility as a student? [Select one correct answer.]
 
 A. They are eligible as long as they take at least one class
 B. They are not considered a full-time student and may not qualify for certain benefits
 C. Their work status does not affect their student eligibility
 D. They must provide proof of their work hours to qualify
 E. They can only be considered a student if they have a scholarship

Question 5: A client is a full-time graduate student living off-campus and financially independent. They do not work and are not disabled. Do they qualify as an eligible household member? [Select one correct answer.]

A. Yes – all graduate students qualify regardless of their employment status
B. No – full-time students without an exemption do not qualify
C. Yes – as long as they live off-campus and are financially independent
D. No – only undergraduate students have eligibility restrictions

"
  
  # Construct the prompt with formatting instructions
  prompt <- sprintf(
    "You are a test designer tasked with creating five single-choice questions to train caseworkers who process applications. 
    Follow these guidelines:
    - Frame each question as a real-world scenario where an client seeks assistance.
    - Use neutral language, excluding any demographic or program-specific terms.
    - Ensure each question has only one correct answer.
    - End each question with '[Select one correct answer.]

    Here are example questions to guide the style and structure:
    '

%s

Now, generate five single-choice questions based on:
Error: %s
Detail: %s",
    examples, error, detail
  )
  
  response <- tryCatch({
    create_chat_completion(
      model = model,
      temperature = 0,
      messages = list(
        list(role = "system", content = "You are a helpful assistant trained to generate multiple-choice questions."),
        list(role = "user", content = prompt)
      )
    )
  }, error = function(e) {
    message(sprintf("Error during LLM query for Error '%s' and Detail '%s': %s", error, detail, e$message))
    return(rep(NA_character_, 5))
  })
  
  # Validate and extract the LLM response
  if (!is.null(response) && is.list(response) && !is.null(response$choices) && length(response$choices) > 0) {
    content <- response$choices$message.content
    if (!is.null(content)) {
      questions <- unlist(strsplit(content, "\n\n(?=Question \\d+:)", perl = TRUE))  
      questions <- trimws(gsub("Question \\d+:\\s*", "", questions))  
      questions <- questions[questions != ""]  
      return(questions)  
    }
  }
  return(rep(NA_character_, 5))
}

results <- df %>%
  rowwise() %>%
  mutate(
    Preamble = generate_preamble(Employment, Age, Gender, Citizenship, Disability, Program),
    Questions = list(generate_questions(Error, Detail))
  ) %>%
  unnest(Questions) %>%  
  rename(Question = Questions) %>%
  filter(!is.na(Question)) %>%
  mutate(Question = str_trim(Question)) %>% 
  select(Question, Preamble, Error, Detail) %>%
  ungroup()
