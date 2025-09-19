library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(openxlsx)

setwd("/Users/ruohan/Documents/RPTU/WS_24_25/LB_Cognition/VLS/data")

csv_files <- list.files(path = "subject_files", pattern = "subject-\\d+\\.csv", full.names = TRUE)


# -----------  Combine participant data from subject files   --------------------

# get the useful columns from the csv files
data_from_csv <- c("subject_nr",
                   "english",
                   "german",
                   "multipic",
                   "cond",
                   "response",
                   "count_fixdot",
                   "live_row")

data_list <- lapply(csv_files, function(file) {
  df <- read.csv(file)
  df <- df[, data_from_csv, drop = FALSE]
  return(df)
})

vls <- do.call(rbind, data_list)

# add new columns for later
vls[c("RT_Chronset", "reaction_time", "accuracy", "language", "type", "notes")] <- NA


# -----------  Extract reaction time from chronset files  --------------------
chronset_files <- list.files(path = "chronset_files", pattern = "chronset_\\d+\\.txt", , full.names = TRUE)

chronset_data <- do.call(rbind, lapply(chronset_files, function(file) {
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("filename", "RT")
  return(df)
}))

chronset_data <- chronset_data %>%
  mutate(
    subject_nr = as.integer(str_extract(filename, "^[0-9]+")),
    english = str_extract(filename, "(?<=_)[A-Z]+(?=_)"),
    cond = str_extract(filename, "(?<=_)[a-z]+[0-9]*(?=_output)"),
    live_row = as.integer(str_extract(filename, "(?<=_)\\d+(?=_)")),
    cond = ifelse(cond == "mixed", "mixed1", cond),
    response = "space"
  )
# -----------  Combine Chronset RT to main data file  --------------------
vls <- vls %>%
  left_join(
    chronset_data %>% select(subject_nr, english, cond, live_row, response, RT),
    by = c("subject_nr", "english", "cond", "live_row", "response")
  )

vls$RT_Chronset <- vls$RT

# -----------  Rename and rescale the rating blocks  --------------------
vls1 <- vls %>%
  select(-RT) %>%
  arrange(subject_nr) %>%
  mutate(cond = ifelse(response !="space" & is.na(RT_Chronset), "rating", cond),
         response = ifelse(cond == "rating", as.numeric(response) - 6, response))

# -----------  Get the content of oral response  --------------------
get_audio_text <- function(df) {
  audio_text <- df %>%
    mutate(
      subject_nr = as.integer(str_extract(filename, "^[0-9]+")),
      english = str_extract(filename, "(?<=_)[A-Z]+(?=_)"),
      cond = str_extract(filename, "(?<=_)[a-z]+[0-9]*(?=_output)"),
      live_row = as.integer(str_extract(filename, "(?<=_)\\d+(?=_)")),
      cond = ifelse(cond == "mixed", "mixed1", cond),
    )
  return(audio_text)
}

english_file <- read.csv("english_text.csv")
english_file = english_file[,-1]
german_file <- read.csv("german_text.csv")
german_file = german_file[,-1]
EN_text = get_audio_text(english_file)
DE_text = get_audio_text(german_file)

get_audio_text_whisper <- function(df) {
  audio_text <- df %>%
    mutate(
      subject_nr = as.integer(str_extract(File.Name, "^[0-9]+")),
      english = str_extract(File.Name, "(?<=_)[A-Z]+(?=_)"),
      cond = str_extract(File.Name, "(?<=_)[a-z]+[0-9]*(?=_output)"),
      live_row = as.integer(str_extract(File.Name, "(?<=_)\\d+(?=_)")),
      cond = ifelse(cond == "mixed", "mixed1", cond),
      output_text = toupper(gsub("[ .]", "", output_text))
    )
  return(audio_text)
}

whisper = read.csv("transcriptions.csv")
whisper_text = get_audio_text_whisper(whisper)

# -----------  Combine the response text to main data file  --------------------
vls2 <- vls1 %>%
  left_join(
    EN_text %>% select(subject_nr, english, cond, live_row, english_text),
    by = c("subject_nr", "english", "cond", "live_row")
  ) %>%
  left_join(
    DE_text %>% select(subject_nr, english, cond, live_row, german_text),
    by = c("subject_nr", "english", "cond", "live_row")
  ) %>%
  left_join(
    whisper_text %>% select(subject_nr, english, cond, live_row, output_text),
    by = c("subject_nr", "english", "cond", "live_row")
  ) %>%
  mutate(
    english_text = toupper(english_text),
    german_text = toupper(german_text)
  )

# write.csv(vls, "vls_data_001.csv", row.names = FALSE)


# -----------  Get response language and accuracy --------------------
vls2 <- vls2 %>%
  mutate(
    language = case_when(
      cond %in% c("mixed1", "mixed2", "single1", "single2") ~ case_when(
        output_text == german ~ "DE",
        output_text == english ~ "EN",
        german_text == german | english_text == german ~ "DE",
        english_text == english | german_text == english ~ "EN",
        TRUE ~ "check"
      ),
      TRUE ~ NA_character_
    ),
    accuracy = case_when(
      cond %in% c("mixed1", "mixed2", "single1", "single2") ~ case_when(
        output_text %in% c(german, english) ~ 1,
        german_text %in% c(german, english) ~ 1,
        english_text %in% c(german, english) ~ 1,
        TRUE ~ 0
      ),
      TRUE ~ NA_real_
    ),
    response_text = case_when(
      cond %in% c("mixed1", "mixed2", "single1", "single2") ~ case_when(
        output_text == german ~ german,
        output_text == english ~ english,
        german_text == german ~ german,
        english_text == german ~ german,
        english_text == english ~ english,
        german_text == english ~ english,
        TRUE ~ "check"
      ),
      TRUE ~ NA_character_
    )
  )


check_data = function(df) {
  cat("Does each subject have all the rows?\n")
  print(table(df$subject_nr))
  
  cat("\nDo the rows of each condition look good?\n")
  print(table(df$cond))
  
  cat("\nChronset RT needs to checked mannually in", 
      sum(!is.na(df$accuracy) & is.na(df$RT_Chronset)), "rows\n")
  
  cat("\nThe total number of condition rows is:", 
      sum(table(df$cond)[c("mixed1", "mixed2", "single1", "single2")]), 
      "- This should be equal to 5400\n")
  
  cat("\nThe accuracy distribution is:\n")
  print(table(df$accuracy))
  
  cat("\nWith a total number of", sum(table(df$accuracy)), 
      "- This should be equal to 5400\n")
  
  cat("\nThe language distribution is:\n")
  print(table(df$language))
  
  cat("\nWith a total number of", sum(table(df$language)), 
      "- This should be equal to 5400\n")
}

check_data(vls2)
# write.xlsx(vls2, "vls_0616_001.xlsx")


# -------------------     Combine manual RT      --------------------
vls_manual <- read_excel("vls_updated_with_type.xlsx")

vls3 <- vls2 %>%
  left_join(
    vls_manual %>% select(subject_nr, english, cond, live_row, reaction_time, accuracy, language, notes, english_text, german_text),
    by = c("subject_nr", "english", "cond", "live_row")
  )

vls_try <- vls3

vls_try$accuracy <- NA_real_
vls_try$language <- NA_character_
vls_try$response_text <- NA_character_

vls_try <- vls_try %>%
  mutate(
    accuracy = ifelse(!is.na(vls3$accuracy.x) & vls3$accuracy.x == 1, 1,
                      ifelse(!is.na(vls3$accuracy.y) & vls3$accuracy.y == 1, 2, accuracy.x)),
    language = ifelse(!is.na(vls3$accuracy.x) & vls3$accuracy.x == 1, vls3$language.x,
                      ifelse(!is.na(vls3$accuracy.y) & vls3$accuracy.y == 1, vls3$language.y, language)),
    response_text = ifelse(!is.na(vls3$accuracy.x) & vls3$accuracy.x == 1, vls3$response,
                      ifelse(!is.na(vls3$accuracy.y) & vls3$accuracy.y == 1, vls3$output_text, notes.y))
  ) %>%
  select(subject_nr, english, german, cond, live_row, response, RT_Chronset, reaction_time.y, response_text, language, accuracy, type)

check_data(vls_try)
# write.xlsx(vls_try, "vls_try.xlsx")

# The ones with accuracy marked as 0, and 2 are checked manually
# I labelled the ones previously marked as 1 by Yajun here as 2, to help the checking.

# all the oral responses that were not transcribed by the language models are checked manually
# but due to the time limits, I only manually rechecked the response text, accuracy and language in *mixed trials*.

# now, combining them to the main data frame.

vls_manual_rh <- read_excel("vls_try+manual.xlsx")

vls4 <- vls3 %>%
  left_join(
    vls_manual_rh %>% select(subject_nr, english, cond, live_row, manual_text),
    by = c("subject_nr", "english", "cond", "live_row")
  )


vls4 <- vls4 %>%
  mutate(
    response_text = toupper(ifelse(!is.na(manual_text), manual_text, response_text)),
    accuracy = case_when(
      cond %in% c("mixed1", "mixed2", "single1", "single2") ~ case_when(
        response_text %in% c(german, english) ~ 1,
        TRUE ~ 0
      ),
      TRUE ~ NA_real_
    ),
    
    language = case_when(
      cond %in% c("mixed1", "mixed2", "single1", "single2") ~ case_when(
        response_text == toupper(german) ~ "DE",
        response_text == toupper(english) ~ "EN",
        TRUE ~ "NA"
      ),
      TRUE ~ NA_character_
    )
  ) %>%
  select(subject_nr, english, german, cond, live_row, response, RT_Chronset, reaction_time.y, response_text, language, accuracy, type)

check_data(vls4)
# write.xlsx(vls4, "vls0617.xlsx")


# -----------            Get response type        --------------------
vls5 <- vls4 %>%
  arrange(subject_nr, cond, live_row) %>%
  group_by(subject_nr) %>%
  mutate(
    type = case_when(
      cond %in% c("mixed1", "mixed2") & live_row == 0 ~ "unclassifiable",
      cond %in% c("mixed1", "mixed2") & language == "EN" & lag(language) == "EN" ~ "repeat",
      cond %in% c("mixed1", "mixed2") & language == "DE" & lag(language) == "DE" ~ "repeat",
      cond %in% c("mixed1", "mixed2") & language == "EN" & lag(language) == "DE" ~ "switch",
      cond %in% c("mixed1", "mixed2") & language == "DE" & lag(language) == "EN" ~ "switch",
      cond %in% c("mixed1", "mixed2") ~ "unclassifiable",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  arrange(subject_nr, cond, live_row)

# write.xlsx(vls5, "data_with_type.xlsx")


# -----------    Add standard valence scale    --------------------
wordlist = read.xlsx("wordlist vvlsdeen.xlsx")
valence_scale = wordlist[(1:30), c("WORD_LOWER", "EMO_MEAN", "EMO_STD")]
valence_scale <- valence_scale %>%
  mutate(valence = case_when(
    EMO_MEAN < 0 ~ "negative", 
    EMO_MEAN >= 0 & EMO_MEAN < 1 ~ "neutral", 
    EMO_MEAN >= 1 ~ "positive"
  ))
table(valence_scale$valence)

vls6 <- vls5 %>%
  mutate(german = tolower(german)) %>%
  merge(valence_scale, by.x = "german", by.y = "WORD_LOWER", all.x = TRUE) %>%
  select(subject_nr, english, german, valence, cond, live_row, response, EMO_MEAN, RT_Chronset, reaction_time.y, response_text, language, accuracy, type) %>%
  arrange(subject_nr, cond, live_row)

# write.xlsx(vls6, "vvlsdeen_data0617.xlsx")


#-----------  Compare response valence with standard scale  -----------------

## ---------------   prepare Rating data    ----------------------------
rating = (data[,c("subject_nr","german","response","EMO_MEAN")])
rating$response = as.numeric(rating$response)
rating$EMO_MEAN = as.numeric(rating$EMO_MEAN)

rating = rating[!is.na(rating$response), ]

rating <- rating %>%
  group_by(german) %>%
  mutate(mean_response = mean(response)) %>%
  arrange(german, subject_nr)

rating$residual <- rating$response - rating$EMO_MEAN


rating_binned <- cut(valence_scale$EMO_MEAN, breaks = seq(-3, 3, by = 1), right = FALSE)
table(rating_binned)

## -------------------   One-sample t-test    ----------------------------

shapiro.test(rating$residual)
hist(rating$residual)

# -> normality violated but
# the residual plot looks quite symmetric and not really skewed, t test should still be ok I think.

rating_comparison = t.test(rating$residual, alternative = "two.sided", mu = 0)
rating_comparison
# -> No significant difference


# ------------- Compare chronsr RT with manual RT in Mixed blocks ---------------

RT <- vls6 %>%
  select(subject_nr, live_row, cond, RT_Chronset, reaction_time.y, accuracy, type) %>%
  filter(
    cond %in% c("mixed1", "mixed2"),
    accuracy == 1,
    type != "unclassifiable",
    subject_nr != 9,
  ) 

RT = RT[!is.na(RT$reaction_time.y), ]

table(RT$subject_nr, RT$cond)
hist(RT$RT_Chronset)
hist(log(RT$RT_Chronset))
hist(RT$reaction_time.y)
RT_head <- RT %>%
  group_by(subject_nr, cond) %>%
  slice_head(n = 10) # select the first 10 rows per condition per participant

table(RT_head$subject_nr, RT_head$cond)
table(RT_head$live_row)

## ----------------- test for normality ----------------------------

### ----------------- grand normality ----------------------------
qqnorm(RT_head$RT_Chronset)
qqnorm(log(RT_head$RT_Chronset))
qqnorm(RT_head$reaction_time.y)
qqnorm(log(RT_head$reaction_time.y))
# -> normality violated

## -----------------  non-parametric test         ----------------------------

### ----------------- grand comparison     ----------------------------
wilcox.test(RT_head$RT_Chronset, RT_head$reaction_time.y, 
            alternative = "two.sided", 
            paired = TRUE,
            exact = FALSE)
# -> significant difference identified



# -------------------- Final dataframe for mixed blocks  --------------------
# Use manual RT instead of RT_Chronset for all the trials with manual RT available
vls_mixed = vls6 %>%
  mutate(RT = ifelse(!is.na(reaction_time.y), reaction_time.y, RT_Chronset)) %>%
  filter((cond %in% c("mixed1", "mixed2")), subject_nr != 9)

write.xlsx(vls_mixed, "vvlsdeen_mixed_0617.xlsx")

