# README -----------------------------------------------------------------------
#
#   file:    redcap-data.R
#   purpose: pull and organize data from REDCap
#
# ------------------------------------------------------------------------------


# packages and source files ----------------------------------------------------
  
  library(tidyr)
  library(stringr)
  library(lubridate)
  
  source('secret-data.R')
  source('functions.R')

  
# pull raw data from REDCap ----------------------------------------------------

  # # demo             <- get_rc('demographics')
  eligibility      <- get_rc('eligibility_requirements')
  informed_consent <- get_rc('informed_consent')
  drug_exposure    <- get_rc('drug_exposure_dosing')

  
# preprocessing ----------------------------------------------------------------
  
  ## drop unnecessary columns 
  
    eligibility <- eligibility %>% 
      select(-c(redcap_repeat_instrument, redcap_repeat_instance))
  
    informed_consent <- informed_consent %>% 
      select(-c(redcap_repeat_instrument, redcap_repeat_instance))
    
    drug_exposure <- drug_exposure %>% 
      select(-c(redcap_repeat_instrument, redcap_repeat_instance))
  
  ## filter to relevant redcap event
    
    eligibility <- eligibility %>% 
      filter(redcap_event_name == "prescreeningbaseli_arm_1")
  
    informed_consent <- informed_consent %>% 
      filter(redcap_event_name == "prescreeningbaseli_arm_1")
    
  ## join eligibility and informed consent data 
    
    screening <- inner_join(
      eligibility, informed_consent,
      by = c("record_id", "redcap_event_name")
    )
  
  ## extract patient classes from eligibility data 
  
    enrolled <- screening %>% 
      filter(grepl("^E", record_id))
    
    excluded <- screening %>% 
      filter(grepl("^NE", record_id))
    
    refused <- screening %>% 
      filter(grepl("^R", record_id))
  

# screening + enrollment tab ---------------------------------------------------
  
  ## enrolled + exclusion counts 
  
    n_enrolled = nrow(enrolled)
    n_excluded = nrow(excluded)
    n_refused  = nrow(refused)
      
    n_screened = n_enrolled + n_excluded + n_refused
    
    n_enrolled_percent = round(n_enrolled / n_screened * 100, 2)
    n_excluded_percent = round(n_excluded / n_screened * 100, 2)
    n_refused_percent  = round(n_refused  / n_screened * 100, 2)
      
  ## reasons for study exclusion 
    
    ### EDA: inspect top exclusion reasons (overlapping) 
    
      exclusion_reasons <- screening %>% 
        filter(!grepl("^E", record_id)) %>% 
        mutate(
          inc_true_count = as.numeric(inc_truetemp) + as.numeric(inc_truepulse) + as.numeric(inc_truehgb) + as.numeric(inc_trueesr) + as.numeric(inc_truecrp), 
          inc_true = ifelse(inc_truebowel == 0 | inc_true_count == 0, 0, 1)
        ) %>% 
        select(record_id, inc_age:inc_hospitalized, inc_true, inc_mabs:exc_clinsig)
      
      include_criteria <- colSums(exclusion_reasons %>% select(contains('inc')) == 0, na.rm = TRUE)
      exclude_criteria <- colSums(exclusion_reasons %>% select(contains('exc')) == 1, na.rm = TRUE)
      
      exclude_reason_counts <- c(include_criteria, exclude_criteria) %>% sort(decreasing=TRUE)
      
    ### map exclusion reasons to labels
    
    exclusion_mappings <- exclusion_reasons %>% 
      mutate(
        exclude_label = case_when(
          # INFORMED CONSENT
          grepl("^R", record_id) ~ "IC: Refused Informed Consent", 
          # INCLUSION CRITERIA 
          inc_age == 0 ~ "Inc: Age", 
          inc_diaguc == 0 ~ "Inc: No UC Diagnosis", 
          inc_hospitalized == 0 ~ "Inc: Non-UC Hospitalization", 
          inc_true == 0 ~ "Inc: No ASUC Diagnosis", 
          inc_mabs == 0 ~ "Inc: No Prior Biologics", 
          # inc_complystudyreq == 0 | inc_icf == 0 | inc_complyduration == 0 | inc_oralsiregimen == 0 ~ "IC: Refused Informed Consent", 
            # should be 0 if first case accurately captures refused IC group
          inc_female == 2 ~ "Inc: Reproductive Potential", 
          # EXCLUSION CRITERIA
          exc_colitiscrohn == 1 ~ "Exc: Colitis or Crohn's Disease", 
          exc_ivcortosteroids == 1 ~ "Exc: Recent Prior Corticosteroids", 
          exc_pregbreast == 1 ~ "Exc: Pregnant or Breastfeeding", 
          exc_toxicmegacolon == 1 ~ "Exc: Toxic Megacolon", 
          exc_hypersens == 1 ~ "Exc: Hypersensitivity", 
          exc_expupadacitinib == 1 ~ "Exc: Prior Upadacitinib", 
          exc_posstool == 1 ~ "Exc: Positive Stool Exam", 
          exc_onginfect == 1 ~ "Exc: Ongoing Infection", 
          exc_gt5cmv == 1 ~ "Exc: Active CMV Colitis", 
          exc_investagent == 1 ~ "Exc: Investigational Agent", 
          exc_malignant == 1 ~ "Exc: Current Malignancy", 
          exc_colbowel == 1 ~ "Exc: History of Bowel Surgery", 
          exc_condition == 1 ~ "Exc: Underlying Condition", 
          exc_hepaticpugh == 1 ~ "Exc: Hepatic Impairment", 
          exc_htn == 1 ~ "Exc: Hypertension", 
          exc_cvsthrom == 1 ~ "Exc: Cardiovascular or Thrombotic Condition", 
          exc_tchol == 1 ~ "Exc: Total Cholesterol", 
          exc_allergicsens == 1 ~ "Exc: Treatment Allergy", 
          exc_hpvhcvhiv == 1 ~ "Exc: Viral History", 
          exc_transpant == 1 ~ "Exc: Transplant", 
          exc_herpes == 1 ~ "Exc: Herpes", 
          exc_venthrombo == 1 ~ "Exc: Venous Thrombo Medication", 
          exc_vaccine == 1 ~ "Exc: Recent Vaccination", 
          exc_lympopro == 1 ~ "Exc: Lymphoproliferative Disorder", 
          exc_giperforation == 1 ~ "Exc: GI Perforation", 
          exc_cyp3a4 == 1 ~ "Exc: CYP3A4 Medications", 
          exc_drugabsorb == 1 ~ "Exc: Drug Absorption", 
          exc_clinsig == 1 ~ "Exc: Clinically Significant Condition",
          TRUE ~ "Other"
        )
      ) %>% 
      select(record_id, exclude_label) %>% 
      group_by(exclude_label) %>% 
      summarize(count = n())
    
    top_exclusion_reasons <- exclusion_mappings %>% arrange(desc(count)) %>% head(7)
    top_exclusion_reasons[nrow(top_exclusion_reasons) + 1, ] = list("Other: Other", nrow(exclusion_reasons) - sum(top_exclusion_reasons$count))
    
    top_exclusion_reasons <- top_exclusion_reasons %>% 
      rowwise() %>% 
      mutate(
        reason = unlist(str_split(exclude_label, ": "))[2], 
        reason_class = unlist(str_split(exclude_label, ": "))[1], 
        reason_class = case_when(
          reason_class == "Inc" ~ "Inclusion Criteria", 
          reason_class == "Exc" ~ "Exclusion Criteria", 
          reason_class == "IC" ~ "Informed Consent", 
          reason_class == "Other" ~ "Other"
        ), 
        reason_class = factor(reason_class, levels = c("Inclusion Criteria", "Exclusion Criteria", "Informed Consent", "Other"))
      ) %>% 
      ungroup() %>% 
      select(reason, reason_class, count)
    
  ## screening dates 
    
    screening_dates <- screening %>% 
      select(record_id, eligdat) %>% 
      mutate(
        eligdat = as.Date(eligdat),
        date    = format(eligdat, "%b %y")
      ) %>% 
      arrange(eligdat)
    
    screening_dates$date = factor(screening_dates$date, levels = unique(screening_dates$date), ordered=TRUE)
    
    screened_by_month <- screening_dates %>% 
      group_by(date) %>% 
      summarize(
        Screened = n()
      )
    
    approached_by_month <- screening_dates %>% 
      filter(!grepl("^NE", record_id)) %>% 
      group_by(date) %>% 
      summarize(
        Approached = n()
      )
    
    enrolled_by_month <- screening_dates %>% 
      filter(grepl("^E", record_id)) %>% 
      group_by(date) %>% 
      summarize(
        Enrolled = n()
      )
    
    screening_by_month <- screened_by_month %>% 
      left_join(approached_by_month, by = "date") %>% 
      left_join(enrolled_by_month, by = "date") %>% 
      replace_na(list(Screened = 0, Approached = 0, Enrolled = 0)) %>% 
      pivot_longer(
        cols = c(Screened, Approached, Enrolled), 
        names_to  = "type",  
        values_to = "count"
      ) %>% 
      mutate(type = factor(type, levels = c("Screened", "Approached", "Enrolled")))
    
    
# treatment assignment tab -----------------------------------------------------
    
  ## randomization
    
    # treatments <- custom_blockrand(15213, 124, c(1, 2, 3), c(1, 2))
        # Kelley: always sample 2x to allow for dropouts/extra enrolls
    # write.csv(treatments, 'treatment_data.csv', row.names = FALSE)
    
    treatments <- read.csv("treatment_data.csv")
  
  ## patient counts by treatment assignment 
    
    treatment_distribution <- drug_exposure %>% 
      filter(
        redcap_event_name == "prescreeningbaseli_arm_1" & 
        !is.na(stage1arm)
      ) %>% 
      select(record_id, stage1arm, stage2arm) %>% 
      mutate(
        treatment_one = case_when(
          stage1arm == "a" ~ "IV Methylprednisolone", 
          stage1arm == "d" ~ "Upadacitinib", 
          stage1arm == "g" ~ "IV Methylprednisolone + Upadacitinib", 
          TRUE ~ "error"
        ), 
        treatment_two = case_when(
          is.na(stage2arm) ~ "Not Assigned"
        )
      )
  
    
# assessment tab ---------------------------------------------------------------
  
  ## calculate medication times
    
    randomization_times <- drug_exposure %>% 
      filter(redcap_event_name == "prescreeningbaseli_arm_1") %>% 
      filter(dsyn_rand == 1) %>% 
      select(record_id, dsstdat_rand, dssttim_rand) %>% 
      mutate(
        start_time = as.POSIXct(paste(dsstdat_rand, dssttim_rand), format = "%Y-%m-%d %H:%M:%S")
      )

    assessment_times <- calculate_assessment_times(randomization_times) %>%
      select(
        record_id, start_time, day1, day2, day3, day4, day5, day6, day30, day60, day90,
        time_until_day1, time_until_day2, time_until_day3, time_until_day4, time_until_day5, time_until_day6,
        time_until_day30, time_until_day60, time_until_day90
      )
    
  
# test data (until REDCap is finalized) ----------------------------------------
  
  ## symptoms and response
    
    test_snr <- data.frame(
      PatientID = c(1:50, 1:50), 
      treatment = sample(c('Methylprednisolone', 'Upadacitinib', 'Methylpred + Upa'), 100, replace = TRUE),
      stage = c(rep(1, 50), rep(2, 50)), 
      day0 = rnorm(100, mean = 9, sd = 1), 
      day1 = rnorm(100, mean = 8, sd = 2), 
      day2 = rnorm(100, mean = 7, sd = 2), 
      day3 = rnorm(100, mean = 7, sd = 2), 
      response = sample(c('Y', 'N', 'U'), 100, replace = TRUE)
    ) %>% mutate(
      response = factor(response, levels = c('U', 'N', 'Y'))
    )
    
    test_responseData1 <- data.frame(
      response_status = c('Responder', 'In-Progress', 'Non-Responder'),
      value = c(13, 10, 30)
    ) %>%
      mutate(
        response_status = factor(response_status, levels = c('Non-Responder', 'In-Progress', 'Responder'))
      )
    
    test_responseData2 <- data.frame(
      response_status = c('Responder', 'In-Progress', 'Non-Responder'),
      value = c(4, 18, 8)
    ) %>%
      mutate(
        response_status = factor(response_status, levels = c('Non-Responder', 'In-Progress', 'Responder'))
      )
    
