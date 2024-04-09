# README -----------------------------------------------------------------------

#   file:    functions.R
#   purpose: create functions for dashboard and helper files

# ------------------------------------------------------------------------------

# packages ---------------------------------------------------------------------

  library(dplyr)
  library(httr)
  library(lubridate)
  library(blockrand)


# REDCap -----------------------------------------------------------------------

  ## get raw data from REDCap

    get_rc <- function(form_id) {
      
      # README ----
      # purpose: pull data from REDCap using csv parameters, API url/token, and form_id
      # inputs:  form_id (ex: 'demographics')
      
      formData = list(
        token = secret_token, 
        content='record',
        action='export',
        format='csv',
        type='flat',
        csvDelimiter='',
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        exportSurveyFields='false',
        exportDataAccessGroups='false',
        returnFormat='csv', 
        'fields[0]' = 'record_id', 
        'forms[0]' = form_id
      )
      
      # retrieve response to API request
      response <- POST(
        url = secret_url, 
        body = formData, 
        encode = 'form'
      )
      
      # obtain result from response
      result <- content(response)
      
      # output
      return(result)
      
    }
    
  
# treatment assignment ---------------------------------------------------------
    
  ## sequential blocked randomization
    
    custom_blockrand <- function(seed, N, stage1_block_sizes, stage2_block_sizes){
      
      set.seed(seed)
      
      first_stage <- blockrand(
        n = N, 
        levels = c('IV Methylprednisolone 30mg BID', 'Upadacitinib 30mg BID', 'IV Methylprednisolone 30mg BID + Upadacitinib 45mg daily'), 
        num.levels = 3, 
        block.sizes = stage1_block_sizes
      ) %>% rename(
        treatment_one = treatment
      )
      
      output <- data.frame()
      
      for(temp_treatment in c('IV Methylprednisolone 30mg BID', 'Upadacitinib 30mg BID', 'IV Methylprednisolone 30mg BID + Upadacitinib 45mg daily')){
        
        if(temp_treatment == 'IV Methylprednisolone 30mg BID'){
          treatment_two_labels = c('Add Cyclosporine Rescue', 'Add Upadacitinib 30mg BID Rescue')
        } else if(temp_treatment == 'Upadacitinib 30mg BID'){
          treatment_two_labels = c('Switch to IV Methylprednisolone 30mg BID + Cyclosporine', 'Add IV Methylprednisolone 30mg BID')
        } else if(temp_treatment == 'Methylpred + Upa'){
          treatment_two_labels = c('Escalate to IV Methylprednisolone 30mg BID + Upadacitinib 30mg BID', 'Switch to Cyclosporine Rescue')
        }
        
        temp_subset <- first_stage %>% 
          filter(treatment_one == temp_treatment)
        
        temp_N <- nrow(temp_subset)
        temp_subset$id2 <- 1:temp_N
        
        temp_second_stage <- blockrand(
          n = temp_N, 
          levels = treatment_two_labels, 
          num.levels = 2, 
          block.sizes = stage2_block_sizes
        ) %>% rename(
          id2 = id, 
          block.size2 = block.size, 
          treatment_two = treatment, 
          block.id2 = block.id
        )
        
        temp_output <- left_join(
          temp_subset, 
          temp_second_stage, 
          by = 'id2'
        )
        
        output <- rbind(output, temp_output)
        
      }
      
      output %>% 
        select(-c(block.size, block.id, block.size2, block.id2, id2)) %>%
        arrange(id) %>%
        mutate(
          id = case_when(
            nchar(id) == 1 ~ paste0('E00', id), 
            nchar(id) == 2 ~ paste0('E0', id), 
            nchar(id) == 3 ~ paste0('E', id), 
            TRUE ~ 'error'
          )
        )
      
    }
    
  ## retrieve treatment based on user input
    
    get_treatment <- function(id, stage){
      
      if(stage == 'Stage 1'){
        treat_stage = 'treatment_one'
      } else {
        treat_stage = 'treatment_two'
      }
      
      treatment <- treatments[treatments$id == id, c('id', treat_stage)]
      
      return(treatment)
      
    }
    
  ## simple randomization (replaced by blockrand)
  
    # stage1 <- function() {
    #   
    #   # README ----
    #   # purpose of function is to assign patient to first stage of treatment
    #   
    #   # treatment stage 1 options
    #   treatments <- c(
    #     'IV Methylprednisolone 30mg BID', 
    #     'Upadacitinib 30mg BID', 
    #     'IV Methylprednisolone 30mg BID + Upadacitinib 45mg daily'
    #   )
    #   
    #   # random assignment
    #   sample(treatments, 1)
    #   
    # }
    # 
    # stage2 <- function(treat1) {
    #   
    #   # README ----
    #   # purpose of function is to assign patient to second stage of treatment, 
    #   # given first treatment as input
    #   
    #   # treatment stage 2 options (dependent on first stage)
    #   treatments <- c()
    #   
    #   if(treat1 == 'IV Methylprednisolone 30mg BID') {
    #     treatments <- c(
    #       'Add Cyclosporine Rescue', 
    #       'Add Upadacitinib 30mg BID Rescue'
    #     )
    #   } else if (treat1 == 'Upadacitinib 30mg BID') {
    #     treatments <- c(
    #       'Switch to IV Methylprednisolone 30mg BID + Cyclosporine', 
    #       'Add IV Methylprednisolone 30mg BID'
    #     )
    #   } else {
    #     treatments <- c(
    #       'Escalate to IV Methylprednisolone 30mg BID + Upadacitinib 30mg BID', 
    #       'Switch to Cyclosporine Rescue'
    #     )
    #   }
    #   
    #   # random assignment
    #   sample(treatments, 1)
    #   
    # }
    
    # assign_patients <- function(cohort_size) {
    #   
    #   # README ----
    #   # purpose of function is to generate treatment dataset (including stages 1 and 2) for n patients
    #   
    #   # UPDATE ----
    #   # deprecated as of change to treatment assignment tab
    #   
    #   data <- data.frame(patient_id = 1:cohort_size) %>%
    #     rowwise() %>%
    #     mutate(
    #       treatment_one = stage1(), 
    #       treatment_two = stage2(treatment_one)
    #     )
    #   
    # }
    

# assessment -------------------------------------------------------------------
    
  ## retrieve assessment times based off of medication start
  
    calculate_assessment_times <- function(start_times) {
      
      # purpose: calculate assessment times based on medication start
      # inputs: dataframe including patient id (patient_id), medication start time (start_time)
      
      current_time = Sys.time()
      assessment_times <- start_times
      
      for(i in c(1:6, 30, 60, 90)){
        
        day_varname <- paste0('day', i)
        difftime_varname <- paste0('time_until_', day_varname)
        
        assessment_times[, day_varname] <- assessment_times$start_time + i * (24 * 60 * 60)
        assessment_times[, difftime_varname] <- difftime(assessment_times[[day_varname]], current_time, units = 'hours')
        
        assessment_times <- data.frame(assessment_times)
        
      }
      
      return(assessment_times)
      
    }
    

# symptoms ---------------------------------------------------------------------
    
  ## plot CRP
  
    symptom_plot <- function(id){
      
      plot1 <- test_snr %>%
        filter(stage == 1) %>%
        filter(PatientID == id) %>%
        pivot_longer(cols = contains('day'), names_to = 'time', values_to = 'CRP') %>%
        ggplot(aes(x = time, y = CRP, group = PatientID, color = PatientID)) + 
        geom_line() + 
        geom_point() + 
        theme_bw() + 
        theme(
          legend.position = 'bottom'
        )
      
      plot2 <- test_snr %>%
        filter(stage == 2) %>%
        filter(PatientID == id) %>%
        pivot_longer(cols = contains('day'), names_to = 'time', values_to = 'CRP') %>%
        ggplot(aes(x = time, y = CRP, group = PatientID, color = PatientID)) + 
        geom_line() + 
        geom_point() + 
        theme_bw() + 
        theme(
          legend.position = 'bottom'
        )
      
      ggplotly(subplot(plot1, plot2, nrows = 1))
      
    }
    