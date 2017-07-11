#
#
#

# Based on:
# Calculate D1 scores, accuracy and latency summary statistics for the 
# Open Source IRAP (Implicit Relational Assessment Procedure)

# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+



# dependencies ------------------------------------------------------------


# check for all dependencies and install missing ones. 
# solution from https://gist.github.com/stevenworthington/3178163
# auto_install_dependencies <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
# }
# packages <- c("plyr", "dplyr", "tidyr")
# auto_install_dependencies(packages)

library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

# NB Given the shared namespaces between plyr and dplyr (e.g., both 
# contain functions called "rename"), this script specifies which 
# library's function is to be called. Usually, loading plyr before dplyr 
# (as done above) prevents any issues, but this method is safer.


# data acquisition and cleaning -------------------------------------------

# input_csv <- read.csv("~/CCHMC/Projects/IAT_IRAP/Chad/IRAPirap5many.csv")
# processIRAPDataQualtrics(input_csv)

# expects data from qualtrics with PP1,PN1,PP2,PN2,PP3,PN3,TP1,TN1,TP2,TN2,TP3,TN3 columns
processIRAPDataQualtrics <- function(data, 
                         unique_identifier_column="V1",
                         timeout.drop=TRUE, 
                         timeout.ms=10000, 
                         fasttrial.drop=FALSE, 
                         fasttrial.ms=400, 
                         fastprt.drop=TRUE, 
                         fastprt.percent=.10, 
                         fastprt.ms=300) {
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(reshape2)


data_format <- "^([0-9])[T]([0-9]*)([CX])([0-9]*)$"
# \\1 trial type
# \\2 stimuli number
# \\3 correct/incorrect
# \\4 latency
# 

tp_tn_p<-tibble::rownames_to_column(
  tibble::column_to_rownames(data,var=unique_identifier_column),
  var="unique_identifier") %>% 
  filter(unique_identifier!="ResponseID") %>%
  rowwise() %>%
  dplyr::mutate(
    TP1_COUNT=length(unlist(strsplit(gsub(",END","",as.character(TP1)),","))),
    TP2_COUNT=length(unlist(strsplit(gsub(",END","",as.character(TP2)),","))),
    TP3_COUNT=length(unlist(strsplit(gsub(",END","",as.character(TP3)),","))),
    TN1_COUNT=length(unlist(strsplit(gsub(",END","",as.character(TN1)),","))),
    TN2_COUNT=length(unlist(strsplit(gsub(",END","",as.character(TN2)),","))),
    TN3_COUNT=length(unlist(strsplit(gsub(",END","",as.character(TN3)),","))),
    PP1_COUNT=length(unlist(strsplit(gsub(",OK","",gsub(",END","",as.character(PP1))),","))),
    PP2_COUNT=length(unlist(strsplit(gsub(",OK","",gsub(",END","",as.character(PP2))),","))),
    PP3_COUNT=length(unlist(strsplit(gsub(",OK","",gsub(",END","",as.character(PP3))),","))),
    PN1_COUNT=length(unlist(strsplit(gsub(",OK","",gsub(",END","",as.character(PN1))),","))),
    PN2_COUNT=length(unlist(strsplit(gsub(",OK","",gsub(",END","",as.character(PN2))),","))),
    PN3_COUNT=length(unlist(strsplit(gsub(",OK","",gsub(",END","",as.character(PN3))),","))),
    TP=strsplit(gsub(",END","",as.character(paste(TP1,TP2,TP3,sep=","))),","),
    TN=strsplit(gsub(",END","",as.character(paste(TN1,TN2,TN3,sep=","))),","),
    PP=strsplit(gsub(",OK","",gsub(",END","",as.character(paste(PP1,PP2,PP3,sep=",")))),","),
    PN=strsplit(gsub(",OK","",gsub(",END","",as.character(paste(PN1,PN2,PN3,sep=",")))),","),
    TP_COUNT=length(TP),
    TN_COUNT=length(TN),
    PP_COUNT=length(PP),
    PN_COUNT=length(PN)
  ) %>% 
  ungroup() %>%
  select(unique_identifier,TP,TN,PP,PN,
         TP1_COUNT,TP2_COUNT,TP3_COUNT,TN1_COUNT,TN2_COUNT,TN3_COUNT,
         PP1_COUNT,PP2_COUNT,PP3_COUNT,PN1_COUNT,PN2_COUNT,PN3_COUNT,
         TP_COUNT,TN_COUNT,PP_COUNT,PN_COUNT)

# infer block count
block_trial_count <- ifelse(median(tp_tn_p$PP1_COUNT)==0,max(tp_tn_p$PP1_COUNT),median(tp_tn_p$PP1_COUNT))

# validate the counts -- exclude those with non-conformant block counts




tp_tn <- tibble::column_to_rownames(tp_tn_p,var="unique_identifier")
rt_a_full<-t(as.data.frame(tp_tn$TP)) # one column per response
rt_b_full<-t(as.data.frame(tp_tn$TN)) # one column per response
rt_pp_full<-t(as.data.frame(tp_tn$PP)) # one column per response
rt_pn_full<-t(as.data.frame(tp_tn$PN)) # one column per response
rownames(rt_a_full) <- rownames(tp_tn) # name each row with participant id
rownames(rt_b_full) <- rownames(tp_tn) # name each row with participant id
rownames(rt_pp_full) <- rownames(tp_tn) # name each row with participant id
rownames(rt_pn_full) <- rownames(tp_tn) # name each row with participant id

responses_a<-melt(rt_a_full) # long format # 
responses_b<-melt(rt_b_full) # long format # 
responses_pp<-melt(rt_pp_full) # long format # 
responses_pn<-melt(rt_pn_full) # long format # 

colnames(responses_a) <- c("unique_identifier", "trial_order", "response_a")
colnames(responses_b) <- c("unique_identifier", "trial_order", "response_b")
colnames(responses_pp) <- c("unique_identifier", "trial_order", "response_a")
colnames(responses_pn) <- c("unique_identifier", "trial_order", "response_b")

responses_a$response_b<-rep(NA,nrow( responses_a))
responses_pp$response_b<-rep(NA,nrow( responses_pp))

responses_b$response_a<-rep(NA,nrow( responses_b))
responses_pn$response_a<-rep(NA,nrow( responses_pn))

responses_a$test_block_pair<-floor((responses_a$trial_order-1)/block_trial_count)+1
responses_b$test_block_pair<-floor((responses_b$trial_order-1)/block_trial_count)+1
responses_a$practice_block_pair<-rep(NA,nrow( responses_a))
responses_b$practice_block_pair<-rep(NA,nrow( responses_b))

responses_pp$test_block_pair<-rep(NA,nrow( responses_pp))
responses_pn$test_block_pair<-rep(NA,nrow( responses_pn))
responses_pp$practice_block_pair<-floor((responses_pp$trial_order-1)/block_trial_count)+1
responses_pn$practice_block_pair<-floor((responses_pn$trial_order-1)/block_trial_count)+1

input_df <- rbind(responses_a,responses_b,
                  responses_pp,responses_pn) 

cleaned_df <-input_df %>% 
  dplyr::mutate(
    rt_a=as.integer(gsub(data_format,"\\4", response_a)),
    rt_b=as.integer(gsub(data_format,"\\4", response_b)),
    trial_type_a=as.integer(gsub(data_format,"\\1", response_a)),
    trial_type_b=as.integer(gsub(data_format,"\\1", response_b)),
    accuracy_a=ifelse(gsub(data_format,"\\3", response_a)=="C",1,0),
    accuracy_b=ifelse(gsub(data_format,"\\3", response_b)=="C",1,0)
  ) %>%
  rowwise() %>%
  dplyr::mutate(
    rt = sum(rt_a, rt_b, na.rm=TRUE),
    trial_type=sum(trial_type_a,trial_type_b,na.rm=TRUE),
    accuracy = sum(accuracy_a, accuracy_b, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  select(
    unique_identifier,
    rt_a,
    rt_b,
    rt,
    trial_type,
    test_block_pair,
    practice_block_pair,
    trial_order,
    accuracy_a,
    accuracy_b,
    accuracy
  )



# demographics and test parameters  ---------------------------------------


# Select variables of interest
demographics_df <-
  cleaned_df %>%
  select(unique_identifier
         ) %>%
  distinct(unique_identifier, .keep_all = TRUE)  
  # NB other routines use a group_by() at the top, but this throws an error in full_join() for some reason. 
  # Rather than bugtest, I've employed the workaround of a distinct() call at the end of the routine instead.

# Calculate total number of practice block pairs completed per participant
n_pairs_practice_blocks_df <-
  cleaned_df %>%
  group_by(unique_identifier) %>%
  dplyr::summarize(n_pairs_practice_blocks = max(practice_block_pair, na.rm = TRUE))


 # D1 scores and mean latency ----------------------------------------------

filtered_cleaned_df <-
  cleaned_df %>%
  filter((timeout.drop==FALSE | rt <= timeout.ms) &
         (fasttrial.drop==FALSE | rt >= fasttrial.ms) &
         (!is.na(test_block_pair))
         ) # test blocks only

# mean rt
mean_rt_df <-  
  filtered_cleaned_df %>%
  group_by(unique_identifier) %>%
  dplyr::summarize(rt_mean = round(mean(rt, na.rm = TRUE), 3)*1000) %>%
  select(unique_identifier, 
         rt_mean) %>%
  ungroup()

# D1 calculated from all test block rts
D1_df <-  
  filtered_cleaned_df %>%
  group_by(unique_identifier,
           test_block_pair) %>%
  dplyr::summarize(rt_a_mean = mean(rt_a, na.rm = TRUE),
                   rt_b_mean = mean(rt_b, na.rm = TRUE),
                   rt_sd = sd(rt)) %>%
  dplyr::mutate(diff = rt_b_mean - rt_a_mean, # this is effectively a rowwise() calculation as we have group_by() participant and then summarize()'d. rowwise() not included for brevity.
                D1 = round(diff / rt_sd, 3)) %>% 
  ungroup() %>%
  group_by(unique_identifier) %>%
  dplyr::summarize(D1 = round(mean(D1), 3)) %>%
  select(unique_identifier, 
         D1) %>%
  ungroup()

# D1 calculated for each of the four trial-types from all test block rts
D1_by_tt_df <-  
  filtered_cleaned_df %>%
  group_by(unique_identifier,
           test_block_pair,
           trial_type) %>%
  dplyr::summarize(rt_a_mean = mean(rt_a, na.rm = TRUE),
                   rt_b_mean = mean(rt_b, na.rm = TRUE),
                   rt_sd = sd(rt)) %>%
  dplyr::mutate(diff = rt_b_mean - rt_a_mean,
                D1_by_tt = round(diff / rt_sd, 3)) %>%
  ungroup() %>%
  group_by(unique_identifier,
           trial_type) %>%
  select(unique_identifier, 
         trial_type,
         D1_by_tt) %>%
  dplyr::summarize(D1_by_tt = round(mean(D1_by_tt), 3)) %>%
  spread(trial_type, D1_by_tt) %>%
  dplyr::rename(D1_trial_type_1 = `1`,
                D1_trial_type_2 = `2`,
                D1_trial_type_3 = `3`,
                D1_trial_type_4 = `4`)
         
# D1 for ODD trials by order of presentation (for split half reliability) calculated from all test block rts
# NB internal consistency can be calculated by a spearman brown correlation or cronbach's alpha between odd and even D1 scores. Pearson's R is less appropriate.
D1_odd_df <-  
  filtered_cleaned_df %>%
  group_by(unique_identifier,
           test_block_pair) %>%
  filter(trial_order %% 2 == 0) %>%  # odd trials only, nb count starts at 0
  dplyr::summarize(rt_a_mean = mean(rt_a, na.rm = TRUE),
                   rt_b_mean = mean(rt_b, na.rm = TRUE),
                   rt_sd = sd(rt)) %>%
  dplyr::mutate(diff = rt_b_mean - rt_a_mean, # this is effectively a rowwise() calculation as we have group_by() participant and then summarize()'d. rowwise() not included for brevity.
                D1_odd = round(diff / rt_sd, 3)) %>% 
  ungroup() %>%
  group_by(unique_identifier) %>%
  dplyr::summarize(D1_odd = round(mean(D1_odd), 3)) %>%
  select(unique_identifier, 
         D1_odd) %>%
  ungroup()

# D1 for EVEN trials by order of presentation (for split half reliability) calculated from all test block rts
D1_even_df <-  
  filtered_cleaned_df %>%
  group_by(unique_identifier,
           test_block_pair) %>%
  filter(trial_order %% 2 == 1) %>%  # even trials only, nb count starts at 0
  dplyr::summarize(rt_a_mean = mean(rt_a, na.rm = TRUE),
                   rt_b_mean = mean(rt_b, na.rm = TRUE),
                   rt_sd = sd(rt)) %>%
  dplyr::mutate(diff = rt_b_mean - rt_a_mean, # this is effectively a rowwise() calculation as we have group_by() participant and then summarize()'d. rowwise() not included for brevity.
                D1_even = round(diff / rt_sd, 3)) %>% 
  ungroup() %>%
  group_by(unique_identifier) %>%
  dplyr::summarize(D1_even = round(mean(D1_even), 3)) %>%
  select(unique_identifier, 
         D1_even) %>%
  ungroup()


# percentage accuracy and percentage fast trials --------------------------


# add new column that records if RT < 300ms.
# exclusions based on fast trials (>10% trials <300ms) is part of the D1 algorithm
cleaned_df$too_fast_trial <- ifelse(cleaned_df$rt < fastprt.ms, 1, 0) 

# calculate % acc and % fast trials from test block data
percentage_accuracy_and_fast_trials_df <- 
  cleaned_df %>%
  group_by(unique_identifier) %>%
  filter(!is.na(test_block_pair)) %>%  # test blocks only
  dplyr::summarize(percentage_accuracy = round(sum(accuracy)/n(), 3),
                   percent_fast_trials = sum(too_fast_trial)/n()) %>%  # arbitrary number of test block trials
  dplyr::mutate(exclude_based_on_fast_trials = ifelse(fastprt.drop==TRUE & percent_fast_trials>=fastprt.percent, TRUE, FALSE)) %>%  
  select(unique_identifier,
         percentage_accuracy,
         percent_fast_trials,
         exclude_based_on_fast_trials)


# join data frames & quantify failed practice blocks ----------------------


output_df <- 
  join_all(list(demographics_df,
                n_pairs_practice_blocks_df,
                D1_df,
                D1_by_tt_df,
                D1_odd_df,
                D1_even_df,
                mean_rt_df,
                percentage_accuracy_and_fast_trials_df),
           by = "unique_identifier",
           type = "full") %>%
  rowwise() %>%
  mutate(passed_practice_blocks = ifelse(!is.na(D1), TRUE, FALSE))
 
  # reliability
  # splithalfcorr <- cor(D1, D2, use="pairwise.complete.obs")
  # reliability <- (2*splithalfcorr) / (1 + splithalfcorr)
  # return(list(reliability=reliability, splithalfcorr=splithalfcorr, D.odd = D1_odd_df$D1_odd, D.even = D1_even_df$D1_even))

  # calculate rates of dropping
  num.timeout.removed <- nrow(cleaned_df %>% filter((timeout.drop==TRUE & rt > timeout.ms) & (!is.na(test_block_pair))))
  num.fasttrial.removed <- nrow(cleaned_df %>% filter((fasttrial.drop==TRUE & rt < fasttrial.ms) &(!is.na(test_block_pair))))
  num.trials <- nrow(cleaned_df)
  timeout.rate <- num.timeout.removed / num.trials
  fasttrial.rate <- num.fasttrial.removed / num.trials
  fastprt.count <- sum(output_df$exclude_based_on_fast_trials,na.rm=T)
  fastprt.rate <- sum(output_df$exclude_based_on_fast_trials,na.rm=T) / nrow(output_df)
  error.num.prt = sum(cleaned_df$accuracy) 
  error.rate.prt = sum(cleaned_df$accuracy) / nrow(cleaned_df)

  return(list(
    # skipped=skipped,
    timeout.drop=timeout.drop,
    timeout.ms=timeout.ms,
    num.timeout.removed=num.timeout.removed,
    timeout.rate=timeout.rate,
    fasttrial.drop=fasttrial.drop,
    fasttrial.ms=fasttrial.ms,
    num.fasttrial.removed=num.fasttrial.removed,
    fasttrial.rate=fasttrial.rate,
    fastprt.drop=fastprt.drop,
    fastprt.ms=fastprt.ms,
    fastprt.percent=fastprt.percent,
    fastprt.count=fastprt.count,
    fastprt.rate=fastprt.rate,
    error.num.prt=error.num.prt,
    error.rate.prt=error.rate.prt,
    output_df=output_df
  ))
}


