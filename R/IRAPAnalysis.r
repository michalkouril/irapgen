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

IRAPretrieve <- function(data, unique_identifier, block, block_pair, 
                         trial_block_type, block_pair_num) {

# Add condition if column V1 exists
data<-data[data$V1!='ResponseID',]   
data['unique_identifier']<-data[unique_identifier]
data['block']<-data[block]
id_string_arr <- 
  data[,c('unique_identifier','block')] %>% 
  rowwise() %>%
  dplyr::mutate(block_arr=strsplit(gsub(",OK","",gsub(",END","",as.character(block))),",")) %>%
  ungroup() %>%
  select(unique_identifier,block_arr) %>%
  filter(block_arr!="character(0)")

id_string_df<-as.data.frame(id_string_arr)
rownames(id_string_df) <- id_string_df$unique_identifier

median_len <- median(t(as.data.frame(lapply(id_string_df$block_arr, length))))
t_id_string_df<-t(as.data.frame(lapply(id_string_df$block_arr, function(x) { x[1:median_len]})))

# t_id_string_df<-t(as.data.frame(id_string_df$block_arr))  
rownames(t_id_string_df)<- rownames(id_string_df)
long_id_string_df<-melt(t_id_string_df)
colnames(long_id_string_df) <- c("unique_identifier", "trial_order", "response")
long_id_string_df$block <- rep(block,nrow(long_id_string_df)) 
long_id_string_df$block_pair <- rep(block_pair,nrow(long_id_string_df)) 
long_id_string_df$block_pair_num <- rep(block_pair_num,nrow(long_id_string_df)) 
long_id_string_df$trial_block_type <- rep(trial_block_type,nrow(long_id_string_df)) 
long_id_string_df$practice_block_pair <- rep(ifelse(block_pair=="P",block_pair_num,NA),nrow(long_id_string_df)) 
long_id_string_df$test_block_pair <- rep(ifelse(block_pair=="T",block_pair_num,NA),nrow(long_id_string_df))
long_id_string_df
}

long_data_df<-
rbind(IRAPretrieve(data, "V1", "PP1", "P", "A", 1),
      IRAPretrieve(data, "V1", "PN1", "P", "B", 1),
      IRAPretrieve(data, "V1", "PP2", "P", "A", 2),
      IRAPretrieve(data, "V1", "PN2", "P", "B", 2),
      IRAPretrieve(data, "V1", "PP3", "P", "A", 3),
      IRAPretrieve(data, "V1", "PN3", "P", "B", 3),
      IRAPretrieve(data, "V1", "TP1", "T", "A", 1),
      IRAPretrieve(data, "V1", "TN1", "T", "B", 1),
      IRAPretrieve(data, "V1", "TP2", "T", "A", 2),
      IRAPretrieve(data, "V1", "TN2", "T", "B", 2),
      IRAPretrieve(data, "V1", "TP3", "T", "A", 3),
      IRAPretrieve(data, "V1", "TN3", "T", "B", 3)
)

cleaned_df <-long_data_df %>% 
  dplyr::mutate(
    rt_a=ifelse(trial_block_type=='A',as.integer(gsub(data_format,"\\4", response)),NA),
    rt_b=ifelse(trial_block_type=='B',as.integer(gsub(data_format,"\\4", response)),NA),
    trial_type_a=ifelse(trial_block_type=='A',as.integer(gsub(data_format,"\\1", response)),NA),
    trial_type_b=ifelse(trial_block_type=='B',as.integer(gsub(data_format,"\\1", response)),NA),
    accuracy_a=ifelse(trial_block_type=='A',ifelse(gsub(data_format,"\\3", response)=="C",1,0),NA),
    accuracy_b=ifelse(trial_block_type=='B',ifelse(gsub(data_format,"\\3", response)=="C",1,0),NA),
    stimulus_a=ifelse(trial_block_type=='A',as.integer(gsub(data_format,"\\2", response)),NA),
    stimulus_b=ifelse(trial_block_type=='B',as.integer(gsub(data_format,"\\2", response)),NA)
  ) %>%
  rowwise() %>%
  dplyr::mutate(
    rt = sum(rt_a, rt_b, na.rm=TRUE),
    trial_type=sum(trial_type_a,trial_type_b,na.rm=TRUE),
    accuracy = sum(accuracy_a, accuracy_b, na.rm=TRUE),
    stimulus = sum(stimulus_a, stimulus_b, na.rm=TRUE)
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
    accuracy,
    stimulus_a,
    stimulus_b,
    stimulus,
    block_pair,
    trial_block_type,
    block_pair_num,
    response
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

block_accuracy_df <-
  cleaned_df[,c('unique_identifier','block_pair','block_pair_num','trial_block_type','rt', 'accuracy')] %>% 
  dplyr::mutate(block=paste0(block_pair,block_pair_num,trial_block_type,"_accuracy")) %>%
  group_by(unique_identifier,block) %>% 
  dplyr::summarise(accuracy_mean=mean(accuracy)) %>% 
  spread(block,accuracy_mean)

block_rtmed_df <-
  cleaned_df[,c('unique_identifier','block_pair','block_pair_num','trial_block_type','rt', 'accuracy')] %>% 
  dplyr::mutate(block=paste0(block_pair,block_pair_num,trial_block_type,"_rtmed")) %>%
  group_by(unique_identifier,block) %>% 
  dplyr::summarise(rt_median=mean(rt)) %>% 
  spread(block,rt_median)


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


if (nrow(filtered_cleaned_df)>0) {
  
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
} else {
  D1_by_tt_df <- data.frame(unique_identifier=1,D1_trial_type_1=1,D1_trial_type_2=2,
                            D1_trial_type_3=3,D1_trial_type_4=4)
}

if (nrow(filtered_cleaned_df)>0) {
  
# D1 calculated for each stimuli from all test block rts
D1_by_stim_df <-  
  filtered_cleaned_df %>%
  group_by(unique_identifier,
           test_block_pair,
           stimulus) %>%
  dplyr::summarize(rt_a_mean = mean(rt_a, na.rm = TRUE),
                   rt_b_mean = mean(rt_b, na.rm = TRUE),
                   rt_sd = sd(rt)) %>%
  dplyr::mutate(diff = rt_b_mean - rt_a_mean,
                D1_by_stim = round(diff / rt_sd, 3)) %>%
  ungroup() %>%
  group_by(unique_identifier,
           stimulus) %>%
  select(unique_identifier, 
         stimulus,
         D1_by_stim) %>%
  dplyr::summarize(D1_by_stim = round(mean(D1_by_stim), 3)) %>%
  spread(stimulus, D1_by_stim) 
} else {
  D1_by_stim_df <- data.frame(unique_identifier=1)
}

# pos stim
posstim <- sort(unlist(unique(cleaned_df[cleaned_df$trial_type %in% c(1,3),'stimulus'])))

# neg stim
negstim <- sort(unlist(unique(cleaned_df[cleaned_df$trial_type %in% c(2,4),'stimulus'])))

         
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

penn_state_filtered_cleaned_df <-
  filtered_cleaned_df %>%
  filter(((trial_type==1 | trial_type==4) & !is.na(rt_a)) | 
         ((trial_type==2 | trial_type==3) & !is.na(rt_b))
  ) 

penn_state_D1_df <-  
  penn_state_filtered_cleaned_df %>%
  group_by(unique_identifier,
           test_block_pair) %>%
  dplyr::summarize(rt_a_mean = mean(rt_a, na.rm = TRUE),
                   rt_b_mean = mean(rt_b, na.rm = TRUE),
                   rt_sd = sd(rt)) %>%
  dplyr::mutate(diff = rt_b_mean - rt_a_mean, # this is effectively a rowwise() calculation as we have group_by() participant and then summarize()'d. rowwise() not included for brevity.
                D1_ps = round(diff / rt_sd, 3)) %>% 
  ungroup() %>%
  group_by(unique_identifier) %>%
  dplyr::summarize(D1_ps = round(mean(D1_ps), 3)) %>%
  select(unique_identifier, 
         D1_ps) %>%
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
                penn_state_D1_df,
                mean_rt_df,
                percentage_accuracy_and_fast_trials_df,
                D1_by_stim_df,
                block_accuracy_df,
                block_rtmed_df),
           by = "unique_identifier",
           type = "full") %>%
  rowwise() %>%
  mutate(passed_practice_blocks = ifelse(!is.na(D1), TRUE, FALSE))
 
D_df <-
  D1_df %>% 
  dplyr::summarize(D = round(mean(D1, na.rm=TRUE),3))

if (nrow(filtered_cleaned_df)>0) {
  
D_by_tt_df <-
  D1_by_tt_df %>% 
  ungroup() %>%
  dplyr::summarize(D_trial_type_1 = round(mean(D1_trial_type_1, na.rm = TRUE),3), 
                   D_trial_type_2 = round(mean(D1_trial_type_2, na.rm = TRUE),3),
                   D_trial_type_3 = round(mean(D1_trial_type_3, na.rm = TRUE),3), 
                   D_trial_type_4 = round(mean(D1_trial_type_4, na.rm = TRUE),3))
} else {
  D_by_tt_df <- data.frame() 
}
D_by_stim_df <-
  D1_by_stim_df[,-1] %>% 
  ungroup() %>%
  summarise_all(funs(mean))

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
  error.num.prt = nrow(cleaned_df)-sum(cleaned_df$accuracy) 
  error.rate.prt = 1-(sum(cleaned_df$accuracy) / nrow(cleaned_df))

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
    D=D_df$D,
    D_by_tt_df=D_by_tt_df,
    D_by_stim_df=D_by_stim_df,
    posstim=posstim,
    negstim=negstim,
    output_df=output_df,
    cleaned_df=cleaned_df
  ))
}


