library(ISLR)
library(dplyr)
library(rpart)
library(rpart.plot)
library(fastDummies)

#### exclude irrelevant value from the dataset to build decision tree
data <- df_decision_tree |> select(-c(project, ParticipantID,calc_language))

#### There are NA value in age. I decided to fill median age value in those NA
data <- data |> mutate(across(calc_age, ~replace_na(., median(., na.rm=TRUE))))

#### grouping LSOA into 5 deprivation decile 
data <- data |> mutate(                                                                       # transform the field to required
    calc_lsoa_imd_decile = case_when(
      calc_lsoa_imd_decile %in% c(1, 2) ~ '1 (most deprived)',
      calc_lsoa_imd_decile %in% c(3, 4) ~ '2',
      calc_lsoa_imd_decile %in% c(5, 6) ~ '3',
      calc_lsoa_imd_decile %in% c(7, 8) ~ '4',
      calc_lsoa_imd_decile %in% c(9, 10) ~ '5 (least deprived)',
      is.na(calc_lsoa_imd_decile) ~ 'Deprivation not known'
    ),
    calc_lsoa_imd_decile = factor(calc_lsoa_imd_decile))

# Convert factor variable into dummy columns 
data2 <- dummy_cols(data, remove_selected_columns = TRUE, remove_first_dummy  = TRUE)

# I excluded NA or Not known column from datasets. 
data2 <- data2|> select(-c(`calc_sex_Not known`,`calc_ethnic_group_Not stated`,
                         `calc_ethnic_group_Not known`,`calc_lsoa_rurality_group_category_Not known`,
                         `calc_marital_status_Prefer not to say`,`calc_marital_status_Not known`,
                         calc_lsoa_imd_decile_NA,`calc_invite_outcome_No response`,
                         calc_invite_outcome_Ineligible))

# Saving to Rds for python processing 
#data |> 
#  saveRDS(file = here('scripts', 'invites_demographics.Rds'))


#build the initial decision tree
# method is class for binary 
# control can decided. how much we had control in our model, the smaller value mean we have less control
tree2 <- rpart(calc_invite_outcome_Declined ~ ., data = data2, method = "class", control=rpart.control(cp=0.0001))

#identify best cp value to use from our model 
best2 <- tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree2 <- prune(tree2, cp=best2)

#plot the pruned tree
rpart.plot(pruned_tree2,type=5,
)

