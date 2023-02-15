#importing necessary libraries for methods used in feature engineering, modeling
# and model evaluation
library(tidyverse)
library(stringr)
library(ROCR)
library(tm)
library(text2vec)
library(text2vec)
library(SnowballC)
library(glmnet)
library(randomForest)
library(vip)
library(naivebayes)
library(e1071)
library(ranger)
library(xgboost)
library(caret)

# reading the datasets
train_x <- read_csv("ks_training_X.csv")
train_y <- read_csv("ks_training_y.csv")

test_x <- read_csv("ks_test_X.csv")

# joining the train_x and train_y datasets to make a train dataset.
# this will be used for model training
train <- train_x %>%
  left_join(train_y, by = "id") %>%
  mutate(success = as.factor(success),
         big_hit = as.factor(big_hit))

summary(train)

# external datasource with gender information associated with names
names =read_csv("baby_names.csv") %>%
  mutate(firstname = toupper(name),
         female_name =ifelse(percent_female>= .5,1,0)) %>%
  select(firstname,female_name)

# text mining cleaning tokenizer
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>%
    removePunctuation %>%
    removeWords(stopwords(kind="en")) %>% 
    word_tokenizer
}

# data cleaning function. 
# this consists all the feature engineering methods which are also referenced
# in the project report
data_cleaning <- function(data_train,test_data=FALSE){
  
  data_train <- data_train %>%
  mutate(region = as.factor(region),
         firstname =creator_name,
         state = ifelse(str_sub(location_slug,-2,-1) == "us",str_sub(location_slug,-5,-4),str_sub(location_slug,-2,-1)),
         state = as.factor(toupper(state)),
         time_deadline_launch_days = as.numeric(difftime(deadline,launched_at,units="days")),
         time_deadline_create_days = as.numeric(difftime(deadline,created_at,units="days")),
         time_launch_create_days  = as.numeric(difftime(launched_at,created_at,units="days")),
         goal_per_day_deadline_launch= goal/time_deadline_launch_days,
         goal_per_day_deadline_create= goal/time_deadline_create_days,
         deadline_month=as.factor(strftime(as.Date(deadline, format="%Y-%m-%d"),"%m")),
         deadline_year=as.factor(strftime(as.Date(deadline, format="%Y-%m-%d"),"%Y")),
         deadline_date=as.factor(strftime(as.Date(deadline, format="%Y-%m-%d"),"%d")),
         launched_at_month=as.factor(strftime(as.Date(launched_at, format="%Y-%m-%d"),"%m")),
         launched_at_year=as.factor(strftime(as.Date(launched_at, format="%Y-%m-%d"),"%Y")),
         launched_at_date=as.factor(strftime(as.Date(launched_at, format="%Y-%m-%d"),"%d")),
         created_at_month=as.factor(strftime(as.Date(created_at, format="%Y-%m-%d"),"%m")),
         created_at_year=as.factor(strftime(as.Date(created_at, format="%Y-%m-%d"),"%Y")),
         goal_bin = as.factor(ntile(goal,5))) %>%
    separate(firstname, into = ("firstname"), extra = "drop") %>%
  group_by(creator_id) %>%
  mutate(
    creator_repeat_count = n()
    )%>%
  ungroup() %>%
  group_by(location_type) %>%
  mutate(
    n_location = n(),
    location_type = as.factor(ifelse(n_location < 200,"Other",location_type))) %>%
  ungroup()

  data_train<-data_train %>%
    mutate(firstname = toupper(firstname)) %>%
    left_join(names,by="firstname") %>%
    mutate(female_name=case_when(
      female_name==1 ~"F",
      female_name==0 ~"M",
      is.na(female_name) ~"U"),
      female_name =as.factor(female_name))
  
  data_train <- data_train %>% 
          mutate ( 
          category_parent=as.factor(category_parent))

  if(test_data){
    data_train <- data_train %>%
      group_by(category_parent,category_name) %>%
        mutate(
          n_category_name = n(),
          category_name = as.factor(ifelse(category_name %in% train$category_name, category_name, paste("Other",category_parent))))
      }
      else{
      data_train <- data_train %>%
        group_by(category_parent,category_name) %>%
          mutate(
            n_category_name = n(),
            category_name = as.factor(ifelse(n_category_name < 750, paste("Other",category_parent), category_name)))
      }

  data_train <- data_train %>% 
        mutate(
            
          numfaces_project = as.factor(case_when(numfaces_project ==0 ~ "0",
                                                 numfaces_project ==1  ~ "1",
                                                 numfaces_project ==2  ~ "2",
                                                TRUE ~ "3+")),
          numfaces_creator = as.factor(case_when(numfaces_creator ==0 ~ "0",
                                                 numfaces_creator ==1  ~ "1",
                                                 numfaces_creator ==2  ~ "2",
                                                 TRUE ~ "3+")), 
          male_project = as.factor(case_when(male_project ==0 ~ "0",
                                             male_project ==1  ~ "1",
                                             male_project ==2  ~ "2",
                                            TRUE ~ "3+")),
          male_creator = as.factor(case_when(male_creator ==0 ~ "0",
                                             male_creator ==1  ~ "1",
                                             male_creator ==2  ~ "2",
                                              TRUE ~ "3+")),
          female_project = as.factor(case_when(female_project ==0 ~ "0",
                                               female_project ==1  ~ "1",
                                               female_project ==2  ~ "2",
                                                 TRUE ~ "3+")),
          female_creator = as.factor(case_when(female_creator ==0 ~ "0",
                                               female_creator ==1  ~ "1",
                                               female_creator ==2  ~ "2",
                                                 TRUE ~ "3+")),
          smiling_project = ifelse(smiling_project>100, 100, smiling_project), 
          smiling_creator = ifelse(smiling_creator>100, 100, smiling_creator),
          )
  

  data_train <- data_train %>% 
  mutate (diff_age_project = maxage_project - minage_project,
          diff_age_creator = maxage_creator - minage_creator,
          minage_project = as.factor(case_when(minage_project == 0 ~ "0",
                                             minage_project >=1 & minage_project<=6   ~ "1-6",
                                             minage_project >=7 & minage_project<=12   ~ "7-12",
                                             minage_project >=13 & minage_project<=18   ~ "13-18",
                                             minage_project >=19 & minage_project<=24   ~ "19-24",
                                             minage_project >=25 & minage_project<=30   ~ "25-30",
                                             minage_project >=31 & minage_project<=36  ~ "31-36",
                                             minage_project >=37 & minage_project<=42   ~ "37-42",
                                             minage_project >=43 & minage_project<=48   ~ "43-48",
                                             minage_project >=49 & minage_project<=54   ~ "49-54",
                                             minage_project >=54 & minage_project<=60   ~ "55-60",
                                              TRUE ~ "61-66")),
          maxage_project = as.factor(case_when(maxage_project == 0 ~ "0",
                                               maxage_project >=1 & maxage_project<=6   ~ "1-6",
                                               maxage_project >=7 & maxage_project<=12   ~ "7-12",
                                               maxage_project >=13 & maxage_project<=18   ~ "13-18",
                                               maxage_project >=19 & maxage_project<=24   ~ "19-24",
                                               maxage_project >=25 & maxage_project<=30   ~ "25-30",
                                               maxage_project >=31 & maxage_project<=36  ~ "31-36",
                                               maxage_project >=37 & maxage_project<=42   ~ "37-42",
                                               maxage_project >=43 & maxage_project<=48   ~ "43-48",
                                               maxage_project >=49 & maxage_project<=54   ~ "49-54",
                                               maxage_project >=54 & maxage_project<=60   ~ "55-60",
                                               TRUE ~ "61-66")),
          minage_creator = as.factor(case_when(minage_creator == 0 ~ "0",
                                               minage_creator >=1 & minage_creator<=6   ~ "1-6",
                                               minage_creator >=7 & minage_creator<=12   ~ "7-12",
                                               minage_creator >=13 & minage_creator<=18   ~ "13-18",
                                               minage_creator >=19 & minage_creator<=24   ~ "19-24",
                                               minage_creator >=25 & minage_creator<=30   ~ "25-30",
                                               minage_creator >=31 & minage_creator<=36  ~ "31-36",
                                               minage_creator >=37 & minage_creator<=42   ~ "37-42",
                                               minage_creator >=43 & minage_creator<=48   ~ "43-48",
                                               minage_creator >=49 & minage_creator<=54   ~ "49-54",
                                               minage_creator >=54 & minage_creator<=60   ~ "55-60",
                                               TRUE ~ "61-66")),
          
          maxage_creator = as.factor(case_when(maxage_creator == 0 ~ "0",
                                               maxage_creator >=1 & maxage_creator<=6   ~ "1-6",
                                               maxage_creator >=7 & maxage_creator<=12   ~ "7-12",
                                               maxage_creator >=13 & maxage_creator<=18   ~ "13-18",
                                               maxage_creator >=19 & maxage_creator<=24   ~ "19-24",
                                               maxage_creator >=25 & maxage_creator<=30   ~ "25-30",
                                               maxage_creator >=31 & maxage_creator<=36  ~ "31-36",
                                               maxage_creator >=37 & maxage_creator<=42   ~ "37-42",
                                               maxage_creator >=43 & maxage_creator<=48   ~ "43-48",
                                               maxage_creator >=49 & maxage_creator<=54   ~ "49-54",
                                               maxage_creator >=54 & maxage_creator<=60   ~ "55-60",
                                               TRUE ~ "61-66")),
          Null_col= as.factor(ifelse(is.na(isbwImg1),1,0)),
          isbwImg1= as.factor(ifelse(is.na(isbwImg1),'No colour', isbwImg1)),
          color_foreground= as.factor(ifelse(is.na(color_foreground),'No foreground', color_foreground)),
          color_background= as.factor(ifelse(is.na(color_background),'No background', color_background)),
          accent_color_red = ifelse(is.na(accent_color),'No red',str_sub(accent_color,0,2)),
          accent_color_green = ifelse(is.na(accent_color),'No green',str_sub(accent_color,3,4)),
          accent_color_blue = ifelse(is.na(accent_color),'No blue',str_sub(accent_color,5,6)),
          isTextPic = as.factor(ifelse(is.na(isTextPic),"NULL", isTextPic)),
          isLogoPic = as.factor(ifelse(is.na(isLogoPic),"NULL", isLogoPic)),
          isCalendarPic = as.factor(ifelse(is.na(isCalendarPic),"NULL", isCalendarPic)),
          isDiagramPic = as.factor(ifelse(is.na(isDiagramPic),"NULL", isDiagramPic)),
          isShapePic = as.factor(ifelse(is.na(isShapePic),"NULL", isShapePic)),
    )
if(test_data){
  data_train <- data_train %>%
    group_by(accent_color_red) %>%
    mutate(
      n_red = n(),
      accent_color_red = as.factor(ifelse(accent_color_red %in% train$accent_color_red,accent_color_red,"Other"))) %>%
    ungroup() %>%

    group_by(accent_color_blue) %>%
    mutate(
      n_blue = n(),
      accent_color_blue = as.factor(ifelse(accent_color_blue %in% train$accent_color_blue,accent_color_blue,"Other"))) %>%
    ungroup()%>%

    group_by(accent_color_green) %>%
    mutate(
      n_green = n(),
      accent_color_green = as.factor(ifelse(accent_color_green %in% train$accent_color_green,accent_color_green,"Other"))) %>%
    ungroup()
}
else{
  data_train <- data_train %>% 
     group_by(accent_color_red) %>%
     mutate(
       n_red = n(),
       accent_color_red = as.factor(ifelse(n_red < 600,"Other",accent_color_red))) %>%
     ungroup() %>%
     
     group_by(accent_color_blue) %>%
     mutate(
       n_blue = n(),
       accent_color_blue = as.factor(ifelse(n_blue < 850,"Other",accent_color_blue))) %>%
     ungroup()%>%
     
     group_by(accent_color_green) %>%
     mutate(
       n_green = n(),
       accent_color_green = as.factor(ifelse(n_green < 600,"Other",accent_color_green))) %>%
     ungroup()
}

data_train <- data_train %>% 
  mutate(
    num_words_bin = as.factor(ntile(num_words,5)),
    contains_youtube = as.factor(contains_youtube),
    sentence_counter_bin = as.factor(ntile(sentence_counter,5)),
    grade_level_bin = as.factor(ntile(grade_level,4)),
    afin_pos_neg= ifelse(afinn_pos>=afinn_neg,1,0),
    afinn_pos_bin = as.factor(ntile(afinn_pos,5)),
    afinn_neg_bin = as.factor(ntile(afinn_neg,5)),
    afinn_pos_by_neg = ifelse(afinn_neg==0,0,afinn_pos/afinn_neg),
    afinn_neg_tot = afinn_neg/sum(afinn_neg),
    afinn_pos_tot = afinn_pos/sum(afinn_pos)
)

data_train <- data_train  %>% mutate(
  reward_amounts=ifelse(is.na(reward_amounts) |  reward_amounts=="B,A,D" ,0, reward_amounts),
  tag_names=ifelse(is.na(tag_names),"", tag_names),
  captions=ifelse(is.na(captions),"", captions),
  blurb=ifelse(is.na(blurb),"", blurb)
  )

data_train$reward_amounts_avg <-  sapply(strsplit(as.character(data_train$reward_amounts), ",", fixed=T), function(x) mean(as.numeric(x)))
data_train$reward_amounts_count <-  sapply(strsplit(as.character(data_train$reward_amounts), ",", fixed=T), function(x) length(as.numeric(x)))
data_train$reward_amounts_min <-  sapply(strsplit(as.character(data_train$reward_amounts), ",", fixed=T), function(x) min(as.numeric(x)))
data_train$reward_amounts_max <-  sapply(strsplit(as.character(data_train$reward_amounts), ",", fixed=T), function(x) max(as.numeric(x)))
data_train$reward_amounts_sd <-  sapply(strsplit(as.character(data_train$reward_amounts), ",", fixed=T), function(x) sd(as.numeric(x)))
data_train$tag_length <-  sapply(strsplit(as.character(data_train$tag_names), "|", fixed=T), function(x) length(as.numeric(x)))
data_train$caption_length <-  sapply(strsplit(as.character(data_train$captions), " ", fixed=T), function(x) length(as.numeric(x)))
data_train$blurb_length <-  sapply(strsplit(as.character(data_train$blurb), " ", fixed=T), function(x) length(as.numeric(x)))
data_train <- data_train  %>% 
  mutate(
    reward_amounts_sd = ifelse(is.na(reward_amounts_sd),0,reward_amounts_sd),
    ADV_per_word= ifelse(num_words==0,0,ADV/num_words),
    NOUN_per_word= ifelse(num_words==0,0,NOUN/num_words),
    ADP_per_word= ifelse(num_words==0,0,ADP/num_words),
    PRT_per_word= ifelse(num_words==0,0,PRT/num_words),
    DET_per_word= ifelse(num_words==0,0,DET/num_words),
    PRON_per_word= ifelse(num_words==0,0,PRON/num_words),
    VERB_per_word= ifelse(num_words==0,0,VERB/num_words),
    NUM_per_word= ifelse(num_words==0,0,NUM/num_words),
    CONJ_per_word= ifelse(num_words==0,0,CONJ/num_words),
    ADJ_per_word= ifelse(num_words==0,0,ADJ/num_words),
    ADV_per_sen= ifelse(sentence_counter==0,0,ADV/sentence_counter),
    NOUN_per_sen= ifelse(sentence_counter==0,0,NOUN/sentence_counter),
    ADP_per_sen= ifelse(sentence_counter==0,0,ADP/sentence_counter),
    PRT_per_sen= ifelse(sentence_counter==0,0,PRT/sentence_counter),
    DET_per_sen= ifelse(sentence_counter==0,0,DET/sentence_counter),
    PRON_per_sen= ifelse(sentence_counter==0,0,PRON/sentence_counter),
    VERB_per_sen= ifelse(sentence_counter==0,0,VERB/sentence_counter),
    NUM_per_sen= ifelse(sentence_counter==0,0,NUM/sentence_counter),
    CONJ_per_sen= ifelse(sentence_counter==0,0,CONJ/sentence_counter),
    ADJ_per_sen= ifelse(sentence_counter==0,0,ADJ/sentence_counter),
    ADV_bin = as.factor(ntile(ADV,5)),
    NOUN_bin = as.factor(ntile(NOUN,5)),
    ADP_bin = as.factor(ntile(ADP,5)),
    PRT_bin = as.factor(ntile(PRT,5)),
    DET_bin = as.factor(ntile(DET,5)),
    PRON_bin = as.factor(ntile(PRON,5)),
    VERB_bin = as.factor(ntile(VERB,5)),
    NUM_bin = as.factor(ntile(NUM,5)),
    CONJ_bin = as.factor(ntile(CONJ,5)),
    ADJ_bin = as.factor(ntile(ADJ,5))
  )

# performing text mining for the attribure captions
prep_fun = tolower
tok_fun = cleaning_tokenizer

it_train_captions = itoken(data_train$captions,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = data_train$id,
                  progressbar = FALSE)
vocab_captions = create_vocabulary(it_train_captions)
vocab_captions = prune_vocabulary(vocab_captions, term_count_min = 100, doc_proportion_max = 0.5)[1]
c=""
for(i in 1:lengths(vocab_captions)){
  c = as.String(c)+ "|" + as.String(vocab_captions[i,])
}
data_train$caption_flag = rep(0, length(data_train$captions))
data_train[grep(c, data_train$captions, value = F), "caption_flag"] <- 1

# text mining for tag_names
it_train_tag_names = itoken(data_train$tag_names,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = data_train$id,
                  progressbar = FALSE)
vocab_tag_names = create_vocabulary(it_train_tag_names)
vocab_tag_names = prune_vocabulary(vocab_tag_names, term_count_min = 100)
vectorizer_tag = vocab_vectorizer(vocab_tag_names)
dtm_train_tag = create_dtm(it_train_tag_names, vectorizer_tag)
tag_names_list<-data.frame(as.matrix(dtm_train_tag), stringsAsFactors=False)

print(length(data_train))

# selecting variables to be used for model training and validation purposes
if(test_data){
data_train <- data_train %>%
  select(Null_col,region,state,time_deadline_launch_days,time_deadline_create_days,goal_per_day_deadline_launch,
         goal_per_day_deadline_create, deadline_month,deadline_year,launched_at_month,launched_at_year,
         created_at_month,created_at_year,goal,goal_bin,creator_repeat_count,location_type, category_parent,
         category_name, numfaces_project, numfaces_creator, male_project, male_creator, female_project,
         female_creator,smiling_project, smiling_creator, diff_age_project, diff_age_creator, 
         minage_project,maxage_project, minage_creator, maxage_creator,
         isbwImg1,color_foreground,color_background, accent_color_red, accent_color_green, accent_color_blue,
         isTextPic, isLogoPic,isCalendarPic, isDiagramPic, isShapePic,
         num_words_bin, avg_wordlengths,contains_youtube, sentence_counter_bin, avgsentencelength,
         avgsyls, grade_level_bin, afinn_pos_bin, afinn_neg_bin, afin_pos_neg,
         reward_amounts_avg,reward_amounts_count, reward_amounts_min,reward_amounts_max, 
         ADV_per_word,NOUN_per_word,ADP_per_word,PRT_per_word,DET_per_word,PRON_per_word,
         VERB_per_word,NUM_per_word, CONJ_per_word, ADJ_per_word,
         ADV_bin, NOUN_bin, ADP_bin,PRT_bin, DET_bin, PRON_bin, VERB_bin, NUM_bin, CONJ_bin, ADJ_bin,
         afinn_pos_tot,afinn_neg_tot,reward_amounts_sd )
}
else{
  data_train <- data_train %>%
    select(Null_col,region,state,time_deadline_launch_days,time_deadline_create_days,goal_per_day_deadline_launch,
           goal_per_day_deadline_create, deadline_month,deadline_year,launched_at_month,launched_at_year,
           created_at_month,created_at_year,goal,goal_bin,creator_repeat_count,location_type, category_parent,
           category_name, numfaces_project, numfaces_creator, male_project, male_creator, female_project,
           female_creator,smiling_project, smiling_creator, diff_age_project, diff_age_creator, 
           minage_project,maxage_project, minage_creator, maxage_creator,
           isbwImg1,color_foreground,color_background, accent_color_red, accent_color_green, accent_color_blue,
           isTextPic, isLogoPic,isCalendarPic, isDiagramPic, isShapePic,
           num_words_bin, avg_wordlengths,contains_youtube, sentence_counter_bin, avgsentencelength,
           avgsyls, grade_level_bin, afinn_pos_bin, afinn_neg_bin, afin_pos_neg,
           reward_amounts_avg,reward_amounts_count, reward_amounts_min,reward_amounts_max, 
           ADV_per_word,NOUN_per_word,ADP_per_word,PRT_per_word,DET_per_word,PRON_per_word,
           VERB_per_word,NUM_per_word, CONJ_per_word, ADJ_per_word,
           ADV_bin, NOUN_bin, ADP_bin,PRT_bin, DET_bin, PRON_bin, VERB_bin, NUM_bin, CONJ_bin, ADJ_bin,
           afinn_pos_tot,afinn_neg_tot,reward_amounts_sd,
           big_hit, backers_count, success
           )
}

return(data_train)
}

# calling the cleaning methods for training and test_x datasets
train <- data_cleaning(train)
test_x <- data_cleaning(test_x,TRUE)

# selecting big_hit as the target variable
train_big_hit <- train %>% select(-c(success, backers_count))

#Model 1: GLM
train_insts1 <- train_big_hit[sample(nrow(train_big_hit)),]
k=5 
folds <- cut(seq(1,nrow(train_insts1)),breaks=k,labels=FALSE)
glm_auc_big_hit_list = rep(0, k)

for(i in 1:k){
  print(i)
  train_inds <- which(folds==i,arr.ind=TRUE)
  
  data_big_hit_train <- train_insts1[-train_inds, ]
  data_big_hit_valid <- train_insts1[train_inds, ]
  
  success_model1 <- glm(data = data_big_hit_train, big_hit ~ ., family="binomial")
  probs_success_model1 <- predict(success_model1, newdata = data_big_hit_valid, type='response')
  preds<- prediction(probs_success_model1,data_big_hit_valid$big_hit)
  glm_auc_big_hit_list[i] <- performance(preds,measure = "auc")@y.values[[1]]
}
glm_auc_big_hit = mean(glm_auc_big_hit_list)
print(glm_auc_big_hit)


#Model 2: Ridge
grid <- 10^seq(10,-10,length=1000)
k<-5

train_insts_big2 = sample(nrow(train_big_hit), .7*nrow(train_big_hit))

big_hit_x2 <- model.matrix(big_hit~.,train_big_hit)
big_hit_y2 <- train_big_hit$big_hit 

train_big_x2 <- big_hit_x2[train_insts_big2,]
valid_big_x2 <- big_hit_x2[-train_insts_big2,]

train_big_y2<-big_hit_y2[train_insts_big2]
valid_big_y2<-big_hit_y2[-train_insts_big2]

ridge_big <- cv.glmnet(train_big_x2, train_big_y2, family="binomial", alpha=0, lambda=grid, nfolds=k)

plot(ridge_big)
bestlam_ridge <- ridge_big$lambda.min

pred_big_2 <- predict(ridge_big, s=bestlam_ridge, newx = valid_big_x2,type="response")
preds_2<- prediction(pred_big_2,valid_big_y2)
ridge_auc_big_hit <- performance(preds_2,measure = "auc")@y.values[[1]]
print(ridge_auc_big_hit)

roc_full_ridge <- performance(preds_2, "tpr", "fpr")
plot(roc_full_ridge, col = "blue", lwd = 2)



#Model 3: Lasso
grid <- 10^seq(10,-10,length=1000)
k<-5

train_insts_big3 = sample(nrow(train_big_hit), .7*nrow(train_big_hit))

big_hit_x3 <- model.matrix(big_hit~.,train_big_hit)
big_hit_y3 <- train_big_hit$big_hit 

train_big_x3 <- big_hit_x3[train_insts_big3,]
valid_big_x3 <- big_hit_x3[-train_insts_big3,]

train_big_y3 <- big_hit_y3[train_insts_big3]
valid_big_y3 <- big_hit_y3[-train_insts_big3]

lasso_big <- cv.glmnet(train_big_x3, train_big_y3, family="binomial", alpha=1, lambda=grid, nfolds=k)

plot(lasso_big)
bestlam_lasso <- lasso_big$lambda.min

pred_big_3 <- predict(lasso_big, s=bestlam_lasso, newx = valid_big_x3,type="response")
preds_3<- prediction(pred_big_3,valid_big_y3)
lasso_auc_big_hit <- performance(preds_3,measure = "auc")@y.values[[1]]
print(lasso_auc_big_hit)

roc_full_lasso <- performance(preds_3, "tpr", "fpr")
plot(roc_full_lasso, col = "red", lwd = 2)


#Model 4: Naive Bayes
train_insts_b4 <- train_big_hit[sample(nrow(train_big_hit)),]
k=5
folds <- cut(seq(1,nrow(train_insts_b4)),breaks=k,labels=FALSE)

nb_auc_big_hit_list = rep(0,k)

for(i in 1:k){
  print(i)
  train_inds <- which(folds==i,arr.ind=TRUE)
  
  data_big_hit_train <- train_insts_b4[-train_inds, ]
  data_big_hit_valid <- train_insts_b4[train_inds, ]
  
  smoothed_model <- naiveBayes(big_hit~.,laplace = 100, data=data_big_hit_train)
  
  predictions <- predict(smoothed_model,newdata=data_big_hit_valid, type="raw")[,2]
  preds <- prediction(predictions, data_big_hit_valid$big_hit)
  nb_auc_big_hit_list[i] = performance(preds, measure = "auc")@y.values[[1]]
  
}

nb_auc_big_hit <- mean(nb_auc_big_hit_list)
print(nb_auc_big_hit)


#Model 5: Random Forest

train_insts5 <- train_big_hit[sample(nrow(train_big_hit)),]
k=5
folds <- cut(seq(1,nrow(train_insts5)),breaks=k,labels=FALSE)

rf_auc_big_hit_list = rep(0, k)

for(i in 1:k){
  print(i)
  train_inds <- which(folds==i,arr.ind=TRUE)
  
  data_big_hit_train <- train_insts5[-train_inds, ]
  data_big_hit_valid <- train_insts5[train_inds, ]
  
  rf.mod <- randomForest(
    data_big_hit_train[,1:81],
    y = data_big_hit_train$big_hit,
    subset = train_inds,
    mtry = 7, ntree = 100,
    importance = TRUE)
  
  rf_preds <- predict(rf.mod, newdata = data_big_hit_valid, type="prob")[,2]
  preds <- prediction(rf_preds, data_big_hit_valid$big_hit)
  
  rf_auc_big_hit_list[i] <- performance(preds, measure = "auc")@y.values[[1]]
  
}

rf_auc_big_hit<-mean(rf_auc_big_hit_list)
print(rf_auc_big_hit)


#Model 6: LM

train_insts6 <- train_big_hit[sample(nrow(train_big_hit)),]
k=5 
folds <- cut(seq(1,nrow(train_insts6)),breaks=k,labels=FALSE)
lm_auc_big_hit_list = rep(0, k)

for(i in 1:k){
  print(i)
  train_inds <- which(folds==i,arr.ind=TRUE)
  
  data_big_hit_train <- train_insts6[-train_inds, ]
  data_big_hit_valid <- train_insts6[train_inds, ]
  
  model6 <- lm(data = data_big_hit_train, big_hit ~ .)
  
  probs_model6 <- predict(model6, newdata = data_big_hit_valid)
  preds6<- prediction(probs_model6,data_big_hit_valid$big_hit)
  lm_auc_big_hit_list[i] <- performance(preds6,measure = "auc")@y.values[[1]]
}
lm_auc_big_hit = mean(lm_auc_big_hit_list)
print(lm_auc_big_hit)


###Submit Model:Lasso###############
grid <- 10^seq(10,-10,length=1000)
k<-5

test_x$big_hit <- as.factor("NO")
complete_data<-rbind(train_big_hit,test_x)

big_hit_xf <- model.matrix(big_hit~.,complete_data)
big_hit_yf <- train_big_hit$big_hit

train_instsf<-1:nrow(train_big_hit)

train_big_xf <-(big_hit_xf[train_instsf,])
test_big_xf <- (big_hit_xf[-train_instsf,])

train_big_yf <- big_hit_yf

lasso_big_final <- cv.glmnet(train_big_xf, train_big_yf, family="binomial", alpha=1, lambda=grid, nfolds=k)

plot(lasso_big_final)
bestlam_lassof <- lasso_big_final$lambda.min
pred_big_f <- predict(lasso_big_final, s=bestlam_lassof, newx = test_big_xf,type="response")

write.table(pred_big_f, "big_hit_group13.csv", row.names = FALSE)