library(shiny)
library(shinydashboard)
library(tidyverse)
library(lazyeval)
library(DT)
library(vcd)
library(xtable)
#this script will be called as a source from the server.r 

#open .csv file
my.data <- read_csv("shinydata.csv")

#filter for cancer
my.data %>% filter(aact_name_2 == "Cancer") -> aact2cancer.filt
aact2cancer.filt

my.data %>% filter(aact_name_3 == "Advanced Cancer") -> aact3cancer.filt


aact2cancer.filt[!rev(duplicated(rev(aact2cancer.filt$user_id))),] -> cancer.uniq

my.data %>% filter(str_detect(name, 'cancer')) %>%
  filter(name != "Stomach Tumor (non cancerous)") -> namefilt

bind.aact <- bind_rows(aact2cancer.filt, aact3cancer.filt)
bind.name <- bind_rows(namefilt, bind.aact)
bind.name[!rev(duplicated(rev(bind.name$user_id))),] -> cancer.data

#filter psych
my.data %>%
  filter(name == "ADHD" |
           name == "ADD" |
           name == "Asperger Syndrome" |
           name == "Obsessive Compulsive Disorder (OCD)" |
           name == "Depression" |
           name == "Anxiety Disorder" |
           name == "Panic disorder" |
           name == "Post Traumatic Stress Disorder (PTSD)" |
           name == "Autism" |
           #personality disorders
           name == "Bipolar disorder" |
           name == "Schizoaffective disorder" |
           name == "Dissociative Identity Disorder (or Multiple Personality Disorder)" |
           name == "Lewy body dementia" |
           name == "Dementia" |
           #Eating Disorders
           name == "Binge Eating Disorder (BED)" |
           name == "Bulimia Nervosa" |
           name == "Anorexia Nervosa") -> psych_cond

#filter for digest
my.data %>%
  filter(name == "Ulcerative Procitis" |
           name == "Ulcerative Colitis" |
           name == "Crohn's Disease" |
           name == "Irritable Bowel Syndrome (IBS)" |
           name == "Gallstones" |
           name == "Hemmorhoids" |
           name == "Diverticulitis" |
           name == "Diverticulosis" |
           name == "Ischemic colitis" |
           name == "Lactose Intolerance" |
           name == "Gastroesophageal Reflux Disease (GERD)") -> digest_cond

#digestive disorders discussed
my.data %>%
  filter(name == "Ulcerative Procitis" |
           name == "Ulcerative Colitis" |
           name == "Crohn's Disease" |
           name == "Irritable Bowel Syndrome (IBS)" |
           name == "Diverticulitis" |
           name == "Diverticulosis" |
           name == "Ischemic colitis") -> digest.data

#filter diabetes?

#change ethnicity names
ethnicity.names <- c('WHITE_EUROPEAN' = 'White\nEuropean',
                     'WHITE_HISPANIC' = 'White\nHispanic',
                     'BLACK_AMERICAN' = 'Black\nAmerican',
                     'HISPANIC_OTHER' = 'Hispanic\nOther',
                     'OTHER' = 'Other',
                     'MIXED_OTHER' = 'Mixed\nOther',
                     'JEWISH_ASHKENAZI' = 'Jewish\nAshkenazi',
                     'SOUTH_ASIAN' = 'South\nAsian',
                     'EAST_ASIAN' = 'East\nAsian',
                     'NATIVE_AMERICAN' = 'Native\nAmerican',
                     'HISPANIC_CARIBBEAN' = 'Hispanic\nCaribbean',
                     'WHITE_NORTH_AFRICAN' = 'White\nNorth African',
                     'MIXED_OTHER' = 'Mixed\nOther',
                     'HISPANIC_OTHER' = 'Hispanic\nOther',
                     'SOUTHEAST_ASIAN' = 'Southeast\nAsian',
                     'MIXED_ASIAN_AND_WHITE' = 'Mixed\nAsian-White',
                     'INUIT' = 'Inuit',
                     'MESTIZO' = 'Mestizo',
                     'NATIVE_AMERICAN' = 'Native\nAmerican',
                     'AMERINDIAN' = 'Amerindian',
                     'MIXED_BLACK_AND_WHITE' = 'Mixed\nBlack-White',
                     'WEST_ASIAN' = 'West\nAsian',
                     'BLACK_CARIBBEAN' = 'Black\nCaribbean',
                     'BLACK_AFRICAN' = 'Black\nAfrican',
                     'JEWISH_MIZRAHI' = 'Jewish\nMizrahi',
                     'ARABIC' = 'Arabic',
                     'AUSTRALIAN_NATIVE' = 'Australian\nNative',
                     'JEWISH_SHEPHARDIC' = 'Jewish\nSephardic',
                     'MIXED_BLACK_AND_HISPANIC' = 'Mixed\nBlack-Hispanic',
                     'ASIAN' = 'Asian',
                     'BLACK' = 'Black',
                     'WHITE' = 'White',
                     'MIDDLE EASTERN' = 'Middle Eastern',
                     'HISPANIC' = 'Hispanic',
                     'OTHER' = 'Other')

#change psych names
my.data %>%
  filter(name == "ADHD" |
           name == "ADD" |
           name == "Asperger Syndrome" |
           name == "Obsessive Compulsive Disorder (OCD)" |
           name == "Depression" |
           name == "Anxiety Disorder" |
           name == "Panic disorder" |
           name == "Post Traumatic Stress Disorder (PTSD)" |
           name == "Autism" |
           #personality disorders
           name == "Bipolar disorder" |
           name == "Schizoaffective disorder" |
           name == "Dissociative Identity Disorder (or Multiple Personality Disorder)" |
           name == "Lewy body dementia" |
           name == "Dementia" |
           #Eating Disorders
           name == "Binge Eating Disorder (BED)" |
           name == "Bulimia Nervosa" |
           name == "Anorexia Nervosa") -> psych.data


psych.names <-  c('Asperger Syndrome' = 'Asperger\nSyndrome',
                  'Anxiety Disorder'= 'Anxiety\nDisorder',
                  'Panic disorder' = 'Panic\nDisorder',
                  'Schizoaffective disorder' = 'Schizoaffective\nDisorder',
                  'Dissociative Identity Disorder (or Multiple Personality Disorder)' =
                    'Dissociative\nIdentity Disorder',
                  'Lewy body dementia' = 'Lewy Body\nDementia',
                  'Binge Eating Disorder (BED)' = 'Binge Eating\nDisorder (BED)',
                  'Bulimia Nervosa' = 'Bulimia\nNervosa',
                  'Anorexia Nervosa' = 'Anorexia\nNervosa',
                  'Obsessive Compulsive Disorder (OCD)' = 'Obsessive\nCompulsive\nDisorder (OCD)',
                  'Post Traumatic Stress Disorder (PTSD)' = 'Post Traumatic\nStress Disorder (PTSD)'
                  )

#change cancer/psych names
cancer.names <- c('Throat cancer' = 'Throat\nCancer',
                  'Breast cancer' = 'Breast\nCancer',
                  'Pancreatic Cancer' = 'Pancreatic\nCancer',
                  'Skin Cancer' = 'Skin\nCancer',
                  'Colon cancer' = 'Colon\nCancer',
                  'Lung Cancer' = 'Lung\nCancer',
                  'Prostate Cancer' = 'Prostate\nCancer',
                  'Ovarian Cancer' = 'Ovarian\nCancer',
                  'Thyroid cancer- Papillary' = 'Thyroid Cancer\nPapillary',
                  'Thyroid cancer- Medullary' = 'Thyroid Cancer\nMedullary',
                  'Thyroid cancer- Follicular' = 'Thyroid Cancer\nFollicular',
                  'Bladder Cancer' = 'Bladder\nCancer',
                  'Cervical Cancer' = 'Cervical\nCancer',
                  'Testicular Cancer' = 'Testicular\nCancer',
                  'Thyroid cancer- Anaplastic' = 'Thyroid Cancer\nAnaplastic',
                  'Stomach Cancer' = 'Stomach\nCancer',
                  'Uterine Cancer' = 'Uterine\nCancer')

cancer.names1 <- c('Throat cancer' = 'Throat',
                   'Breast cancer' = 'Breast',
                   'Pancreatic Cancer' = 'Pancreatic',
                   'Skin Cancer' = 'Skin',
                   'Colon cancer' = 'Colon',
                   'Lung Cancer' = 'Lung',
                   'Prostate Cancer' = 'Prostate',
                   'Ovarian Cancer' = 'Ovarian',
                   'Thyroid cancer- Papillary' = 'Thyroid\nPapillary',
                   'Thyroid cancer- Medullary' = 'Thyroid\nMedullary',
                   'Thyroid cancer- Follicular' = 'Thyroid\nFollicular',
                   'Bladder Cancer' = 'Bladder',
                   'Cervical Cancer' = 'Cervical',
                   'Testicular Cancer' = 'Testicular',
                   'Thyroid cancer- Anaplastic' = 'Thyroid\nAnaplastic',
                   'Stomach Cancer' = 'Stomach',
                   'Uterine Cancer' = 'Uterine')

#all names
#cancer adjust
name_adjust <- c('Throat cancer' = 'Throat',
                 'Breast cancer' = 'Breast',
                 'Pancreatic Cancer' = 'Pancreatic',
                 'Skin Cancer' = 'Skin',
                 'Colon cancer' = 'Colon',
                 'Lung Cancer' = 'Lung',
                 'Prostate Cancer' = 'Prostate',
                 'Ovarian Cancer' = 'Ovarian',
                 'Thyroid cancer- Papillary' = 'Thyroid\nPapillary',
                 'Thyroid cancer- Medullary' = 'Thyroid\nMedullary',
                 'Thyroid cancer- Follicular' = 'Thyroid\nFollicular',
                 'Bladder Cancer' = 'Bladder',
                 'Cervical Cancer' = 'Cervical',
                 'Testicular Cancer' = 'Testicular',
                 'Thyroid cancer- Anaplastic' = 'Thyroid\nAnaplastic',
                 'Stomach Cancer' = 'Stomach',
                 'Uterine Cancer' = 'Uterine',
                 
                 #psychological disorder adjust
                 'Asperger Syndrome' = 'Asperger\nSyndrome',
                 'Anxiety Disorder'= 'Anxiety\nDisorder',
                 'Panic disorder' = 'Panic\nDisorder',
                 'Schizoaffective disorder' = 'Schizoaffective\nDisorder',
                 'Dissociative Identity Disorder (or Multiple Personality Disorder)' =
                   'Dissociative\nIdentity Disorder',
                 'Lewy body dementia' = 'Lewy Body\nDementia',
                 'Binge Eating Disorder (BED)' = 'Binge Eating\nDisorder (BED)',
                 'Bulimia Nervosa' = 'Bulimia\nNervosa',
                 'Anorexia Nervosa' = 'Anorexia\nNervosa',
                 'Obsessive Compulsive Disorder (OCD)' = 'Obsessive\nCompulsive\nDisorder (OCD)',
                 'Post Traumatic Stress Disorder (PTSD)' = 'Post Traumatic\nStress Disorder (PTSD)',
              
                 #GI adjust
                 'Ulcerative Procitis' = 'Ulcerative\nProcitis',
                 'Ulcerative Colitis' = 'Ulcerative\nColitis',
                 "Crohn's Disease" = "Crohn's\nDisease",
                 'Ischemic colitis' = 'Ischemic\nColitis',
                 'Lactose Intolerance' = 'Lactose\nIntolerance',
                 'Gastroesophageal Reflux Disease (GERD)' = 'Gastroesophageal\nReflux Disease (GERD)',
                          
                 #ethnicity/race adjust
                 'WHITE_EUROPEAN' = 'White\nEuropean',
                 'WHITE_HISPANIC' = 'White\nHispanic',
                 'BLACK_AMERICAN' = 'Black\nAmerican',
                 'HISPANIC_OTHER' = 'Hispanic\nOther',
                 'OTHER' = 'Other',
                 'MIXED_OTHER' = 'Mixed\nOther',
                 'JEWISH_ASHKENAZI' = 'Jewish\nAshkenazi',
                 'SOUTH_ASIAN' = 'South\nAsian',
                 'EAST_ASIAN' = 'East\nAsian',
                 'NATIVE_AMERICAN' = 'Native\nAmerican',
                 'HISPANIC_CARIBBEAN' = 'Hispanic\nCaribbean',
                 'WHITE_NORTH_AFRICAN' = 'White\nNorth African',
                 'MIXED_OTHER' = 'Mixed\nOther',
                 'HISPANIC_OTHER' = 'Hispanic\nOther',
                 'SOUTHEAST_ASIAN' = 'Southeast\nAsian',
                 'MIXED_ASIAN_AND_WHITE' = 'Mixed\nAsian-White',
                 'INUIT' = 'Inuit',
                 'MESTIZO' = 'Mestizo',
                 'NATIVE_AMERICAN' = 'Native\nAmerican',
                 'AMERINDIAN' = 'Amerindian',
                 'MIXED_BLACK_AND_WHITE' = 'Mixed\nBlack-White',
                 'WEST_ASIAN' = 'West\nAsian',
                 'BLACK_CARIBBEAN' = 'Black\nCaribbean',
                 'BLACK_AFRICAN' = 'Black\nAfrican',
                 'JEWISH_MIZRAHI' = 'Jewish\nMizrahi',
                 'ARABIC' = 'Arabic',
                 'AUSTRALIAN_NATIVE' = 'Australian\nNative',
                 'JEWISH_SHEPHARDIC' = 'Jewish\nSephardic',
                 'MIXED_BLACK_AND_HISPANIC' = 'Mixed\nBlack-Hispanic',
                 'ASIAN' = 'Asian',
                 'BLACK' = 'Black',
                 'WHITE' = 'White',
                 'MIDDLE EASTERN' = 'Middle\nEastern',
                 'HISPANIC' = 'Hispanic',
                 'OTHER' = 'Other')

#USplot code

#code for CMH test
#get data in table format for CMH package
# my.data %>%
#   select(has_condition,
#          gender, race, ethnicity, diagnosed_by_physician, takes_medication, is_genetic_testing, is_carrier) %>%
#   group_by(has_condition, gender, race, ethnicity) %>%
#   na.omit() %>% tally() -> CMH_df   #make sure to check if using na.omit AFTER creating the dataframe
# CMH_df
# 
# 
# #potential for selecting variables to test here!
# #https://shiny.rstudio.com/reference/shiny/1.0.5/renderPrint.html
# #create table for CMH test
# Table = xtabs(n ~ gender + has_condition + race, data = CMH_df)
# ftable(Table)
# mantelhaen.test(Table)
# 
# 
# #print output CMH test (table)!