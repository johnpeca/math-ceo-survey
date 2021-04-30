install.packages("aod")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("googlesheets4")

install.packages("likert")
install.packages("ggthemes")
install.packages("HH")

library(aod)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(googlesheets4)

library(likert)
library(ggthemes)
library(tidyr)
library(tibble)
library(forcats)
library(HH)

##############################
#        Initial setup       #

lik5_lab = c("Strongly disagree","Somewhat disagree"
             ,"Neutral"
             ,"Somewhat agree","Strongly agree")
lik7_lab = c("Strongly disagree","Disagree","Somewhat disagree"
             ,"Neutral"
             ,"Somewhat agree","Agree","Strongly agree")

# UPDATE to where you want your images to go
setwd("~/Documents/R/math ceo analysis/survey analysis/images")

# used for the image ratios

disp_r = function(n) {
  print(100+50*(n+1))
}
w = 800

##############################
# Start with student results #

student_results = read_sheet("1xtYNQDWTxTeygVejZOP04v9Y0tW7FMb0JC3s4Qw7zWE"
                             ,sheet = "Form Responses 1"
                             ,range = "E2:AF28")
student_key = read_sheet("1xtYNQDWTxTeygVejZOP04v9Y0tW7FMb0JC3s4Qw7zWE"
                         ,sheet = "Question Key")

cat_s = student_key$category
cat_s = cat_s[!is.na(cat_s)]
cat_s = unique(cat_s)

r_s0 = student_results[,which(!is.na(student_key$category))]
# to force a category for each, adding one more scale to be removed later
for (i in 1:5){ 
  r_s0[nrow(r_s0)+1,] = i
}

r_s1 = table(r_s0[,1])
for (i in 2:ncol(r_s0)){
  r_s1 = rbind(r_s1,table(r_s0[,i]))
}
r_s1 = r_s1 - 1 # removing added point
rm(r_s0)

rownames(r_s1) = student_key$display_question[which(!is.na(student_key$category))]
colnames(r_s1) = lik5_lab

student_key = student_key[which(!is.na(student_key$category)),]

summary_s = as.data.frame(r_s1)  # likert function uses a summary table
summary_s = rownames_to_column(summary_s, "Question")
rm(r_s1)

summary_s = summary_s[c(-4)] # removes neutral

# Create subsets for each category type (View "cat_s")

tech_s = summary_s[which(student_key$category == cat_s[1]),]
m_motiv_s = summary_s[which(student_key$category == cat_s[2]),]
aut_s = summary_s[which(student_key$category == cat_s[3]),]
rel_s = summary_s[which(student_key$category == cat_s[4]),]
comp_s = summary_s[which(student_key$category == cat_s[5]),]
ped_str_s = summary_s[which(student_key$category == cat_s[6]),]
g_sup_s = summary_s[which(student_key$category == cat_s[7]),]
m_sup_s = summary_s[which(student_key$category == cat_s[8]),]

# Generate Likert charts

x = tech_s
png("tech_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Technology use and proficiency",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = m_motiv_s
png("m_motiv_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Math motivational beliefs",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = aut_s
png("aut_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Student autonomy with learning strategies",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = rel_s
png("rel_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Relationships and sense of belonging",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = comp_s
png("comp_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Content comprehension and proficiency",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = ped_str_s
png("ped_str_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Pedagogical strategies",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = g_sup_s
png("g_sup_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "General comfort and support within program",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = m_sup_s
png("m_sup_s.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Math comfort level",
       ylab = NULL, positive.order = TRUE)
dev.off()

##############################
# Now look at mentor results #

mentor_results = read_sheet("1SEPDkVCaSWVb1Goh4daT-TqsKgTkrEl5Gbp1OX2_baw"
                            ,sheet = "Form Responses 1"
                            ,range = "D2:AS35")
mentor_key = read_sheet("1SEPDkVCaSWVb1Goh4daT-TqsKgTkrEl5Gbp1OX2_baw"
                        ,sheet = "Question Key")

cat_m = mentor_key$category
cat_m = cat_m[!is.na(cat_m)]
cat_m = unique(cat_m)

r_m0 = mentor_results[,which(!is.na(mentor_key$category))]

for (i in 1:ncol(r_m0)){
  if (r_m0[1,i] > 7){ # Adding a logical statement to identify alpha characters
    r_m0[,i] = ifelse(r_m0[,i] == "Strongly agree",7,
                ifelse(r_m0[,i] == "Agree",5,
                 ifelse(r_m0[,i] == "Neutral",4,
                  ifelse(r_m0[,i] == "Disagree",3,1
                              ))))
  }
}

for (i in 1:7){ # Add extra datapoint so each scale nonempty
  r_m0[nrow(r_m0)+1,] = i
}

r_m1 = table(r_m0[,1])
for (i in 2:ncol(r_m0)){
  r_m1 = rbind(r_m1,table(r_m0[,i]))
}
r_m1 = r_m1 - 1 # removing added point
rm(r_m0)

rownames(r_m1) = mentor_key$display_question[which(!is.na(mentor_key$category))]
colnames(r_m1) = lik7_lab

mentor_key = mentor_key[which(!is.na(mentor_key$category)),]

summary_m = as.data.frame(r_m1)  # likert function uses a summary table
summary_m = rownames_to_column(summary_m, "Question")
rm(r_m1)

summary_m = summary_m[c(-5)] # removes neutral

# Create subsets for each category type (View "cat_m")

rel_m = summary_m[which(mentor_key$category == cat_m[1]),]
teach_m = summary_m[which(mentor_key$category == cat_m[2]),]
career_m = summary_m[which(mentor_key$category == cat_m[3]),]
prog_assess_m = summary_m[which(mentor_key$category == cat_m[4]),]
m_motiv_m = summary_m[which(mentor_key$category == cat_m[5]),]
action_m = summary_m[which(mentor_key$category == cat_m[6]),]
empathy_m = summary_m[which(mentor_key$category == cat_m[7]),]
tech_m = summary_m[which(mentor_key$category == cat_m[8]),]
m_knowledge_m = summary_m[which(mentor_key$category == cat_m[9]),]

# Correct 5-point Likert in dataset (default is 7-point)
empathy_m = empathy_m[c(-3,-6)]
m_knowledge_m = m_knowledge_m[c(-3,-6)]

# Generate Likert charts

x = rel_m
png("rel_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Relationships and sense of belonging",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = teach_m
png("teach_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Teaching growth",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = career_m
png("career_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Future careers",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = prog_assess_m
png("prog_assess_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Program assessment",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = m_motiv_m
png("m_motiv_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Math motivational beliefs",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = action_m
png("action_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = action_m, as.percent = "noRightAxis", 
       main = "Contributions to broader community",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = empathy_m
png("empathy_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Empathy",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = tech_m
png("tech_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = "Technology use and proficiency",
       ylab = NULL, positive.order = TRUE)
dev.off()

x = m_knowledge_m
png("m_knowledge_m.png",
    width = w, height = disp_r(nrow(x)))
likert(Question ~., data = x, as.percent = "noRightAxis", 
       main = cat_m[9],
       ylab = NULL, positive.order = TRUE)
dev.off()
