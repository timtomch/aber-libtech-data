# This script takes the data prepared in the data_prep.R script
# and analyses it for project success.

# Load prepped data
source('scripts/data_prep.R')



# Define value mapping
# In Q9,  "All requirements were met" is coded with 1,      mapped to +2 points
#         "Some requirements were not met" is coded with 2, mapped to -2 points
# In Q10, "The work was complete" is coded with 1,          mapped to +2 points
#         "Additional work was needed" is coded with 2,     mapped to -2 points
#                                                   NA is   mapped to 0 point.
reqs_map <- cbind(c(1,2,NA),c(2,-2,0))

# In Q11, Q12, Q13 and Q14, a Likert scale is used with the following mapping:
#           "Strongly disagree" is coded with 1,            mapped to -2 points
#           "Somewhat disagree" is coded with 2,            mapped to -1 point
#           "Neither agree nor disagree" is coded with 3,   mapped to 0 point
#           "Somewhat agree" is coded with 4,               mapped to +1 point
#           "Strongly agree" is coded with 5,               mapped to +2 points
#                                                   NA is   mapped to 0 point.
likert_map <- cbind(c(1,2,3,4,5,NA),c(-2,-1,0,1,2,0))

# Compute project success metric using process variables

reqs_score <- (reqs_map[match(survey$Q9, reqs_map[,1]),2]
               + reqs_map[match(survey$Q10, reqs_map[,1]),2])
statement_score <- (likert_map[match(survey$Q11_1_a, likert_map[,1]),2]
                  + likert_map[match(survey$Q11_2_a, likert_map[,1]),2]
                  + likert_map[match(survey$Q11_3_a, likert_map[,1]),2]
                  + likert_map[match(survey$Q11_4_a, likert_map[,1]),2])

# Normalize (max possible value is 6*2 = 12)
process_score <- (reqs_score + statement_score)/12
#print(process_score)

# Compute project success metric using outcome variable (deLone + McNeal)
# Normalize (max possible value is 16*2 = 32)
delonemclean_score <- (likert_map[match(survey$Q12_1_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_2_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_3_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_4_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_5_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_6_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_7_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_8_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_9_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_10_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q12_11_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q13_1_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q13_2_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q14_1_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q14_2_a, likert_map[,1]),2]
                       + likert_map[match(survey$Q14_3_a, likert_map[,1]),2])/32
#print(delonemclean_score)

# Correlation between the two success scores
score_correlation <- cor(process_score,delonemclean_score)
print(paste("Correlation between project success scores: ", score_correlation))

# Question for Tom: how can I determine the p value?
# summary(lm(delonemclean ~ process, data = scores_df))
# gives different results than cor()

# Load the ggplot2 library, for plotting results.
library("ggplot2")
scores_df <- data.frame(process = process_score, delonemclean = delonemclean_score)
scores_plot <- ggplot(scores_df, aes(x=process, y=delonemclean)) +
                     geom_point(shape=1) +     # Use hollow circles
                     geom_smooth(method=lm) +  # Add linear regression line
                     xlab("Project success defined as on time and budget (process)") + 
                     ylab("Project success defined on deLone & McLean framework")
print(scores_plot)
ggsave(paste(plotspath,"success_factor_correlation.png", sep = "/"))