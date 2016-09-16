# This script takes the data prepared in the data_prep.R script
# and analyses it for project success.

# Load prepped data
source('scripts/data_prep.R')



# Define value mapping

# In Q9,  "All requirements were met" is coded with 1,      mapped to +2 points
#         "Some requirements were not met" is coded with 2, mapped to -2 points
# In Q10, "The work was complete" is coded with 1,          mapped to +2 points
#         "Additional work was needed" is coded with 2,     mapped to -2 points

reqs_map <- cbind(c(1,2),c(2,-2))

# In Q11, Q12, Q13 and Q14, a Likert scale is used with the following mapping:
#           "Strongly disagree" is coded with 1,            mapped to -2 points
#           "Somewhat disagree" is coded with 2,            mapped to -1 point
#           "Neither agree nor disagree" is coded with 3,   mapped to 0 point
#           "Somewhat agree" is coded with 4,               mapped to +1 point
#           "Strongly agree" is coded with 5,               mapped to +2 points

likert_map <- cbind(c(1,2,3,4,5),c(-2,-1,0,1,2))

# Compute project success metric using process variables

reqs_score <- rowSums(cbind(reqs_map[match(survey$Q9, reqs_map[,1]),2],
                reqs_map[match(survey$Q10, reqs_map[,1]),2]), na.rm = TRUE)
statement_score <- rowSums(cbind(likert_map[match(survey$Q11_1_a, likert_map[,1]),2],
                  likert_map[match(survey$Q11_2_a, likert_map[,1]),2],
                  likert_map[match(survey$Q11_3_a, likert_map[,1]),2],
                  likert_map[match(survey$Q11_4_a, likert_map[,1]),2]), na.rm = TRUE)

# Normalize (max possible value is 6*2 = 12)
process_score <- (reqs_score + statement_score)/12
#print(process_score)

# Compute project success metric using outcome variable (deLone + McNeal)
# Normalize (max possible value is 16*2 = 32)
delonemclean_score <- rowSums(cbind(likert_map[match(survey$Q12_1_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_2_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_3_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_4_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_5_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_6_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_7_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_8_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_9_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_10_a, likert_map[,1]),2],
                       likert_map[match(survey$Q12_11_a, likert_map[,1]),2],
                       likert_map[match(survey$Q13_1_a, likert_map[,1]),2],
                       likert_map[match(survey$Q13_2_a, likert_map[,1]),2],
                       likert_map[match(survey$Q14_1_a, likert_map[,1]),2],
                       likert_map[match(survey$Q14_2_a, likert_map[,1]),2],
                       likert_map[match(survey$Q14_3_a, likert_map[,1]),2]), na.rm = TRUE)/32
#print(delonemclean_score)

# Correlation between the two success scores
#score_correlation <- cor(process_score,delonemclean_score)
score_correlation <- lm(process_score ~ delonemclean_score)
print("Correlation between project success metrics:")
print(summary(score_correlation))
#print(paste("Correlation between project success scores: ", score_correlation))

# Question for Tom: how can I determine the p value?
# summary(lm(delonemclean ~ process, data = scores_df))
# gives different results than cor()

# Load the ggplot2 library, for plotting results.
library("ggplot2")
scores_df <- data.frame(process = process_score, delonemclean = delonemclean_score)
success_factor_correlation <- ggplot(scores_df, aes(x=process, y=delonemclean)) +
                     geom_point(shape=1) +     # Use hollow circles
                     geom_smooth(method=lm) +  # Add linear regression line
                     xlab(expression(paste("Process-oriented project success score " (s[process])))) + 
                     ylab(expression(paste("Results-oriented project success score " (s[results]))))
ggsave(paste(plotspath,"success_factor_correlation.png", sep = "/"))

# Save graph as EMF format for import into MS Word :poop:
library("devEMF")
emf(file=paste(plotspath,"success_factor_correlation.emf", sep = "/"))
show(success_factor_correlation)
dev.off()



# Analyse whether there is a correlation between representation of a particular
# skill and project success.
# "Well represented" and "Somewhat represented" skills are grouped together
# for this metric.
# In Q8,  "Well represented is coded with 1,      mapped to 1
#         "Somewhat represented" is coded with 2, mapped to 1
#         "Not well represented" is coded with 3, mapped to 0
#                                         
skills_map <- cbind(c(1,2,3),c(1,1,0))
proj_mgmt_rep <- factor(skills_map[match(survey$Q8_1_a, skills_map[,1]),2], ordered=TRUE)
accounting_rep <- factor(skills_map[match(survey$Q8_2_a, skills_map[,1]),2], ordered=TRUE)
communications_rep <- factor(skills_map[match(survey$Q8_3_a, skills_map[,1]),2], ordered=TRUE)
accessibility_rep <- factor(skills_map[match(survey$Q8_4_a, skills_map[,1]),2], ordered=TRUE)
negotiation_rep <- factor(skills_map[match(survey$Q8_5_a, skills_map[,1]),2], ordered=TRUE)
UX_rep <- factor(skills_map[match(survey$Q8_6_a, skills_map[,1]),2], ordered=TRUE)
design_rep <- factor(skills_map[match(survey$Q8_7_a, skills_map[,1]),2], ordered=TRUE)
webdev_rep <- factor(skills_map[match(survey$Q8_8_a, skills_map[,1]),2], ordered=TRUE)
dev_rep <- factor(skills_map[match(survey$Q8_9_a, skills_map[,1]),2], ordered=TRUE)
sysadmin_rep <- factor(skills_map[match(survey$Q8_10_a, skills_map[,1]),2], ordered=TRUE)

skills_df <- data.frame(delonemclean = delonemclean_score,
                        proj_mgmt_rep, accounting_rep, communications_rep,
                        accessibility_rep, negotiation_rep, UX_rep, design_rep,
                        webdev_rep, dev_rep, sysadmin_rep)


# Output box and whiskers plots of project succes score in relation
# to each of the identified skills.

proj_mgmpt_rep_box <- ggplot(na.omit(skills_df), aes(x=proj_mgmt_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                     labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                       labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Project Management\" in procurement team")
ggsave(paste(plotspath,"proj_mgmpt_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"proj_mgmpt_rep_box.emf", sep = "/"))
show(proj_mgmpt_rep_box)
dev.off()

accounting_rep_box <- ggplot(na.omit(skills_df), aes(x=accounting_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Accounting/costing/budgeting\" in procurement team")
ggsave(paste(plotspath,"accounting_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"accounting_rep_box.emf", sep = "/"))
show(accounting_rep_box)
dev.off()

communications_rep_box <- ggplot(na.omit(skills_df), aes(x=communications_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Communications/outreach\" in procurement team")
ggsave(paste(plotspath,"communications_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"communications_rep_box.emf", sep = "/"))
show(communications_rep_box)
dev.off()

accessibility_rep_box <- ggplot(na.omit(skills_df), aes(x=accessibility_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Accessibility assessment\" in procurement team")
ggsave(paste(plotspath,"accessibility_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"accessibility_rep_box.emf", sep = "/"))
show(accessibility_rep_box)
dev.off()

negotiation_rep_box <- ggplot(na.omit(skills_df), aes(x=negotiation_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Negotiation\" in procurement team")
ggsave(paste(plotspath,"negotiation_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"negotiation_rep_box.emf", sep = "/"))
show(negotiation_rep_box)
dev.off()

communications_rep_box <- ggplot(na.omit(skills_df), aes(x=communications_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Communications/outreach\" in procurement team")
ggsave(paste(plotspath,"communications_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"communications_rep_box.emf", sep = "/"))
show(communications_rep_box)
dev.off()

UX_rep_box <- ggplot(na.omit(skills_df), aes(x=UX_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"User experience design\" in procurement team")
ggsave(paste(plotspath,"UX_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"UX_rep_box.emf", sep = "/"))
show(UX_rep_box)
dev.off()

design_rep_box <- ggplot(na.omit(skills_df), aes(x=design_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Graphic design\" in procurement team")
ggsave(paste(plotspath,"design_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"design_rep_box.emf", sep = "/"))
show(design_rep_box)
dev.off()

webdev_rep_box <- ggplot(na.omit(skills_df), aes(x=webdev_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Web design\" in procurement team")
ggsave(paste(plotspath,"webdev_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"webdev_rep_box.emf", sep = "/"))
show(webdev_rep_box)
dev.off()

dev_rep_box <- ggplot(na.omit(skills_df), aes(x=dev_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"Computer programming\" in procurement team")
ggsave(paste(plotspath,"dev_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"dev_rep_box.emf", sep = "/"))
show(dev_rep_box)
dev.off()

sysadmin_rep_box <- ggplot(na.omit(skills_df), aes(x=sysadmin_rep, y=delonemclean)) +
  geom_boxplot() +
  xlab("Skill represented on project team") +
  ylab("Project success score [DeLone & McLean]") +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("No", "Yes")) +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  ggtitle("Skill \"System administration\" in procurement team")
ggsave(paste(plotspath,"sysadmin_rep_box.png", sep = "/"))

emf(file=paste(plotspath,"sysadmin_rep_box.emf", sep = "/"))
show(sysadmin_rep_box)
dev.off()


# Analysis of variance (ANOVA) of presence of skills on project success score.
# cf Wikipedia (https://en.wikipedia.org/wiki/Analysis_of_variance):
# "ANOVAs are useful for comparing (testing) three or more means (groups or variables)
# for statistical significance.
# It is conceptually similar to multiple two-sample t-tests, but is more conservative
# (results in less type I error).

skills_fit <- lm(delonemclean ~ proj_mgmt_rep + accounting_rep + communications_rep +
                  accessibility_rep + negotiation_rep + UX_rep + design_rep +
                  webdev_rep + dev_rep + sysadmin_rep, data=skills_df)
print("ANOVA of presence of skills on project success score:")
print(anova(skills_fit))
print("Summary:")
print(summary(skills_fit))

# Next, investigate whether the procurement type had an impact on projet success
procurement_df <- data.frame(delonemclean = delonemclean_score, proctype = survey$Q6)
procurement_impact_box <- ggplot(na.omit(procurement_df), aes(x=proctype, y=delonemclean, group=proctype)) + 
  geom_boxplot() +
  xlab("Procurement method employed") +
  ylab("Project success score [DeLone & McLean]") +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1.0\nUnsuccessful","0.5","0.0","0.5","Successful\n1.0")) +
  scale_x_continuous(breaks=c(1,2,3,4,5),
                   labels=c("Formal/RFP", "Competitive", "Noncompetitive", "In-house", "Other")) +
  ggtitle("Procurement method employed")
ggsave(paste(plotspath,"procurement_impact_box.pdf", sep = "/"), width=10,height=5)

emf(file=paste(plotspath,"procurement_impact_box.emf", sep = "/"))
show(procurement_impact_box)
dev.off()
