# Read survey data
survey <- read.csv("data/results-for-investigating-2016-08-13-2209.csv")

# Replace the long names of the columns by just the question codes
# E.g. "X1..My.question" is replaced by "X1" in the column names.
names(survey) <- gsub("^(.*?)\\.\\.(.*)", "\\1", colnames(survey))

print(paste('Total number of responses: ', nrow(survey)))

# Load the ggplot2 library, for plotting results.
library("ggplot2")

# This displays a bar graph of the library type distribution.
# Not very useful, replace it by something better.
#ggplot(data=survey, aes(x = X1)) + geom_bar(stat="count") + coord_flip() + ylab("Number of responses") + ggtitle("Figure 1: Responses by type of library")

# This is better, a simple contingency table across library type and size:
print("Response demographics:")
with(survey_demographics,table(X1,X2, dnn=c("Type of institution", "Total staff (FTE)")))
