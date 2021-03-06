# This script takes the CSV file exported from BOS with the survey data

# Specify which file to open
infile <- "data/results-for-investigating-2016-08-29-0052.csv"

# And where to store results
plotspath <- "plots"
tablespath <- "tables"


# Read survey data
survey <- read.csv(infile)

# Replace the long names of the columns by just the question codes
# E.g. "X1..My.question" is replaced by "X1" in the column names.
names(survey) <- gsub("^(.*?)\\.\\.(.*)", "\\1", colnames(survey))
# TO DO: salvage the full question names somewhat
# so as not to depend on survey keys for question names.

# Create a Word document for analyzed data (to copy-paste into thesis)
library(rtf)
rtffile <- RTF(paste(tablespath,"demographics.doc", sep = "/"))
addHeader(rtffile, "Survey demographics", subtitle = paste("Created:", Sys.time(), "from file:", infile))

# Output total number of responses
survey_tot <- nrow(survey)
print(paste('Total number of responses:', survey_tot))
addParagraph(rtffile, paste('Total number of responses:', survey_tot))



# Load the ggplot2 library, for plotting results.
library("ggplot2")

# This displays a bar graph of the library type distribution.
# Not very useful, replace it by something better.
#ggplot(data=survey, aes(x = X1)) + geom_bar(stat="count") + coord_flip() + ylab("Number of responses") + ggtitle("Figure 1: Responses by type of library")

survey_demographics <- survey[c(1,2,4)]
# This is better, a simple contingency table across library type and size:
demog_table <- with(survey_demographics,table(X1,X2, dnn=c("Type of institution", "Total staff (FTE)")))

# Add totals and percentage
demog_results <- data.frame(matrix(demog_table, nrow=nrow(demog_table)))
colnames(demog_results) = dimnames(demog_table)[[2]]
rownames(demog_results) = dimnames(demog_table)[[1]]
demog_results$Total = rowSums(demog_results)
demog_results$Percentage = round(demog_results$Total/sum(demog_results$Total)*100)
results_row <- nrow(demog_results)+1
demog_results[results_row,] <- colSums(demog_results)
rownames(demog_results)[results_row] <- "Total"

# TO FIX: Add proper labels for type and size of institution
# Also sort by percentage
addParagraph(rtffile, "Survey demographics:")
addTable(rtffile, cbind(rownames(demog_results), demog_results))

print("Response demographics:")
print(demog_results)

# Extract base competencies data
survey_basecomp = survey[6:15]
basecomp_results <- t(sapply(survey_basecomp, function(x) table(factor(x, levels=levels(unlist(survey_basecomp))))))
rownames(basecomp_results) <- c("Project Management", "Accounting/costing/budgeting", "Communication/outreach", "Accessibility assessment", "Negotiation", "User Experience design", "Graphic design", "Web design", "Computer programming", "System admin.")
print(basecomp_results)

addParagraph(rtffile, "Base competencies:")
addTable(rtffile, cbind(rownames(basecomp_results), basecomp_results))
# Close and write out the RTF file
done(rtffile)

# This is the ugliest code ever
# Either switch to Excel or learn proper R
# See this for hints https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
# Also my legend is not printing :(
df2 <- data.frame(basecomp_results)
basecomp_plot <- ggplot() + geom_bar(data=df2, aes(x=rownames(df2),y=survey_tot), 
                    stat="identity", fill="#999999") +
                 geom_bar(data=df2, aes(x=rownames(df2),y=Well.represented+Somewhat.represented+Not.well.represented),
                          stat="identity", fill="#009E73") + 
                 geom_bar(data=df2, aes(x=rownames(df2),y=Somewhat.represented+Not.well.represented), 
                         stat="identity", fill="#56B4E9") + 
                 geom_bar(data=df2, aes(x=rownames(df2),y=Not.well.represented), 
                          stat="identity", fill="#E69F00") + 
                 coord_flip() + 
                 xlab("Competencies present amongst library staff") + 
                 ylab("Responses") + 
                geom_text(data=df2, aes(x=rownames(df2),y=Not.well.represented/2, 
                                        label=paste(round(Not.well.represented/survey_tot*100),"%"))) + 
                geom_text(data=df2, aes(x=rownames(df2),y=Not.well.represented+(Somewhat.represented/2), 
                                        label=paste(round(Somewhat.represented/survey_tot*100),"%"))) +
                geom_text(data=df2, aes(x=rownames(df2),y=Not.well.represented+Somewhat.represented+(Well.represented/2), 
                          label=paste(round(Well.represented/survey_tot*100),"%"))) +
                scale_fill_manual(name="Legend", values = c("Not well represented"="#E69F00", "Somewhat represented"="#56B4E9", "Well represented"="#009E73", "Don't know - N/A"="#999999"))
ggsave(paste(plotspath,"baseline_competencies.png", sep = "/"))               

# Save graph as EMF format for import into MS Word :poop:
library("devEMF")
emf(file=paste(plotspath,"baseline_competencies.emf", sep = "/"))
show(basecomp_plot)
dev.off()

