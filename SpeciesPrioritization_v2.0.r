#(c) Torsten Struck 06.03.2023, modified 06.10.2023 for round 2

### !!! IMPORTANT NOTE !!! ### !!! IMPORTANT NOTE !!! ### !!! IMPORTANT NOTE !!! ###
# Set working directory to the folder where you want all your results to be saved. #
####################################################################################

#Libraries needed
library(readxl)
library(writexl)
library(stringr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(pheatmap)

setwd("~/000_changed_documents_20230810/Analyses/SpeciesPrioritization/Round2")

#create result directories in the working folder
print("Generating directories")
dir.create("Species_IDs")
dir.create("Actual_weights")
dir.create("PieCharts")
dir.create("Species_lists")
dir.create("Enrichment")

#open necessary files
print("Opening files")
Inputfile <- readline("Please provide the name of the input file: ")
SpeciesNominations_all <- read_excel(Inputfile)
ITC <- read.csv("Widening_countries.txt")
Cou_selected <- read.csv("Countries_selected.txt") ### countries selected in previous rounds 
Ind_selected <- read.csv("Individuals_selected.txt") ### individuals (email) selected in previous rounds
cat("Date and time started:",file="Configuration.txt",sep="\n")
cat(date(),file="Configuration.txt",sep="\n",append=TRUE)
cat("Input file:",file="Configuration.txt",sep="\n",append=TRUE)
cat(Inputfile,file="Configuration.txt",sep="\n",append=TRUE)
cat("Priority countries are in Widening_countries.txt.",file="Configuration.txt",sep="\n",append=TRUE)
cat("Countries already selected are in Countries_selected.txt.",file="Configuration.txt",sep="\n",append=TRUE)
cat("Individual researchers already selected are selected in Individuals_selected.txt.",file="Configuration.txt",sep="\n",append=TRUE)

######################################################
###                                                ###
###               Exclusion (stage 1)              ###
###                                                ###
######################################################

#subset dataset based on exclusion criteria
print("Subsetting files and generating addional columns")
SpeciesNominations <- subset(SpeciesNominations_all, Ex1 == 'No' & Ex2 == 'No' & Ex3 == 'Yes')

######################################################
###                                                ###
###             Prioritization (stage 2)           ###
###                                                ###
######################################################

#check if country occurs in Widening country list
SpeciesNominations$Cou8_ITC <- ifelse(SpeciesNominations$Cou8 %in% ITC$ISO, "Yes", "No")

#procedure for first round and following rounds which have already assigned species to countries
Round1 <- readline("Is this first round of species selection (yes/no): ")

#generate the list of countries from which only one species is suggested (only needed for round 1) 
if (Round1 == "yes") {
  Cou_counted <- data.frame(table(SpeciesNominations$Cou7a))
  Cou_counted_single <- Cou_counted[Cou_counted$Freq == 1, ]
  SpeciesNominations$Cou7 <- ifelse(SpeciesNominations$Cou7a %in% Cou_counted_single$Var1, "Yes", "No")
} else if (Round1 == "no") {
  SpeciesNominations$Cou7 <- ifelse(SpeciesNominations$Cou7a %in% Cou_selected$ISO, "No", "Yes")
} else {
 stop("Input: ",Round1," -> Only yes or no are allowed as input.") 
}

#generate list of individuals already selected in the previous rounds (in the first round these will all "no")
SpeciesNominations$Ind <- ifelse(SpeciesNominations$Email %in% Ind_selected$Individual, "No", "Yes")
write.csv(SpeciesNominations, file = "CommunityNomination_AfterExclusion.csv")
write_xlsx(SpeciesNominations, "CommunityNomination_AfterExclusion.xlsx")

#assigning the values to the different criteria
print("Assign values for criteria")
Tax <- ifelse((SpeciesNominations$Tax6 == "Genus"), 1, 
     +    ifelse((SpeciesNominations$Tax6 == "Family"), 2,
     +    ifelse((SpeciesNominations$Tax6 == "Order"), 3,
     +    ifelse((SpeciesNominations$Tax6 == "Class"), 4, 5))))
Cou <- ifelse((SpeciesNominations$Cou8_ITC == "No") & (SpeciesNominations$Cou7 == "No"), 1, 
     +    ifelse((SpeciesNominations$Cou8_ITC == "Yes") & (SpeciesNominations$Cou7 == "No"), 4, 
     +    ifelse((SpeciesNominations$Cou8_ITC == "Yes") & (SpeciesNominations$Cou7 == "Yes") & (SpeciesNominations$Cou7b == "No"), 4, 5)))
Cer <- ifelse((SpeciesNominations$Cer5 == "Collection not close to type locality") & (SpeciesNominations$Cer4a == "Known taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") < 2), 1, 
     +    ifelse((SpeciesNominations$Cer5 == "Collection at or close to type locality") & (SpeciesNominations$Cer4a == "No taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") < 2), 2, 
     +    ifelse((SpeciesNominations$Cer5 == "Collection at or close to type locality") & (SpeciesNominations$Cer4a == "Known taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") > 1), 2,
     +    ifelse((SpeciesNominations$Cer5 == "Type locality not known or not yet attributed") & (SpeciesNominations$Cer4a == "No taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") < 2), 2, 
     +    ifelse((SpeciesNominations$Cer5 == "Type locality not known or not yet attributed") & (SpeciesNominations$Cer4a == "Known taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") > 1), 2,
     +    ifelse((SpeciesNominations$Cer5 == "Collection not close to type locality") & (SpeciesNominations$Cer4a == "No taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") < 2), 2, 
     +    ifelse((SpeciesNominations$Cer5 == "Collection not close to type locality") & (SpeciesNominations$Cer4a == "Known taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") > 1), 2,
     +    ifelse((SpeciesNominations$Cer5 == "Collection not close to type locality") & (SpeciesNominations$Cer4a == "No taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") > 1), 5,
     +    ifelse((SpeciesNominations$Cer5 == "Type locality not known or not yet attributed") & (SpeciesNominations$Cer4a == "No taxonomic problems") & (count.fields(textConnection(SpeciesNominations$Cer4b), sep = ",") > 1), 5, 6)))))))))
Nov <- ifelse((SpeciesNominations$Nov9 == "No"), 0, 2)
JEDI <- ifelse((SpeciesNominations$JEDI10 == "Male") & (SpeciesNominations$JEDI11 == "Do not identify as an underrepresented minority") & (SpeciesNominations$JEDI12 == "\"Genome team is purely scientific (i.e., not fulfilling one of the four criteria above)\""), 1, 
     +    ifelse((SpeciesNominations$JEDI10 != "Male") & (SpeciesNominations$JEDI11 == "Do not identify as an underrepresented minority") & (SpeciesNominations$JEDI12 == "\"Genome team is purely scientific (i.e., not fulfilling one of the four criteria above)\""), 2, 
     +    ifelse((SpeciesNominations$JEDI10 == "Male") & (SpeciesNominations$JEDI11 != "Do not identify as an underrepresented minority") & (SpeciesNominations$JEDI12 == "\"Genome team is purely scientific (i.e., not fulfilling one of the four criteria above)\""), 2, 
     +    ifelse((SpeciesNominations$JEDI10 == "Male") & (SpeciesNominations$JEDI11 == "Do not identify as an underrepresented minority") & (SpeciesNominations$JEDI12 != "\"Genome team is purely scientific (i.e., not fulfilling one of the four criteria above)\""), 2,  
     +    ifelse((SpeciesNominations$JEDI10 != "Male") & (SpeciesNominations$JEDI11 != "Do not identify as an underrepresented minority") & (SpeciesNominations$JEDI12 == "\"Genome team is purely scientific (i.e., not fulfilling one of the four criteria above)\""), 3, 
     +    ifelse((SpeciesNominations$JEDI10 != "Male") & (SpeciesNominations$JEDI11 == "Do not identify as an underrepresented minority") & (SpeciesNominations$JEDI12 != "\"Genome team is purely scientific (i.e., not fulfilling one of the four criteria above)\""), 3, 
     +    ifelse((SpeciesNominations$JEDI10 == "Male") & (SpeciesNominations$JEDI11 != "Do not identify as an underrepresented minority") & (SpeciesNominations$JEDI12 != "\"Genome team is purely scientific (i.e., not fulfilling one of the four criteria above)\""), 3,  
     +    ifelse((SpeciesNominations$JEDI10 == "Prefer not to say") & (SpeciesNominations$JEDI11 == "Prefer not to say") & (SpeciesNominations$JEDI12 == "\"Genome team has support from non-scientific interests or organizations (e.g., NGO, indigenous council)\""), 4,  
     +    ifelse((SpeciesNominations$JEDI10 != "Prefer not to say") & (SpeciesNominations$JEDI11 == "Prefer not to say") & (SpeciesNominations$JEDI12 == "\"Genome team has support from non-scientific interests or organizations (e.g., NGO, indigenous council)\""), 4,  
     +    ifelse((SpeciesNominations$JEDI10 == "Prefer not to say") & (SpeciesNominations$JEDI11 != "Prefer not to say") & (SpeciesNominations$JEDI12 == "\"Genome team has support from non-scientific interests or organizations (e.g., NGO, indigenous council)\""), 4, 
     +    ifelse((SpeciesNominations$JEDI10 == "Prefer not to say") & (SpeciesNominations$JEDI11 == "Prefer not to say") & (SpeciesNominations$JEDI12 != "\"Genome team has support from non-scientific interests or organizations (e.g., NGO, indigenous council)\""), 4, 5)))))))))))   
App <- ifelse((SpeciesNominations$App13 != "Less than a year") & (SpeciesNominations$App14 < 3), 0, 
     +    ifelse((SpeciesNominations$App13 != "Less than a year") & (SpeciesNominations$App14 > 2), 1, 
     +    ifelse((SpeciesNominations$App13 == "Less than a year") & (SpeciesNominations$App14 < 3), 1, 2)))

#compiling the individual categories plus the species ID into one data frame and write out the table
Categories <- data.frame(as.numeric(SpeciesNominations$Species_ID), Tax, Cou, Cer, JEDI, Nov, App)
Categories <- replace(Categories, is.na(Categories), 1)
colnames(Categories)[1] ="Species_ID"
write.csv(Categories, file = "Actual_weights/AllSpecies_not_sorted.csv")

#generate histograms of the distribution of scores
print("Generating histrograms of scores")
for (col in c("Tax","Cou","Cer","Nov","JEDI","App")) {
  outhisto = paste0("Actual_weights/Histo_", col, "_Scores.pdf")
  var_histo <- ggplot(Categories, aes(x=Categories[[col]])) +
    geom_histogram(binwidth=0.5) +
    ggtitle(col)
  ggarrange(var_histo, ncol = 1, nrow = 1)
  ggsave(file = outhisto, width = 14,  height = 14, units = "cm", device = "pdf")
}


#calculate average value for the entire dataset
Ave_Categories <- apply(Categories, 2, mean)
avevar <- paste0("Comp_Average_values")
Ave_Categories_df <- as.data.frame(Ave_Categories)
colnames(Ave_Categories_df)[1] = "All"
{assign(avevar, cbind(Ave_Categories_df))}

#generate pie charts of different aspects for all species
print("Generating pie charts for all species")
Email_counted <- data.frame(table(SpeciesNominations$Email))
Email_pie <- ggplot(Email_counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      ggtitle("Individuum") +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size = 5),
            legend.position = "none", 
            panel.background = element_rect(fill = "white"))
ggarrange(Email_pie, ncol = 1, nrow = 1)
ggsave(file = "PieCharts/Pies_Individuum_all_species.pdf", width = 56,  height = 56,  units = "cm", device = "pdf")
for (col in c("Phylum","Cer4b","Cer4a","Cer5","Tax6","Cou7","Cou7a","Cou7b","Cou8","Cou8_ITC","Nov9","JEDI10","JEDI11","JEDI12","App13")) {
outpie = paste0("PieCharts/Pies_", col, "_allSpecies.pdf")
counted <- data.frame(table(SpeciesNominations[[col]]))
var_pie <- ggplot(counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  ggtitle(col) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5),
        legend.position = "right", 
        panel.background = element_rect(fill = "white"))
ggarrange(var_pie, ncol = 1, nrow = 1)
ggsave(file = outpie, width = 56,  height = 56, units = "cm", device = "pdf")
}

#maximally possible weights for each category
Max_V <- c(0, 5, 5, 6, 5, 2, 2)
Max_V_df <- as.data.frame(Max_V)
colnames(Max_V_df)[1] = "Maximum"
{assign(avevar, cbind(eval(parse(text = avevar)), Max_V_df))}

#Define the models to be tested and applied to the dataset with only the scores
print("Assigning models")
Model1 <- function() {Categories[order(-Categories$Tax,-Categories$Cou,-Categories$Cer,-Categories$JEDI,-Categories$Nov,-Categories$App),]}
Model2 <- function() {Categories[order(-Categories$Tax,-Categories$Cer,-Categories$Cou,-Categories$JEDI,-Categories$App,-Categories$Nov),]}
Model3 <- function() {Categories[order(-Categories$Tax,-Categories$Cou,-Categories$Cer,-Categories$JEDI,-Categories$App,-Categories$Nov),]}
Model4 <- function() {Categories[order(-Categories$Tax,-Categories$Cer,-Categories$Cou,-Categories$JEDI,-Categories$Nov,-Categories$App),]}
Model5 <- function() {Categories[order(-Categories$Tax,-(rowSums(Categories[ , c("Cou","Cer")])),-Categories$JEDI,-Categories$Nov,-Categories$App),]}
Model6 <- function() {Categories[order(-Categories$Tax,-(rowSums(Categories[ , c("Cou","Cer")])),-Categories$JEDI,-Categories$App,-Categories$Nov),]}
Model7 <- function() {Categories[order(-Categories$Tax,-(rowSums(Categories[ , c("Cou","Cer")])),-Categories$JEDI,-(rowSums(Categories[ , c("Nov","App")]))),]}
Model8 <- function() {Categories[order(-(rowSums(Categories[,2:7]))),]}

Count_First_Row = 1
#apply models to the data without email and country
for (m in 1:8) {
  print(paste("Working on model",m))
  model <- paste0("Model", m)
  dataset <- paste0("Categories_Model", m)
  outfile_all_species = paste0("Actual_weights/AllSpecies_sorted_Model", m, ".csv")
  outfile_ranked_species = paste0("Species_lists/AllSpecies_sorted_Model", m, ".csv")
  {assign(dataset, eval(parse(text = model)) ())}
  write.csv({assign(dataset, eval(parse(text = model)) ())}, file = outfile_all_species)
  SpeciesNominations2 <- SpeciesNominations[match({eval(parse(text = dataset))}$Species_ID,SpeciesNominations$Species_ID),]
  write.csv(SpeciesNominations2, file = outfile_ranked_species)
  #select the top 25, 50 and 75 species and calculate the relative difference
  for (t in c(25,50,75)) {
    print(paste("Selecting",t,"species"))
    Count_First_Row = (Count_First_Row - 1)
    #generate output variable
    dataset_top <- paste0("Categories_Model", m, "_Top", t)
    outvar <- paste0("Comp_Diff_Model")
    model_top <- paste0("Model", m, "_Top", t)
    outfile_top_values = paste0("Actual_weights/SelectedSpecies_Model", m, "_Top", t, ".csv")
    outfile_top_species = paste0("Species_lists/SelectedSpecies_Model", m, "_Top", t, ".xlsx")
    outpie_Email_top_species = paste0("PieCharts/Pies_Individuum_SelectedSpecies_Model", m, "_Top", t, ".pdf")
    outbar_top_species = paste0("Enrichment/BarPlot_SelectedSpecies_Model", m, "_Top", t, ".pdf")

    #select the top species
    {assign(dataset_top, head(eval(parse(text = dataset)),t))}
    write.csv({eval(parse(text = dataset_top))}, file = outfile_top_values)
    #subset SpeciesNominations based on Species_ID in top-selected datasets
    top_selected_species <- head(SpeciesNominations2,t)
    write_xlsx(top_selected_species, outfile_top_species)

	  #generate histograms of the distribution of scores for the top-selected species
    print("Generating histograms")
	  for (col in c("Tax","Cou","Cer","Nov","JEDI","App")) {
		  outhisto = paste0("Actual_weights/Histo_", col, "_Scores_Model", m, "_Top", t, ".pdf")
		  var_histo <- ggplot({eval(parse(text = dataset_top))}, aes(x={eval(parse(text = dataset_top))}[[col]])) +
		  geom_histogram(binwidth=0.5) +
		  ggtitle(col)
	  ggarrange(var_histo, ncol = 1, nrow = 1)
	  ggsave(file = outhisto, width = 14,  height = 14, units = "cm", device = "pdf")
	  }

	  #generate pie charts of different aspects for all species
    print("Generating pie charts")
	  Email_counted <- data.frame(table(top_selected_species$Email))
	  Email_pie <- ggplot(Email_counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
	  	geom_col(width = 1, color = 1) +
		  coord_polar(theta = "y") +
		  ggtitle("Individuum") +
		  theme(axis.ticks = element_blank(),
			  axis.title = element_blank(),
			  axis.text = element_text(size = 5),
			  legend.position = "none", 
			  panel.background = element_rect(fill = "white"))
	  ggarrange(Email_pie, ncol = 1, nrow = 1)
	  ggsave(file = outpie_Email_top_species, width = 56,  height = 56,  units = "cm", device = "pdf")
	  for (col in c("Phylum","Cer4b","Cer4a","Cer5","Tax6","Cou7","Cou7a","Cou7b","Cou8","Cou8_ITC","Nov9","JEDI10","JEDI11","JEDI12","App13")) {
		  outpie = paste0("PieCharts/Pies_", col, "_SelectedSpecies_Model", m, "_Top", t, ".pdf")
		  counted <- data.frame(table(top_selected_species[[col]]))
		  var_pie <- ggplot(counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
			  geom_col(width = 1, color = 1) +
			  coord_polar(theta = "y") +
			  ggtitle(col) +
			  theme(axis.ticks = element_blank(),
				  axis.title = element_blank(),
				  axis.text = element_text(size = 5),
				  legend.position = "right", 
				  panel.background = element_rect(fill = "white"))
		  ggarrange(var_pie, ncol = 1, nrow = 1)
		  ggsave(file = outpie, width = 56,  height = 56, units = "cm", device = "pdf")
	  }

    #select calculate the average of the top species
    Ave_Model <- apply(eval(parse(text = dataset_top)), 2, mean)
    Ave_Model_df <- as.data.frame(Ave_Model)
    colnames(Ave_Model_df)[1] = model_top
    {assign(avevar, cbind(eval(parse(text = avevar)), Ave_Model_df))}

    #calculate the difference of the average values of the top selected species to the entire data set relative to the maximal positive enrichment
    Diff_Model <- (Ave_Model - Ave_Categories)/(Max_V - Ave_Categories)
    Diff_Model_df <- as.data.frame(Diff_Model)
    colnames(Diff_Model_df)[1] = model_top

    #compile results of replicates into one dataset
    if (Count_First_Row == 0) {assign(outvar, cbind(Diff_Model_df))}
    else {assign(outvar, cbind(eval(parse(text = outvar)), Diff_Model_df))}

    #generate barplots of relative enrichment
    print("Generating barplots of enrichment")
    Diff_Model_df_red <- data.frame(Diff_Model_df[2:7, , drop = FALSE])
    Diff_Model_Bar <- ggplot(data=Diff_Model_df_red, aes(rownames(Diff_Model_df_red), Diff_Model_df_red[,1], fill=rownames(Diff_Model_df_red))) +
      geom_bar(stat="identity") +
      xlab("Category") + ylab("Relative Enrichment")
    ggarrange(Diff_Model_Bar, ncol = 1, nrow = 1)
    ggsave(file = outbar_top_species, width = 28,  height = 21,  units = "cm", device = "pdf")
  }
}

#write out the compiled measurement per selected set of species
print("Writing out data")
Comp_Diff_Model <- as.data.frame(t(Comp_Diff_Model))
Comp_Diff_Model <- Comp_Diff_Model[,-1]
write.csv(Comp_Diff_Model, file = "Enrichment/Enrichment_Categories_selected_species.csv")
Comp_Average_values <- as.data.frame(t(Comp_Average_values))
Comp_Average_values <- Comp_Average_values[,-1]
write.csv(Comp_Average_values, file = "Enrichment/AverageValues_SelectedSpecies_all_models_tops.csv")

#calculate heatmap of the relative enrichment
print("Generating heatmaps")
#dev.off()
pdf(file = "Enrichment/Heatmap_all_models_tops.pdf", width = 2, height = 3)
pheatmap(as.matrix(Comp_Diff_Model), cluster_rows = TRUE, treeheight_row = 15, treeheight_col = 15, fontsize = 4, fontsize_row = 4)
dev.off()

#calculate overlap for each set of top-select and applied model
for (t in c(25,50,75)) {
  outdata_ids <- paste0("SpeciesIDs_Top", t)
  outfile_ids_top = paste0("Species_IDs/SelectedIDs_all_models_Top", t, ".csv")
  Count_First_Row = 1
  for (m in 1:8) {
    Count_First_Row = (Count_First_Row - 1)
    indata <- paste0("Categories_Model", m, "_Top", t)
    indata_df <- as.data.frame(eval(parse(text = indata)))
    if (Count_First_Row == 0) {assign(outdata_ids, cbind(indata_df$Species_ID))}
    else {assign(outdata_ids, cbind(eval(parse(text = outdata_ids)), indata_df$Species_ID))}
  }
  Species_IDs <- as.data.frame(eval(parse(text = outdata_ids)))
  colnames(Species_IDs) = c("model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8")
  write.csv(Species_IDs, file = outfile_ids_top)
  data_intersect_ids <- paste0("SpeciesIDs_Intersect_Top", t)
  outfile_intersect_ids <- paste0("Species_IDs/SpeciesIDs_Intersect_all_models_Top", t, ".csv")
  Count_First_Row2 = 1
  for (m in 1:8) {
    Count_First_Row2 = (Count_First_Row2 - 1)
    Combined = c()
    for (n in 1:8) {
      Intersect <- (length(intersect(Species_IDs[,m],Species_IDs[,n]))/t)
      Combined <- cbind(Combined,Intersect)
    }
    if (Count_First_Row2 == 0) {assign(data_intersect_ids, rbind(Combined))}
    else {assign(data_intersect_ids, rbind(eval(parse(text = data_intersect_ids)), Combined))}
  }
  Intersect_IDs <- as.data.frame(eval(parse(text = data_intersect_ids)))
  colnames(Intersect_IDs) = c("model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8")
  row.names(Intersect_IDs) = c("model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8")
  write.csv(Intersect_IDs, file = outfile_intersect_ids)
  pdf_file = paste0("Species_IDs/Heatmap_SpeciesIDs_Intersect_all_models_Top", t,".pdf")
  pdf(file = pdf_file, width = 2, height = 3)
  pheatmap(as.matrix(Intersect_IDs), cluster_rows = TRUE, treeheight_row = 15, treeheight_col = 15, fontsize = 4, fontsize_row = 4)
  dev.off()
}

###############################################################
###                                                         ###
### Additional sorting by country and individual researcher ###
###                                                         ###
###############################################################


#apply models to the data and the filter the based on country and e-mail
#add email and country columns for later additional sorting on individuals and countries
print("Additional sorting based on countries and individual researchers")
Categories <- data.frame(cbind(Categories, SpeciesNominations$Cou7a, SpeciesNominations$Cou7, SpeciesNominations$Email, SpeciesNominations$Ind))
colnames(Categories)[8] ="Cou7a"
colnames(Categories)[9] ="Cou7"
colnames(Categories)[10] ="Email"
colnames(Categories)[11] ="Ind"

#prepare database for average values of the categories
avevar_filter <- paste0("Comp_Average_values_filtered")
{assign(avevar_filter, cbind(Ave_Categories_df))}
{assign(avevar_filter, cbind(eval(parse(text = avevar_filter)), Max_V_df))}

#create new result directories in the working folder for the filter data sets
print("Generating output folders")
dir.create("Filtered")
dir.create("Filtered/Species_IDs")
dir.create("Filtered/Actual_weights")
dir.create("Filtered/PieCharts")
dir.create("Filtered/Species_lists")
dir.create("Filtered/Enrichment")

Count_First_Row = 1
#apply models to the data then keep the top 10, sort by country one for each, keep all with ar least one and then one per individual
for (m in 1:8) {
  print(paste("Working on model",m))
  model <- paste0("Model", m)
  dataset_filter <- paste0("Categories_Model", m, "_filtered")
  outfile_all_species = paste0("Filtered/Actual_weights/AllSpecies_sorted_Model", m, "_filtered.csv")
  outfile_ranked_species = paste0("Filtered/Species_lists/AllSpecies_sorted_Model", m, ".csv")
  {assign(dataset_filter, eval(parse(text = model)) ())}
  
  #filtering based on country and individual researchers
  CatFilter <- eval(parse(text = dataset_filter))
  #indicate the duplicates as 0
  Cou7afirst <- as.integer(!duplicated(CatFilter$Cou7a))
  Indfirst <- as.integer(!duplicated(CatFilter$Email))
  #bind the new columns to the dataset
  CatFilter <- data.frame(cbind(CatFilter, Cou7afirst, Indfirst))
  #indicate all countries and individuals selected before as 0
  CatFilter$Cou7afirst[CatFilter$Cou7 == "No" & CatFilter$Cou7afirst == 1] <- 0
  CatFilter$Indfirst[CatFilter$Ind == "No" & CatFilter$Indfirst == 1] <- 0
  #keep the first 10 selected species
  CatFilter_Top <- CatFilter[1:10,]
  #sort the remaining species based on country first occurrence, individual first occurrence and then the old order
  CatFilter_Country <- CatFilter[11:nrow(CatFilter),]
  CatFilter_Country_sorted <- CatFilter_Country[order(-CatFilter_Country$Cou7afirst,-CatFilter_Country$Indfirst),]
  #bind the top-selected species and the newly sorted species together
  CatFilter_Sorted <- data.frame(rbind(CatFilter_Top, CatFilter_Country_sorted))
  CatFilter_Sorted <- CatFilter_Sorted[, -(8:13)]
  #write out results and match the new order to the data file
  {assign(dataset_filter, CatFilter_Sorted)}
  write.csv(CatFilter_Sorted, file = outfile_all_species)
  SpeciesNominations2 <- SpeciesNominations[match(CatFilter_Sorted$Species_ID,SpeciesNominations$Species_ID),]
  write.csv(SpeciesNominations2, file = outfile_ranked_species)
  
  #select the top 25, 50 and 75 species and calculate the relative difference
  for (t in c(25,50,75)) {
    print(paste("Selecting",t,"species"))
    Count_First_Row = (Count_First_Row - 1)
    #generate output variable
    dataset_filter_top <- paste0("Categories_Model", m, "_filtered_Top", t)
    outvar <- paste0("Comp_Diff_Model_filtered")
    model_top <- paste0("Model", m, "_filtered_Top", t)
    outfile_top_values = paste0("Filtered/Actual_weights/SelectedSpecies_Model", m, "_Top", t, ".csv")
    outfile_top_species = paste0("Filtered/Species_lists/SelectedSpecies_Model", m, "_Top", t, ".xlsx")
    outpie_Email_top_species = paste0("Filtered/PieCharts/Pies_Individuum_SelectedSpecies_Model", m, "_Top", t, ".pdf")
    outbar_top_species = paste0("Filtered/Enrichment/BarPlot_SelectedSpecies_Model", m, "_Top", t, ".pdf")
    
    #select the top species
    {assign(dataset_filter_top, head(eval(parse(text = dataset_filter)),t))}
    write.csv({eval(parse(text = dataset_filter_top))}, file = outfile_top_values)
    #subset SpeciesNominations based on Species_ID in top-selected datasets
    top_selected_species <- head(SpeciesNominations2,t)
    write_xlsx(top_selected_species, outfile_top_species)
    
    #generate histograms of the distribution of scores for the top-selected species
    print("Generating histograms")
    for (col in c("Tax","Cou","Cer","Nov","JEDI","App")) {
      outhisto = paste0("Filtered/Actual_weights/Histo_", col, "_Scores_Model", m, "_Top", t, ".pdf")
      var_histo <- ggplot({eval(parse(text = dataset_filter_top))}, aes(x={eval(parse(text = dataset_filter_top))}[[col]])) +
        geom_histogram(binwidth=0.5) +
        ggtitle(col)
      ggarrange(var_histo, ncol = 1, nrow = 1)
      ggsave(file = outhisto, width = 14,  height = 14, units = "cm", device = "pdf")
    }
    
    #generate pie charts of different aspects for all species
    print("Generating pie charts")
    Email_counted <- data.frame(table(top_selected_species$Email))
    Email_pie <- ggplot(Email_counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      ggtitle("Individuum") +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size = 5),
            legend.position = "none", 
            panel.background = element_rect(fill = "white"))
    ggarrange(Email_pie, ncol = 1, nrow = 1)
    ggsave(file = outpie_Email_top_species, width = 56,  height = 56,  units = "cm", device = "pdf")
    for (col in c("Phylum","Cer4b","Cer4a","Cer5","Tax6","Cou7","Cou7a","Cou7b","Cou8","Cou8_ITC","Nov9","JEDI10","JEDI11","JEDI12","App13")) {
      outpie = paste0("Filtered/PieCharts/Pies_", col, "_SelectedSpecies_Model", m, "_Top", t, ".pdf")
      counted <- data.frame(table(top_selected_species[[col]]))
      var_pie <- ggplot(counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        ggtitle(col) +
        theme(axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text = element_text(size = 5),
              legend.position = "right", 
              panel.background = element_rect(fill = "white"))
      ggarrange(var_pie, ncol = 1, nrow = 1)
      ggsave(file = outpie, width = 56,  height = 56, units = "cm", device = "pdf")
    }
    
    #select calculate the average of the top species
    Ave_Model <- apply(eval(parse(text = dataset_filter_top)), 2, mean)
    Ave_Model_df <- as.data.frame(Ave_Model)
    colnames(Ave_Model_df)[1] = model_top
    {assign(avevar_filter, cbind(eval(parse(text = avevar_filter)), Ave_Model_df))}
    
    #calculate the difference of the average values of the top selected species to the entire data set relative to the maximal positive enrichment
    Diff_Model <- (Ave_Model - Ave_Categories)/(Max_V - Ave_Categories)
    Diff_Model_df <- as.data.frame(Diff_Model)
    colnames(Diff_Model_df)[1] = model_top
    
    #compile results of replicates into one dataset
    if (Count_First_Row == 0) {assign(outvar, cbind(Diff_Model_df))}
    else {assign(outvar, cbind(eval(parse(text = outvar)), Diff_Model_df))}
    
    #generate barplots of relative enrichment
    print("Generating bar plots of enrichment")
    Diff_Model_df_red <- data.frame(Diff_Model_df[2:7, , drop = FALSE])
    Diff_Model_Bar <- ggplot(data=Diff_Model_df_red, aes(rownames(Diff_Model_df_red), Diff_Model_df_red[,1], fill=rownames(Diff_Model_df_red))) +
      geom_bar(stat="identity") +
      xlab("Category") + ylab("Relative Enrichment")
    ggarrange(Diff_Model_Bar, ncol = 1, nrow = 1)
    ggsave(file = outbar_top_species, width = 28,  height = 21,  units = "cm", device = "pdf")
  }
}

#write out the compiled measurement per selected set of species
print("Writing out data")
Comp_Diff_Model_filtered <- as.data.frame(t(Comp_Diff_Model_filtered))
Comp_Diff_Model_filtered <- Comp_Diff_Model_filtered[,-1]
write.csv(Comp_Diff_Model_filtered, file = "Filtered/Enrichment/Enrichment_Categories_selected_species.csv")
Comp_Average_values_filtered <- as.data.frame(t(Comp_Average_values_filtered))
Comp_Average_values_filtered <- Comp_Average_values_filtered[,-1]
write.csv(Comp_Average_values_filtered, file = "Filtered/Enrichment/AverageValues_SelectedSpecies_all_models_tops.csv")

#calculate heatmap of the relative enrichment
print("Generating heatmaps")
pdf(file = "Filtered/Enrichment/Heatmap_all_models_tops.pdf", width = 2, height = 3)
pheatmap(as.matrix(Comp_Diff_Model_filtered), cluster_rows = TRUE, treeheight_row = 15, treeheight_col = 15, fontsize = 4, fontsize_row = 4)
dev.off()

#calculate overlap for each set of top-select and applied model
for (t in c(25,50,75)) {
  outdata_ids <- paste0("SpeciesIDs_filtered_Top", t)
  outfile_ids_top = paste0("Filtered/Species_IDs/SelectedIDs_all_models_Top", t, ".csv")
  Count_First_Row = 1
  for (m in 1:8) {
    Count_First_Row = (Count_First_Row - 1)
    indata <- paste0("Categories_Model", m, "_filtered_Top", t)
    indata_df <- as.data.frame(eval(parse(text = indata)))
    if (Count_First_Row == 0) {assign(outdata_ids, cbind(indata_df$Species_ID))}
    else {assign(outdata_ids, cbind(eval(parse(text = outdata_ids)), indata_df$Species_ID))}
  }
  Species_IDs <- as.data.frame(eval(parse(text = outdata_ids)))
  colnames(Species_IDs) = c("model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8")
  write.csv(Species_IDs, file = outfile_ids_top)
  data_intersect_ids <- paste0("SpeciesIDs_Intersect_filtered_Top", t)
  outfile_intersect_ids <- paste0("Filtered/Species_IDs/SpeciesIDs_Intersect_all_models_Top", t, ".csv")
  Count_First_Row2 = 1
  for (m in 1:8) {
    Count_First_Row2 = (Count_First_Row2 - 1)
    Combined = c()
    for (n in 1:8) {
      Intersect <- (length(intersect(Species_IDs[,m],Species_IDs[,n]))/t)
      Combined <- cbind(Combined,Intersect)
    }
    if (Count_First_Row2 == 0) {assign(data_intersect_ids, rbind(Combined))}
    else {assign(data_intersect_ids, rbind(eval(parse(text = data_intersect_ids)), Combined))}
  }
  Intersect_IDs <- as.data.frame(eval(parse(text = data_intersect_ids)))
  colnames(Intersect_IDs) = c("model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8")
  row.names(Intersect_IDs) = c("model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8")
  write.csv(Intersect_IDs, file = outfile_intersect_ids)
  pdf_file = paste0("Filtered/Species_IDs/Heatmap_SpeciesIDs_Intersect_all_models_Top", t,".pdf")
  pdf(file = pdf_file, width = 2, height = 3)
  pheatmap(as.matrix(Intersect_IDs), cluster_rows = TRUE, treeheight_row = 15, treeheight_col = 15, fontsize = 4, fontsize_row = 4)
  dev.off()
}

######################################################
###                                                ###
###           Feasibility check (stage 3)          ###
###                                                ###
######################################################

#open necessary files
print("Starting feasibility check (Stage 3)")
ChosenModel <- readline("Please provide the chosen model (1-8): ")
IndataFeasibility <- paste0("Filtered/Species_lists/AllSpecies_sorted_Model", ChosenModel, ".csv")
SpeciesFeasibility <- read.csv(IndataFeasibility)
SpeciesFeasibility <- SpeciesFeasibility[,-1]
cat("Chosen model:",file="Configuration.txt",sep="\n",append=TRUE)
cat(ChosenModel,file="Configuration.txt",sep="\n",append=TRUE)

#check feasibility of the different criteria with the strict and the loose assessment for sample size
Feasibility <- ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Collected") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1,  
               +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Collected") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Collected") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1,  
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Collected") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
             +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Extracted") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
               +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Extracted") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Extracted") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Extracted") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
             +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "CWA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
               +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "CWA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "CWA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "CWA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
             +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "RA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
               +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "RA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Snap frozen") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "RA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Snap frozen") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 
                 +    ifelse((SpeciesFeasibility$GSize < 6000) & (SpeciesFeasibility$Avail1 == "Not Collected") & (SpeciesFeasibility$Abund == "RA") & (tolower(SpeciesFeasibility$SSize) == tolower("Yes")) & (SpeciesFeasibility$Preserv == "Dry ice") & (SpeciesFeasibility$Transcript == "Dry ice") & (SpeciesFeasibility$Time == "<5 mins") & (tolower(SpeciesFeasibility$Storage) == tolower("70 chain")), 1, 0)))))))))))))))) 
#bind them together with original data to a new dataframe
SpeciesNomFea <- data.frame(SpeciesFeasibility, Feasibility)
write.csv(SpeciesNomFea, file = "SpeciesNominations_Feasibility.csv")

#subset dataframes based on feasibility and sort them into feasible and challenging species
SpeciesFeasible <- subset (SpeciesNomFea, Feasibility == 1)
SpeciesChallenging <- subset (SpeciesNomFea, Feasibility == 0)

#write them to csv files for sharing
print("Writing out feasible and challenging species list")
write.csv(SpeciesFeasible, file = "Species_selected_feasible.csv")
write.csv(SpeciesChallenging, file = "Species_selected_challenging.csv")
write_xlsx(SpeciesFeasible, "Species_selected_feasible.xlsx")
write_xlsx(SpeciesChallenging, "Species_selected_challenging.xlsx")

#generate the same graphics as for species prioritization
#create result directories in the working folder
dir.create("Feasibility")
dir.create("Feasibility/Actual_weights")
dir.create("Feasibility/PieCharts")
dir.create("Feasibility/Enrichment")

#reduce number of columns in categories back to original categories
Categories <- Categories[, -(8:11)]

#calculate average value for the entire dataset
Ave_Categories_fea <- apply(Categories, 2, mean)
avevar_fea <- paste0("Comp_Average_values_fea")
Ave_Categories_fea_df <- as.data.frame(Ave_Categories_fea)
colnames(Ave_Categories_fea_df)[1] = "All"
{assign(avevar_fea, cbind(Ave_Categories_fea_df))}

#maximally possible weights for each category
Max_V_fea <- c(0, 5, 5, 6, 5, 2, 2)
Max_V_fea_df <- as.data.frame(Max_V_fea)
colnames(Max_V_fea_df)[1] = "Maximum"
{assign(avevar_fea, cbind(eval(parse(text = avevar_fea)), Max_V_fea_df))}

#select only feasible species from the categories
print("Generating histograms")
CategoriesFea <- Categories[match(SpeciesFeasible$Species_ID,Categories$Species_ID),]

#generate histograms of the distribution of scores for all feasible species
for (col in c("Tax","Cou","Cer","Nov","JEDI","App")) {
  outhisto = paste0("Feasibility/Actual_weights/Histo_", col, "_Scores_SelectedModel_All_Feasible.pdf")
  var_histo <- ggplot(CategoriesFea, aes(x=CategoriesFea[[col]])) +
    geom_histogram(binwidth=0.5) +
    ggtitle(col)
  ggarrange(var_histo, ncol = 1, nrow = 1)
  ggsave(file = outhisto, width = 14,  height = 14, units = "cm", device = "pdf")
}

#generate pie charts of different aspects for all feasible species
print("Generating pie charts")
Email_counted <- data.frame(table(SpeciesFeasible$Email))
Email_pie <- ggplot(Email_counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  ggtitle("Individuum") +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5),
        legend.position = "none", 
        panel.background = element_rect(fill = "white"))
ggarrange(Email_pie, ncol = 1, nrow = 1)
ggsave(file = "Feasibility/PieCharts/Pies_Individuum_SelectedSpecies_SelectedModel_All_Feasible.pdf", width = 56,  height = 56,  units = "cm", device = "pdf")
for (col in c("Phylum","Cer4b","Cer4a","Cer5","Tax6","Cou7","Cou7a","Cou7b","Cou8","Cou8_ITC","Nov9","JEDI10","JEDI11","JEDI12","App13")) {
  outpie = paste0("Feasibility/PieCharts/Pies_", col, "_SelectedSpecies_SelectedModel_All_Feasible.pdf")
  counted <- data.frame(table(SpeciesFeasible[[col]]))
  var_pie <- ggplot(counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    ggtitle(col) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 5),
          legend.position = "right", 
          panel.background = element_rect(fill = "white"))
  ggarrange(var_pie, ncol = 1, nrow = 1)
  ggsave(file = outpie, width = 56,  height = 56, units = "cm", device = "pdf")
}

#select calculate the average of the feasible species
print("Generating bar plots of enrichment")
Ave_Model_fea <- apply(CategoriesFea, 2, mean)
Ave_Model_fea_df <- as.data.frame(Ave_Model_fea)
colnames(Ave_Model_fea_df)[1] = "all_feasible"
{assign(avevar_fea, cbind(eval(parse(text = avevar_fea)), Ave_Model_fea_df))}

#calculate the difference of the average values of the top selected species to the entire data set relative to the maximal positive enrichment
Diff_Model_fea <- (Ave_Model_fea - Ave_Categories_fea)/(Max_V_fea - Ave_Categories_fea)
Diff_Model_fea_df <- as.data.frame(Diff_Model_fea)
colnames(Diff_Model_fea_df)[1] = "all_feasible"
outvar_fea <- paste0("Comp_Diff_Model_fea")
{assign(outvar_fea, cbind(Diff_Model_fea_df))}

#generate barplots of relative enrichment
Diff_Model_fea_df_red <- data.frame(Diff_Model_fea_df[2:7, , drop = FALSE])
Diff_Model_fea_Bar <- ggplot(data=Diff_Model_fea_df_red, aes(rownames(Diff_Model_fea_df_red), Diff_Model_fea_df_red[,1], fill=rownames(Diff_Model_fea_df_red))) +
  geom_bar(stat="identity") +
  xlab("Category") + ylab("Relative Enrichment")
ggarrange(Diff_Model_fea_Bar, ncol = 1, nrow = 1)
ggsave(file = "Feasibility/Enrichment/BarPlot_SelectedSpecies_SelectedModel_All_Feasible.pdf", width = 28,  height = 21,  units = "cm", device = "pdf")

#select the 50 species and calculate the relative difference
print("Selecting top 50 species")
CategoriesFea_Top <- head(CategoriesFea,50)  
top_selected_species <- head(SpeciesFeasible,50)

#generate histograms of the distribution of scores for the top-selected species
print("Generating histograms")
for (col in c("Tax","Cou","Cer","Nov","JEDI","App")) {
  outhisto = paste0("Feasibility/Actual_weights/Histo_", col, "_Scores_SelectedModel_Top50_Feasible.pdf")
  var_histo <- ggplot(CategoriesFea_Top, aes(x=CategoriesFea_Top[[col]])) +
    geom_histogram(binwidth=0.5) +
    ggtitle(col)
  ggarrange(var_histo, ncol = 1, nrow = 1)
  ggsave(file = outhisto, width = 14,  height = 14, units = "cm", device = "pdf")
}

#generate pie charts of different aspects for all species
print("Generating pie charts")
Email_counted <- data.frame(table(top_selected_species$Email))
Email_pie <- ggplot(Email_counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  ggtitle("Individuum") +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5),
        legend.position = "none", 
        panel.background = element_rect(fill = "white"))
ggarrange(Email_pie, ncol = 1, nrow = 1)
ggsave(file = "Feasibility/PieCharts/Pies_Individuum_SelectedSpecies_SelectedModel_Top50_Feasible.pdf", width = 56,  height = 56,  units = "cm", device = "pdf")
for (col in c("Phylum","Cer4b","Cer4a","Cer5","Tax6","Cou7","Cou7a","Cou7b","Cou8","Cou8_ITC","Nov9","JEDI10","JEDI11","JEDI12","App13")) {
  outpie = paste0("Feasibility/PieCharts/Pies_", col, "_SelectedSpecies_SelectedModel_Top50_Feasible.pdf")
  counted <- data.frame(table(top_selected_species[[col]]))
  var_pie <- ggplot(counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    ggtitle(col) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 5),
          legend.position = "right", 
          panel.background = element_rect(fill = "white"))
  ggarrange(var_pie, ncol = 1, nrow = 1)
  ggsave(file = outpie, width = 56,  height = 56, units = "cm", device = "pdf")
}

#select calculate the average of the top 50 feasible species
print("Generating bar plots of enrichment")
Ave_Model_top <- apply(CategoriesFea_Top, 2, mean)
Ave_Model_top_df <- as.data.frame(Ave_Model_top)
colnames(Ave_Model_top_df)[1] = "top50_feasible"
{assign(avevar_fea, cbind(eval(parse(text = avevar_fea)), Ave_Model_top_df))}

#calculate the difference of the average values of the top selected species to the entire data set relative to the maximal positive enrichment
Diff_Model_top <- (Ave_Model_top - Ave_Categories_fea)/(Max_V_fea - Ave_Categories_fea)
Diff_Model_top_df <- as.data.frame(Diff_Model_top)
colnames(Diff_Model_top_df)[1] = "top50_feasible"
{assign(outvar_fea, cbind(eval(parse(text = outvar_fea)), Diff_Model_top_df))}

#generate barplots of relative enrichment
Diff_Model_top_df_red <- data.frame(Diff_Model_top_df[2:7, , drop = FALSE])
Diff_Model_top_Bar <- ggplot(data=Diff_Model_top_df_red, aes(rownames(Diff_Model_top_df_red), Diff_Model_top_df_red[,1], fill=rownames(Diff_Model_top_df_red))) +
  geom_bar(stat="identity") +
  xlab("Category") + ylab("Relative Enrichment")
ggarrange(Diff_Model_top_Bar, ncol = 1, nrow = 1)
ggsave(file = "Feasibility/Enrichment/BarPlot_SelectedSpecies_SelectedModel_Top50_Feasible.pdf", width = 28,  height = 21,  units = "cm", device = "pdf")

#write out the compiled measurement per selected set of species
Comp_Diff_Model_fea <- as.data.frame(t(Comp_Diff_Model_fea))
Comp_Diff_Model_fea <- Comp_Diff_Model_fea[,-1]
write.csv(Comp_Diff_Model_fea, file = "Feasibility/Enrichment/Enrichment_Categories_feasible_species.csv")
Comp_Average_values_fea <- as.data.frame(t(Comp_Average_values_fea))
Comp_Average_values_fea <- Comp_Average_values_fea[,-1]
write.csv(Comp_Average_values_fea, file = "Feasibility/Enrichment/AverageValues_SelectedSpecies_feasible.csv")

##################################################
###                                            ###
###         Additional sorting by genus        ###
###                                            ###
##################################################

#sort by genus and keep one species per genus, all duplicates based on model
print("Working on sorting of genus")
#create result directories in the working folder
dir.create("Feasibility_Sorted")
dir.create("Feasibility_Sorted/Actual_weights")
dir.create("Feasibility_Sorted/PieCharts")
dir.create("Feasibility_Sorted/Enrichment")

#filtering based on genus
FeasibleFilter <- SpeciesFeasible
#indicate the duplicates as 0
Genusfirst <- as.integer(!duplicated(FeasibleFilter$Genus))
#bind the new columns to the dataset
FeasibleFilter <- data.frame(cbind(FeasibleFilter, Genusfirst))
#sort based on genus first occurrence and then the old order
FeasibleFilter_sorted <- FeasibleFilter[order(-FeasibleFilter$Genusfirst),]
#write out results and match the new order to the category file
print("Writing out newly sorted feasible species list")
write.csv(FeasibleFilter_sorted, file = "Species_selected_feasible_sorted.csv")
write_xlsx(FeasibleFilter_sorted, "Species_selected_feasible_sorted.xlsx")

#sort category values given new order based on genus 
CategoriesFea_sorted <- CategoriesFea[match(FeasibleFilter_sorted$Species_ID,CategoriesFea$Species_ID),]

#select the 50 species and calculate the relative difference
print("Selecting top 50 species")
CategoriesFea_sorted_Top <- head(CategoriesFea_sorted,50)  
top_selected_species_sorted <- head(FeasibleFilter_sorted,50)

#generate histograms of the distribution of scores for the top-selected species
print("Generating histograms")
for (col in c("Tax","Cou","Cer","Nov","JEDI","App")) {
  outhisto = paste0("Feasibility_Sorted/Actual_weights/Histo_", col, "_Scores_SelectedModel_Top50_Feasible.pdf")
  var_histo <- ggplot(CategoriesFea_sorted_Top, aes(x=CategoriesFea_sorted_Top[[col]])) +
    geom_histogram(binwidth=0.5) +
    ggtitle(col)
  ggarrange(var_histo, ncol = 1, nrow = 1)
  ggsave(file = outhisto, width = 14,  height = 14, units = "cm", device = "pdf")
}

#generate pie charts of different aspects for top-selected species
print("Generating pie charts")
Email_counted <- data.frame(table(top_selected_species_sorted$Email))
Email_pie <- ggplot(Email_counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  ggtitle("Individuum") +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5),
        legend.position = "none", 
        panel.background = element_rect(fill = "white"))
ggarrange(Email_pie, ncol = 1, nrow = 1)
ggsave(file = "Feasibility_Sorted/PieCharts/Pies_Individuum_SelectedSpecies_SelectedModel_Top50_Feasible.pdf", width = 56,  height = 56,  units = "cm", device = "pdf")
for (col in c("Phylum","Cer4b","Cer4a","Cer5","Tax6","Cou7","Cou7a","Cou7b","Cou8","Cou8_ITC","Nov9","JEDI10","JEDI11","JEDI12","App13")) {
  outpie = paste0("Feasibility_Sorted/PieCharts/Pies_", col, "_SelectedSpecies_SelectedModel_Top50_Feasible.pdf")
  counted <- data.frame(table(top_selected_species_sorted[[col]]))
  var_pie <- ggplot(counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    ggtitle(col) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 5),
          legend.position = "right", 
          panel.background = element_rect(fill = "white"))
  ggarrange(var_pie, ncol = 1, nrow = 1)
  ggsave(file = outpie, width = 56,  height = 56, units = "cm", device = "pdf")
}

#select calculate the average of the top 50 feasible species
print("Generating bar plots of enrichment")

#calculate average value for the entire dataset
Ave_Categories_sorted <- apply(Categories, 2, mean)
avevar_sorted <- paste0("Comp_Average_values_sorted")
Ave_Categories_sorted_df <- as.data.frame(Ave_Categories_sorted)
colnames(Ave_Categories_sorted_df)[1] = "All"
{assign(avevar_sorted, cbind(Ave_Categories_sorted_df))}

#maximally possible weights for each category
Max_V_sorted <- c(0, 5, 5, 6, 5, 2, 2)
Max_V_sorted_df <- as.data.frame(Max_V_sorted)
colnames(Max_V_sorted_df)[1] = "Maximum"
{assign(avevar_sorted, cbind(eval(parse(text = avevar_sorted)), Max_V_sorted_df))}

Ave_Model_sorted_top <- apply(CategoriesFea_sorted_Top, 2, mean)
Ave_Model_sorted_top_df <- as.data.frame(Ave_Model_sorted_top)
colnames(Ave_Model_sorted_top_df)[1] = "top50_feasible_sorted"
{assign(avevar_sorted, cbind(eval(parse(text = avevar_sorted)), Ave_Model_sorted_top_df))}

#calculate the difference of the average values of the top selected species to the entire data set relative to the maximal positive enrichment
Diff_Model_sorted_top <- (Ave_Model_sorted_top - Ave_Categories_sorted)/(Max_V_sorted - Ave_Categories_sorted)
Diff_Model_sorted_top_df <- as.data.frame(Diff_Model_sorted_top)
colnames(Diff_Model_sorted_top_df)[1] = "top50_feasible_sorted"
outvar_sorted <- paste0("Comp_Diff_Model_sorted")
{assign(outvar_sorted, cbind(Diff_Model_sorted_top_df))}

#generate barplots of relative enrichment
Diff_Model_sorted_top_df_red <- data.frame(Diff_Model_sorted_top_df[2:7, , drop = FALSE])
Diff_Model_sorted_top_Bar <- ggplot(data=Diff_Model_sorted_top_df_red, aes(rownames(Diff_Model_sorted_top_df_red), Diff_Model_sorted_top_df_red[,1], fill=rownames(Diff_Model_sorted_top_df_red))) +
  geom_bar(stat="identity") +
  xlab("Category") + ylab("Relative Enrichment")
ggarrange(Diff_Model_sorted_top_Bar, ncol = 1, nrow = 1)
ggsave(file = "Feasibility_Sorted/Enrichment/BarPlot_SelectedSpecies_SelectedModel_Top50_Feasible.pdf", width = 28,  height = 21,  units = "cm", device = "pdf")

#write out the compiled measurement per selected set of species
Comp_Diff_Model_sorted <- as.data.frame(t(Comp_Diff_Model_sorted))
Comp_Diff_Model_sorted <- Comp_Diff_Model_sorted[,-1]
write.csv(Comp_Diff_Model_sorted, file = "Feasibility_Sorted/Enrichment/Enrichment_Categories_feasible_species.csv")
Comp_Average_values_sorted <- as.data.frame(t(Comp_Average_values_sorted))
Comp_Average_values_sorted <- Comp_Average_values_sorted[,-1]
write.csv(Comp_Average_values_sorted, file = "Feasibility_Sorted/Enrichment/AverageValues_SelectedSpecies_feasible.csv")

##################################################
###                                            ###
###       Final top selection of species       ###
###                                            ###
##################################################

#Determine the total genome size threshold
print("Starting final species selection from the feasible species")
TotalGenome <- as.integer(readline("Provide the total genome size to be sequenced (in Mb): "))
WaitGenome <- as.integer(readline("Provide the total genome size to be sequenced for the waiting list (in Mb): "))
cat("The total genome size in Mb to be sequenced:",file="Configuration.txt",sep="\n",append=TRUE)
cat(TotalGenome,file="Configuration.txt",sep="\n",append=TRUE)
cat("The total genome size in Mb to be sequenced for the waiting list:",file="Configuration.txt",sep="\n",append=TRUE)
cat(WaitGenome,file="Configuration.txt",sep="\n",append=TRUE)

#Subset the data using the maxium cumulative sum of the genome size reaching the provided threshold
FinalSelectedSpecies <- subset(FeasibleFilter_sorted, as.logical(ave(GSize, FUN = function(x) 
           seq_along(x) <= which.max(cumsum(x) >= TotalGenome))))

#Remove the selected species to determine the species for the waiting list		   
RemainingSpecies <- subset(FeasibleFilter_sorted, !(Species_ID %in% FinalSelectedSpecies$Species_ID))
#Subset the data using the maxium cumulative sum of the genome size reaching the half of provided threshold for the waiting list
WaitingListSpecies <- subset(RemainingSpecies, as.logical(ave(GSize, FUN = function(x) 
           seq_along(x) <= which.max(cumsum(x) >= WaitGenome)))) 
#Remove the waiting-list species to determine the non-selected species		   
NotSelectedSpecies <- subset(RemainingSpecies, !(Species_ID %in% WaitingListSpecies$Species_ID))

#write out results and match the final species selection
print("Writing out final species lists")
write.csv(FinalSelectedSpecies, file = "Species_selected.csv")
write_xlsx(FinalSelectedSpecies, "Species_selected.xlsx")
write.csv(WaitingListSpecies, file = "Species_waiting_list.csv")
write_xlsx(WaitingListSpecies, "Species_waiting_list.xlsx")
write.csv(NotSelectedSpecies, file = "Species_not_selected.csv")
write_xlsx(NotSelectedSpecies, "Species_not_selected.xlsx")

#subset the selected species also from the category values
CategoriesFea_selected <- subset(CategoriesFea_sorted, (Species_ID %in% FinalSelectedSpecies$Species_ID))

#create result directories in the working folder
dir.create("Final_selection")
dir.create("Final_selection/Actual_weights")
dir.create("Final_selection/PieCharts")
dir.create("Final_selection/Enrichment")

#generate histograms of the distribution of scores for the top-selected species
print("Generating histograms")
for (col in c("Tax","Cou","Cer","Nov","JEDI","App")) {
  outhisto = paste0("Final_selection/Actual_weights/Histo_", col, "_Scores_SelectedModel_Top50_Feasible.pdf")
  var_histo <- ggplot(CategoriesFea_selected, aes(x=CategoriesFea_selected[[col]])) +
    geom_histogram(binwidth=0.5) +
    ggtitle(col)
  ggarrange(var_histo, ncol = 1, nrow = 1)
  ggsave(file = outhisto, width = 14,  height = 14, units = "cm", device = "pdf")
}

#generate pie charts of different aspects for top-selected species
print("Generating pie charts")
Email_counted <- data.frame(table(FinalSelectedSpecies$Email))
Email_pie <- ggplot(Email_counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  ggtitle("Individuum") +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5),
        legend.position = "none", 
        panel.background = element_rect(fill = "white"))
ggarrange(Email_pie, ncol = 1, nrow = 1)
ggsave(file = "Final_selection/PieCharts/Pies_Individuum_FinalSelectedSpecies.pdf", width = 56,  height = 56,  units = "cm", device = "pdf")
for (col in c("Phylum","Cer4b","Cer4a","Cer5","Tax6","Cou7","Cou7a","Cou7b","Cou8","Cou8_ITC","Nov9","JEDI10","JEDI11","JEDI12","App13")) {
  outpie = paste0("Final_selection/PieCharts/Pies_", col, "_FinalSelectedSpecies.pdf")
  counted <- data.frame(table(FinalSelectedSpecies[[col]]))
  var_pie <- ggplot(counted, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    ggtitle(col) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 5),
          legend.position = "right", 
          panel.background = element_rect(fill = "white"))
  ggarrange(var_pie, ncol = 1, nrow = 1)
  ggsave(file = outpie, width = 56,  height = 56, units = "cm", device = "pdf")
}

#select calculate the average of the top 50 feasible species
print("Generating bar plots of enrichment")

#calculate average value for the entire dataset
Ave_Categories_final <- apply(Categories, 2, mean)
avevar_final <- paste0("Comp_Average_values_final")
Ave_Categories_final_df <- as.data.frame(Ave_Categories_final)
colnames(Ave_Categories_final_df)[1] = "All"
{assign(avevar_final, cbind(Ave_Categories_final_df))}

#maximally possible weights for each category
Max_V_final <- c(0, 5, 5, 6, 5, 2, 2)
Max_V_final_df <- as.data.frame(Max_V_final)
colnames(Max_V_final_df)[1] = "Maximum"
{assign(avevar_final, cbind(eval(parse(text = avevar_final)), Max_V_final_df))}

Ave_Model_final_top <- apply(CategoriesFea_selected, 2, mean)
Ave_Model_final_top_df <- as.data.frame(Ave_Model_final_top)
colnames(Ave_Model_final_top_df)[1] = "final_selected"
{assign(avevar_final, cbind(eval(parse(text = avevar_final)), Ave_Model_final_top_df))}

#calculate the difference of the average values of the top selected species to the entire data set relative to the maximal positive enrichment
Diff_Model_final_top <- (Ave_Model_final_top - Ave_Categories_final)/(Max_V_final - Ave_Categories_final)
Diff_Model_final_top_df <- as.data.frame(Diff_Model_final_top)
colnames(Diff_Model_final_top_df)[1] = "final_selected"
outvar_final <- paste0("Comp_Diff_Model_final")
{assign(outvar_final, cbind(Diff_Model_final_top_df))}

#generate barplots of relative enrichment
Diff_Model_final_top_df_red <- data.frame(Diff_Model_final_top_df[2:7, , drop = FALSE])
Diff_Model_final_top_Bar <- ggplot(data=Diff_Model_final_top_df_red, aes(rownames(Diff_Model_final_top_df_red), Diff_Model_final_top_df_red[,1], fill=rownames(Diff_Model_final_top_df_red))) +
  geom_bar(stat="identity") +
  xlab("Category") + ylab("Relative Enrichment")
ggarrange(Diff_Model_final_top_Bar, ncol = 1, nrow = 1)
ggsave(file = "Final_selection/Enrichment/BarPlot_FinalSelectedSpecies.pdf", width = 28,  height = 21,  units = "cm", device = "pdf")

#write out the compiled measurement per selected set of species
Comp_Diff_Model_final <- as.data.frame(t(Comp_Diff_Model_final))
Comp_Diff_Model_final <- Comp_Diff_Model_final[,-1]
write.csv(Comp_Diff_Model_final, file = "Final_selection/Enrichment/Enrichment_Categories_FinalSelectedSpecies.csv")
Comp_Average_values_final <- as.data.frame(t(Comp_Average_values_final))
Comp_Average_values_final <- Comp_Average_values_final[,-1]
write.csv(Comp_Average_values_final, file = "Final_selection/Enrichment/AverageValues_FinalSelectedSpecies.csv")
