#Author: Leila Erbay
#Purpose: Capstone Script

#TO BE UPDATED:
# 1. ADD INDEX to gene_contr_tbl to speed up determining contribution
# 1a. TRY TO create gene_contr_tbl in R
# 1b. create unique Gene_id Table
# 2. SEND variant_summary_upd and disease_names to MySQL to create relationships
# 3. make phenotype names into an actual list associated to allele on that gene
# APPLY TRY CATCH FOR MORE AUTOMATIZATION
## SELECT IMPORTANT DATA BY INTERSECTION
# ALLOW R SHINY TO BE INTERACTIVE WITH USER



#INSTALL NECESSARY PACKAGES
#install.packages("RMySQL")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("tidyverse")
# install.packages("cluster")
install.packages("XML")
install.packages('RCurl')
install.packages("rlist")
# install.packages("clustMixType")
# install.packages("PCAmixdata")
# install.packages("eclust")
# install.packages("StatMatch")
# install.packages("easycsv")

#LOAD NECESSARY PACKAGES
library(RMySQL)
library(tidyr)
library(dplyr)
library(tidyverse)
library(cluster)
library(easycsv)
library(XML)
library(RCurl)
library(rlist)
# library(clustMixType)
# library(PCAmixdata)
# library(eclust)
# library(StatMatch)

setwd(choose_dir())

############################### MySQL QUERIES #############################

#USER MUST CREATE A DATABASE in MYSQL.
#Database name must be consistent with variables below.
#MySQL code to create DB (must be created in MYSQL):
# CREATE Database databaseName

#VARIABLES THAT USER NEEDS TO ALTER FOR RESPECTIVE INPUTS:
database = 'Capstone_db'
user = 'root'
pwd = 'Rajonrondo'
driver = MySQL()
host = 'localhost'
###DATABASE NEEDS TO EXIST ALREADY

### SETTING THE CONNECTION TO DATABASE
connection <- dbConnect(drv=driver, user = user, password = pwd,
                        host = host, dbname = database)


###############################################################################################
############################# TASK 1: MYSQL INTERACTION #######################################
###############################################################################################

# 1) Count the number of observations of unique pubmed IDs that are
#     linked to each gene, so the output would be:
#     gene, number_of_unique_pubmed_ids_linked_to_the_gene
# 2) Count the relative influence of each gene in a publication -- e.g.
#     if there are 100 genes linked to a specific pubmed ID, you should end
#     up counting 1/100 for that gene, and the output would be:
#     gene, sum_of_relative_pubmed_contributions_linked_to_the_gene


########################### GENE2PUBMED ################################################
### CREATE TABLE FOR gene2pubmed 
dbSendQuery(connection, 'DROP TABLE IF EXISTS gene2pubmed;')

dbSendQuery(connection, 
            'CREATE TABLE  gene2pubmed (
            tax_num_id INT,
            gene_id INT,
            pubmed_id INT);')

### load in gene file into gene2pubmed table 
#NOTE: MAY NEED TO CHANGE FILE PATH WITHIN QUERY2
dbSendQuery(connection, 
            'LOAD DATA LOCAL INFILE 
            \'/Users/LeilaErbay/Desktop/LevelNeu2018/Capstone/gene2pubmed.txt\' 
            INTO TABLE gene2pubmed;')

### Deleting Unnecessary Row
dbSendQuery(connection, 
            'DELETE FROM gene2pubmed
            WHERE gene_id = 0;')

### Clean erroneous data from gene2pubmed
dbSendQuery(connection, 
            'DELETE FROM gene2pubmed
            WHERE pubmed_id IS NULL OR
            gene_id is NULL;')

########################### GENE_CONTR_TBL ################################################
### CREATE GENE_CONTR_TBL : Contribution for each gene to the respective publication id
dbSendQuery(connection, 'DROP TABLE IF EXISTS gene_contr_tbl;')


dbSendQuery(connection, 
            'CREATE TABLE gene_contr_tbl as
            SELECT gene_id, pubmed_id, ROUND(1 /num_occ, 4) as gene_contr
            FROM gene2pubmed
            INNER JOIN(																								
            SELECT pubmed_id as unq_id, COUNT(pubmed_id) as num_occ					
            FROM gene2pubmed 
            GROUP BY unq_id
            ORDER BY unq_id ) as g2
            ON g2.unq_id = pubmed_id
            ORDER BY pubmed_id ASC;')


gene_contr_tbl <- dbReadTable(connection, 'gene_contr_tbl', row.names=F)
gene_contr_tbl <- gene_contr_tbl[order(gene_contr_tbl$gene_id),]
rownames(gene_contr_tbl) <- NULL

########################### TOTAL_CONTR_TBL PER GENE #############################################
dbSendQuery(connection, 'DROP VIEW IF EXISTS total_contr;')
dbSendQuery(connection,
              'CREATE VIEW total_contr as
              SELECT
              	gene_id, SUM(gene_contr) as total_gene_contr
              FROM
              	gene_contr_tbl
              GROUP BY
              	gene_id
              ORDER BY
              	gene_id ASC;')

total_contr <- dbReadTable(connection, 'total_contr', row.names=F)

########################### TOTAL_GENES per PUBID ################################################
dbSendQuery(connection, 'DROP VIEW IF EXISTS total_genes;')
dbSendQuery(connection, 
          'CREATE VIEW total_genes as
          SELECT
            pubmed_id, COUNT(gene_id) as num_genes
          FROM
            gene_contr_tbl
          GROUP BY
            pubmed_id
          ORDER BY
            pubmed_id ASC;')

total_genes <- dbReadTable(connection, 'total_genes', row.names = F)

########################### NUM_PUBS_PER_GENE ################################################
dbSendQuery(connection, 'DROP VIEW IF EXISTS num_pubs_per_gene;')
dbSendQuery(connection,
            'CREATE VIEW num_pubs_per_gene as 
            SELECT
              gene_id, Count(pubmed_id) as num_pubs
            FROM
              gene_contr_tbl
            GROUP BY
              gene_id
            Order By
              gene_id ASC;')
num_pubs_per_gene <- dbReadTable(connection, 'num_pubs_per_gene', row.names = F)

########################### AVG_GENE_CONTR : Sum_contr / num_pubs ################################################
dbSendQuery(connection,'DROP VIEW IF EXISTS avg_gene_contr;')
dbSendQuery(connection, 
          'CREATE VIEW avg_gene_contr as
          SELECT 
              gene_id, total_gene_contr / num_pubs as avg_contr
            FROM (
              SELECT
                total_contr.gene_id, num_pubs, total_gene_contr
              FROM
                num_pubs_per_gene
              JOIN
                total_contr
              ON
                total_contr.gene_id = num_pubs_per_gene.gene_id) as allInfo
            GROUP BY
              gene_id
            ORDER BY
              gene_id ASC;')
avg_gene_contr <- dbReadTable(connection, 'avg_gene_contr', row.names = F)

########################### EXPORTING TABLES ################################################
# write.table(avg_contr, file = 'avg_contr', sep = "\t", col.names = T, row.names = F)
write.table(total_contr, file = "total_contr", sep = "\t", col.names = T, row.names = F)
write.table(total_genes, file = 'total_genes', sep = "\t", col.names = T, row.names = F)
write.table(num_pubs_per_gene, file = "num_pubs_per_gene", sep= "\t", col.names = T, row.names = F)

### REMOVE UNNECESSARY ITEMS
# rm(total_contr, total_genes, num_pubs_per_gene)
rm(database, host, pwd, user, connection)


################################################################################################
################################# TASK 2: EXPLANATION #################################
################################################################################################
# 1)  
# Each row in this file contains a known disease mutation. There are
# actually two records for each variant in this file -- one for genome
# assembly GRCh37 and one for genome assembly GRCh38. They are the exact
# same mutations, but have different coordinates based on the version of
# the genome we are using.
# So we can start with just using GRCh37 for now.

# 2) 
# When you parse the file, check out the phenotypelist column, which
# includes all of the diseases that are linked to each variant -- we'll
# want to use those to come up with number of variants that are linked
# to individual diseases in each gene. These are in their own list per
# variant, so you may end up wanting to make a 1 to many relationship
# between variants and phenotypes, when you unpack this file into the
# db.

# 3) 
# The type column contains the specific mutation type (e.g. whether
# they stop creation of the protein, or just cause a change in the
# protein, as we had discussed), so this is something we may keep track
# of in an aggregation.

# 4) 
# The clinicalsignificance column has the impact of the variants
# (e.g. whether they will cause disease or not.)

# 5) 
# the numbersubmitters field is the number of different labs that
# have seen this variant, it is something we may sum to say we have more
# evidence for this variant being real.



###################################################################################################
################################ VARIANT SUMMARY DF ##############################################
####################################################################################################

variant_summary <- read.delim(file.choose(),
                              header = T, stringsAsFactors = FALSE, quote = "", sep = "\t")

names(variant_summary)[1] <- "AlleleID"
names(variant_summary)[2] <- "variant_type"
names(variant_summary)[3] <- "variant_name"
names(variant_summary)[10] <- "RS_dbSNP"
names(variant_summary)[11] <- "NSV_dbVar"

variant_summary <- rowid_to_column(variant_summary, "recordID")




##############################################################################################
############################## CREATING DISEASE_NAMES DF ######################################
#################################################################################################
disease_names <- read.delim(file.choose(),
                            header = T, stringsAsFactors = F, quote = "", sep = "\t")

names(disease_names)[1] <- "DiseaseNames"

disease_names <- rowid_to_column(disease_names, "diseaseID")



##############################################################################################
############# CHANGE LAST EVALUATED OF VARIANT SUMMARY and Disease_Names TO type DATE #########
#################################################################################################
#set LastEvaluated as a date ### WORKS :)
variant_summary$LastEvaluated <- as.Date(variant_summary$LastEvaluated, "%b %d, %Y")

disease_names$LastModified <- as.Date(disease_names$LastModified, "%d %b %Y")



#########################################################################################################
########################## SEPARATE THE PHENOTYPE LIST INTO SEPARATE ROWS per ALLELE ID #############
##########################################################################################################

### APPLYING SPLICING TO WHOLE DATA SET ######## WORKS :)
s1 <- strsplit(variant_summary$PhenotypeIDS, split = ";")
s2 <- strsplit(variant_summary$PhenotypeList, split = ";")

param2 <- sapply(s2, length)
variant_summary_upd <- data.frame(V1 = rep(variant_summary[[1]], param2),
                                  V2 = rep(variant_summary[[2]], param2),
                                  V3 = rep(variant_summary[[3]], param2),
                                  V4 = rep(variant_summary[[4]], param2),
                                  V5= rep(variant_summary[[5]], param2),
                                  V6 = rep(variant_summary[[6]], param2),
                                  V7 = rep(variant_summary[[7]], param2),
                                  V8 = rep(variant_summary[[8]], param2),
                                  V9 = rep(variant_summary[[9]], param2),
                                  V10 = rep(variant_summary[[10]], param2),
                                  V11 = rep(variant_summary[[11]], param2),
                                  V12 = rep(variant_summary[[12]], param2),
                                  V13 = rep(variant_summary[[13]], param2),
                                  V14 = unlist(s1),
                                  V15 = unlist(s2),
                                  V16 = rep(variant_summary[[16]], param2),
                                  V17 = rep(variant_summary[[17]], param2),
                                  V18 = rep(variant_summary[[18]], param2),
                                  V19 = rep(variant_summary[[19]], param2),
                                  V20 = rep(variant_summary[[20]], param2),
                                  V21 = rep(variant_summary[[21]], param2),
                                  V22 = rep(variant_summary[[22]], param2),
                                  V23 = rep(variant_summary[[23]], param2),
                                  V24 = rep(variant_summary[[24]], param2),
                                  V25 = rep(variant_summary[[25]], param2),
                                  V26 = rep(variant_summary[[26]], param2),
                                  V27 = rep(variant_summary[[27]], param2),
                                  V28 = rep(variant_summary[[28]],param2),
                                  V29 = rep(variant_summary[[29]],param2),
                                  V30 = rep(variant_summary[[30]], param2),
                                  V31 = rep(variant_summary[[31]], param2),
                                  V32 = rep(variant_summary[[32]], param2))

names(variant_summary_upd) <- names(variant_summary)




#########################################################################################################
########################### WRITE updated Variant Summary table to MySQL ################################
#########################################################################################################

# TO DO LATER: 
## send variant_summary_upd, disease_names to MYSQL and use IDs to create Relationship between two 

# dbWriteTable(connection, "variant_summary_upd", variant_summary_upd, row.names = F)
# dbWriteTable(connection, "disease_names", disease_names, row.names = F)


##########################################################################################################
########################### GENOME ASSEMBLY grch37 #######################################################
##########################################################################################################
grch37 <- variant_summary[variant_summary$Assembly == "GRCh37",] 
grch37_upd<- variant_summary_upd[variant_summary_upd$Assembly == 'GRCh37',]

#Name: resetFactors
#Purpose: Look at columns of data set and reset the number of factors in dataframe after it has been subsetted
#Inputs: any data frame
#Outputs: data frame with same amount of data points with the factors reset
resetFactors <- function(someDataFrame){
  for(i in 1:ncol(someDataFrame)){
    if(class(someDataFrame[[i]]) =="factor"){
      someDataFrame[[i]] <- droplevels(someDataFrame[[i]])
    }
  }
  return(someDataFrame)
}

importantGenes <- grch37[grch37$GeneID %in% unique(gene_contr_tbl$gene_id),]
importantGenes_upd <- grch37_upd[grch37_upd$GeneID %in% unique(gene_contr_tbl$gene_id),]

importantGenes<- resetFactors(importantGenes)
importantGenes_upd <- resetFactors(importantGenes_upd)

grch37Genes <- unique(importantGenes$GeneID)

intxn_gene_contr <- gene_contr_tbl[gene_contr_tbl[[1]] %in% grch37Genes,]
intxn_total_contr <- total_contr[total_contr[[1]] %in% grch37Genes,]
intxn_numPubPerGene <- num_pubs_per_gene[num_pubs_per_gene[[1]] %in% grch37Genes,]


###############################################################################################
############################## LIST OF IMPORTANT DF ###################################
###########################################################################################
# 1. variant_summary      --- contains all original data of variant_summary
# 2. variant_summary_upd  --- contains all data of variant_summary 
#                             but phenotype list and phenotype Ids are split
# 3. grch37               --- contains all original data of variant_summary for grch37 assembly
# 4. grch37_upd           --- contains data from variant_summary_upd for grch37 assembly
# 5. disease_names        --- df of disease names
# 6. gene_contr_tbl       --- df of geneID, publication ID and each gene to pubmed contr. 

# 7.****importantGenes       --- gene_ids that intersect gene_contr_tbl and grch37: grch37 data
# 8. importantGenes_upd   --- gene_ids that intersect gene_contr_tbl and grch37_upd: grch37_upd data

# 9.**** intxn_gene_contr    --- gene_ids that intersect gene_contr_tbl and importantGenes : gene_contr data

# 10. total_contr         --- total sum contr per gene
# 11.**** intxn_total_contr  --- gene_ids that intersect total_contr and importantGenes : total_contr data

# 12. num_pub_per_gene    --- total number of publications that gene appears in
# 13.****intxn_numPubPerGene --- gene_ids that intersect num_pub_per_gene and importantGenes: num_pub_per_gene data


###REMOVE SOME DATA FRAMES HERE ****

########################################################################################################
####################################### AGGREGATIONS ####################################################
########################################################################################################

################### AGGREGATE FUNCTION ##################
aggregateFxn <- function(inputDF, dptVar, indptVar, indptVar2 =NULL,
                         indptVar3 = NULL, functionInput,allColNames = NULL){
  if(!is.null(indptVar2) & is.null(indptVar3)){
    outputDF <- aggregate(dptVar~indptVar+indptVar2, data = inputDF, FUN = functionInput)
    rownames(outputDF) <- NULL
    names(outputDF) <- allColNames
    outputDF <- outputDF[order(outputDF[[1]]),]
    return(outputDF)
  }
  
  if(!is.null(indptVar2) & !is.null(indptVar3)){
    outputDF <- aggregate(dptVar~indptVar+indptVar2 + indptVar3, data = inputDF, FUN = functionInput)
    rownames(outputDF) <- NULL
    names(outputDF) <- allColNames
    outputDF <- outputDF[order(outputDF[[1]]),]
    return(outputDF)
  }
  
  outputDF <- aggregate(dptVar~indptVar, data = inputDF, FUN = functionInput)
  rownames(outputDF) <- NULL
  names(outputDF) <- allColNames
  outputDF <- outputDF[order(outputDF[[1]]),]
  return(outputDF)
}



############## AGGREGATION 1: ##################
#### gene_id, phenotype assoc to variant, number of variants
gpVs <- aggregateFxn(inputDF=importantGenes_upd,dptVar= importantGenes_upd$recordID,
                     indptVar=importantGenes_upd$GeneID, indptVar2=importantGenes_upd$PhenotypeList,
                     functionInput=length,
                     allColNames = c("GeneID", "PhenotypeList", "NumberOfVariants"))



############## AGGREGATION 2: #########
### gene_id, variant_type, number of variants
variantType <- aggregateFxn(inputDF=importantGenes,dptVar= importantGenes$recordID,
                            indptVar = importantGenes$GeneID,
                            indptVar2=importantGenes$variant_type, 
                            functionInput=length, 
                            allColNames=c("GeneID", "variant_type", "NumberOfVariants"))

variantTypeTot<- aggregateFxn(importantGenes, dptVar = importantGenes$recordID, 
                        indptVar = importantGenes$GeneID,
                        functionInput = length, 
                        allColNames =c("GeneID", "NumberOfRecords") )

########## AGGREGATION 3 : ##############
#  geneID, variant name, # submitters
variantName <- aggregateFxn(inputDF = importantGenes, dptVar=importantGenes$NumberSubmitters,
                            indptVar=importantGenes$GeneID, indptVar2= importantGenes$variant_name, 
                            functionInput = sum, allColNames = c("GeneID", "variant_name", "NumberSubmitters"))

########## AGGREGATION 4 : ##############
# geneID, clinical sig, clinSig simple # submitters
clinSigSub <- aggregateFxn(inputDF = importantGenes, dptVar = importantGenes$NumberSubmitters,
                           indptVar = importantGenes$GeneID, indptVar2 = importantGenes$ClinicalSignificance,
                           indptVar3= importantGenes$ClinSigSimple, 
                           functionInput = sum, allColNames = c("GeneID", "ClinicalSignificance", "ClinSigSimple", "NumberSubmitters"))

clinSigTotSub <- aggregateFxn(inputDF=clinSigSub, dptVar= clinSigSub$NumberSubmitters,
                            indptVar= clinSigSub$GeneID,functionInput  =sum,
                            allColNames = c("GeneID", "TotalNumberSubmitters"))

clinSigRec <- aggregateFxn(inputDF = importantGenes, dptVar = importantGenes$recordID,
                           indptVar = importantGenes$GeneID, indptVar2 = importantGenes$ClinicalSignificance,
                           indptVar3= importantGenes$ClinSigSimple, 
                           functionInput = length, allColNames = c("GeneID", "ClinicalSignificance", "ClinSigSimple", "NumberOfRecords"))

clinSigTotRec <-  aggregateFxn(inputDF=clinSigRec, dptVar= clinSigRec$NumberOfRecords,
                               indptVar= clinSigRec$GeneID,functionInput  =sum,
                               allColNames = c("GeneID", "TotalNumberRecords"))


########## AGGREGATION 5 : ##############
#gene Id, clin sig simple, # submitters
clinSimSub <- aggregateFxn(inputDF = clinSigSub, dptVar = clinSigSub$NumberSubmitters, 
                              indptVar = clinSigSub$GeneID, indptVar2 = clinSigSub$ClinSigSimple,
                              functionInput = sum, allColNames = c("GeneID","ClinSigSimple", "NumberSubmitters"))


clinSimTotSub <- aggregateFxn(inputDF = clinSimSub, dptVar = clinSimSub$NumberSubmitters, 
                              indptVar = clinSimSub$GeneID, functionInput = sum,
                              allColNames = c("GeneID","TotalNumberSubmitters"))


# geneID, NumOFAlleles that are pathogenic
clinSimRec <- aggregateFxn(inputDF = importantGenes, dptVar = importantGenes$recordID, 
                                 indptVar = importantGenes$GeneID, indptVar2 = importantGenes$ClinSigSimple, 
                                 functionInput = length, allColNames = c("GeneID", "ClinSigSimple", "NumberOfRecords"))

clinSimTotRec <- aggregateFxn(inputDF = clinSimRec, dptVar =clinSimRec$NumberOfRecords,
                                     indptVar =clinSimRec$GeneID, functionInput = sum,
                                     allColNames = c("GeneID", "TotalNumRecords"))



############## EXPAND FUNCTION ######################
expandFxn <- function(inputDF, formula, fxn, value, addCol= NULL, colName=NULL){
  output <- dcast(data = inputDF,formula = formula ,fun.aggregate = fxn,value.var = value)
  if(!is.null(colName)){
    output <- cbind(output, addCol)
    names(output)[ncol(output)] <- colName
  }
  return(output)
}


##################################### EXPANDED DATA ###########################################
clinSigExpSubmitters <- expandFxn(clinSigSub,formula = GeneID~ClinicalSignificance, fxn = sum, 
                                  value = "NumberSubmitters", addCol =clinSimTotSub$TotalNumberSubmitters, 
                                  colName="TotalNumberSubmitters")

clinSigExpRecords <- expandFxn(clinSigRec, formula = GeneID~ClinicalSignificance, fxn = sum, 
                               value = "NumberOfRecords", addCol = clinSigTotRec$TotalNumberRecords,
                               colName = "TotalNumberRecords")


clinSimExpSubmitters <-  expandFxn(inputDF = clinSimSub,formula = GeneID~ClinSigSimple,
                                   fxn=sum, value = "NumberSubmitters", 
                                   addCol = clinSimTotSub$TotalNumberSubmitters,
                                   colName = "TotalNumberSubmitters")


clinSimExpRecords <- expandFxn(clinSimRec, formula= GeneID~ClinSigSimple, fxn = sum,
                           value = "NumberOfRecords", addCol = clinSimTotRec$TotalNumRecords,"TotalNumRecords")


variantTypeExp <- expandFxn(variantType, formula = GeneID ~ variant_type, 
                            fxn = sum, value ="NumberOfVariants", variantTypeTot$NumberOfRecords,
                            "TotalNumOfRecords")

rm( clinSigTotRec, clinSigTotSub,clinSigTotRec, clinSimTotSub, variantTypeTot )



#########################################################################################################
################################### GATHER CONTINUOUS VARS ###################################
#########################################################################################################
makeContinuous <- function(inputDF){
  outputDF <- cbind(inputDF[,1] ,inputDF[,3:ncol(inputDF)-1]/ as.numeric(inputDF[,ncol(inputDF)]))
  colnames(outputDF)[1] <- "GeneID"
  return(outputDF)
}

clinSigCont_NumSub <- makeContinuous(clinSigExpSubmitters)
clinSigCont_NumRec <- makeContinuous(clinSigExpRecords)
clinSimCont_NubSub <- makeContinuous(clinSimExpSubmitters)
clinSimCont_NumRec <- makeContinuous(clinSimExpRecords)
variantTypeCont <- makeContinuous(variantTypeExp)

##additional ~cont data
#intxn_gene_contr
#intxn_numPubPerGene
#intxn_total_contr


##################################################################################################
############################### Additional Info on Clinical Signifiance ###########################
##################################################################################################


retrieveHTML <- function(urlString){
  url <- getURL(urlString)
  htmlTbls <- readHTMLTable(url) 
  htmlTbls<- list.clean(htmlTbls, fun = is.null, recursive = FALSE)
  print(length(htmlTbls))
  return(htmlTbls)
}


getHTMLtbls <- function(htmlList, tblNum){
  htmlDF <-cbind(data.frame(unlist(htmlTbls[[tblNum]][,1])),data.frame(unlist(htmlTbls[[tblNum]][,2])))
  names(htmlDF) <- c("X1", "X2")
  return(htmlDF)
}

htmlTbls <- retrieveHTML('https://www.ncbi.nlm.nih.gov/clinvar/docs/clinsig/')
htmlTbls_1 <-getHTMLtbls(htmlTbls, 1)
htmlTbls_2 <- getHTMLtbls(htmlTbls, 2)
htmlTbls_3 <- getHTMLtbls(htmlTbls, 3)
htmlTbls_4 <- getHTMLtbls(htmlTbls, 4)
  

##################################################################################################
############################### GROUPING BY Clinical Significance ###############################
#################################################################################################


#####################################################################################################
################################## PREDICTIVE ANALYSIS #################################
#####################################################################################################

## TRAIN SET: Contains Pathogenic and NonPathogenic
## TEST SET: Unknown 
trainSet <- grch37[importantGenes$ClinSigSimple==1 |importantGenes$ClinSigSimple==-1,]
testSet <- grch37[importantGenes$ClinSigSimple==0 ,]


#Name: preProc
#Purpose: Reduce the number of variables in data frame in relation to response var (ClinSigSimple)
#         Methods of variable selection: Chi squared test against response var
#                                         T-test test of continuous vars between pathogenic and not pathogenic
#Inputs: any data frame, boolean for hinting if the input data frame is the test or train set
#Outputs: Altered data frame
preProc <- function(alteredDF, train = FALSE){

  
  #Additional Columns:
  #1. NumPhenos = counts the number of phenotypic changes assoc with a variant
  #2. Distance = difference between the start and stop locations of the allele where the variant occurs
  alteredDF$NumDiseases <- lengths(regmatches(alteredDF$PhenotypeList, gregexpr(";", alteredDF$PhenotypeList)))+1
  alteredDF$Distance <- alteredDF$Stop - alteredDF$Start
  

  ## removing unnecessary data
  delete <- c("LastEvaluated", "Start", "Stop", "PhenotypeList", "PhenotypeIDS")
  alteredDF <- alteredDF[, !names(alteredDF) %in% delete]


  ### APPROPRIATELY SET COLUMNS TO FACTORS / NUMERICS / 
  for (i in 1:ncol(alteredDF)){
    if (i == (which(colnames(alteredDF) == "NumDiseases")) |
        i == (which(colnames(alteredDF) == "Distance")) |
        i == (which(colnames(alteredDF) == "NumberSubmitters"))) {
      next()
    }
    else{
      alteredDF[[i]] <- as.factor(alteredDF[[i]])
      alteredDF[[i]] <- as.factor(alteredDF[[i]])
    }
  }
  
  
  ### REMOVE COLUMNS WHOSE TOTAL # FACTORS / TOTAL ROWS > 0.2

  
  ## Determine THE DIMENSIONS FOR THE DATA that are deemed important
  # 1. USING CHI SQ analysis to test between response and categorical vars
  # 2. using t test to test between response and each continuous
  if (train == T){
    
    delete <- NULL
    ### FOR LOOP: REMOVES COLUMNS THAT HAVE TOO MANY CATEGORIES
    for (i in 1:ncol(alteredDF)){   
      if (class(alteredDF[[i]]) == "factor" ){
        if(nlevels(alteredDF[[i]])/nrow(alteredDF) > 0.2){

          delete <- append(delete, colnames(alteredDF)[i])
        }
      }
    }

    alteredDF <- alteredDF[, !names(alteredDF) %in% delete]

    
    ##RESET THE FACTORS 
    alteredDF <- resetFactors(alteredDF)   
  

    ### FOR LOOP: DO Chi-SQ INDPT TEST BETWEEN CATEGORICAL VARIABLES AND RESPONSE
    ## IF P-value > 0.05 then it is considered that there is no relation between response and variable at hand
    delete <- NULL
    for (i in 1:ncol(alteredDF)){
      if(class(alteredDF[[i]])== "factor"){
        if (colnames(alteredDF)[i] == "ClinSigSimple") next()
        tbl <- table( alteredDF$ClinSigSimple, alteredDF[[i]])
        result <- chisq.test(tbl)
        
 

        
        if ((result$p.value) > 0.05) {        #We remove the columns that do not seem to have any 
          delete <-  append(delete, colnames(alteredDF)[i])        #significant relationship to the response var (ie if there is a pvalue > 0.05)
        }   
        
      }
    }
    alteredDF <- alteredDF[, !names(alteredDF) %in% delete]

    ### FOR LOOP: T-test between continuous var and response
    ## IF P-value > 0.05 then it is considered that there is no relation between response and variable at hand
    
    ###### MAYBE NOT NEED TO DO T TEST bc it selects EVERYTHING OUT
    # delete <- NULL
    # for(i in 1:ncol(alteredDF)){
    #   if (class(alteredDF[[i]]) != "factor") {
    #     set1 <- alteredDF[alteredDF$ClinSigSimple == 1, i]    #Pathogenic Set
    #     set2 <- alteredDF[alteredDF$ClinSigSimple == -1, i]   #Non Pathogenic Set
    #    
    # 
    # 
    #     #Determine t test result
    #     result <- t.test(set1, set2, alternative = "two.sided",
    #                      mu = mean(set1)- mean(set2), 
    #                      var.equal = ifelse(var.test(set1, set2, alternative = "two.sided")$p.value*2 < 0.05, F, T),
    #                      conf.level = .90)
    #     
    #     ## remove columns of continuous vars that don't have a relation to response
    #     if(result$p.value > 0.1){
    #       delete <-  append(delete, colnames(alteredDF)[i]) 
    #     }
    #     
    #   }
    # }
    # 
    # alteredDF <- alteredDF[, !names(alteredDF) %in% delete]
 

    ###FINAL COLUMNS OF ALTERED (train set) will be the columns used in the TEST set
    columns<<- colnames(alteredDF)
  }
  

 
  
  ##WORKING ON TEST SET: Clear out Columns in test set that have not been selected in the preproccess on the train set
  if(train == F){
    delete <- NULL
    for (i in 1:ncol(alteredDF)) {
        if (colnames(alteredDF)[i] %in% columns) next()
        else delete <- append(delete, colnames(alteredDF)[i])
    }
    alteredDF <- alteredDF[, !names(alteredDF) %in% delete]
  }
  return(alteredDF)
}



trainSet <- preProc(trainSet, train =T)
testSet <- preProc(testSet, train = F)

## NAME: normalRegr
# Purpose: Normalize continuous variables using standard score (z-score method)
# Input: any data frame that needs to be normalized
normalizeContVar <- function(alteredDF){
  for (i in 1:ncol(alteredDF)) {
    if (class(alteredDF[[i]]) != "factor" & class(alteredDF[[i]]) == "numeric" |class(alteredDF[[i]]) == "integer" ){
     alteredDF[[i]] <- as.vector(scale(alteredDF[[i]]))

    }
  }
  return(alteredDF)
}


### # POINTS IN TRAIN SET < # POINTS IN TEST 

trainSet <- normalizeContVar(trainSet)
testSet <- normalizeContVar(testSet)



################# SIMILARITY MATRIX BETWEEN PATHOGENIC, NON PATHO and UNKNOWN ##############

set.seed(1996)
gower.matPatho <- as.matrix(daisy(trainSet[trainSet$ClinSigSimple == 1,][1:100,],metric = "gower"))
colnames(gower.matPatho)<-1:100
rownames(gower.matPatho) <- 1:100

gower.matNonPatho <- as.matrix(daisy(trainSet[trainSet$ClinSigSimple == -1,][1:100,],metric = "gower"))
colnames(gower.matNonPatho)<-1:100
rownames(gower.matNonPatho) <- 1:100

gower.matUnknown <- as.matrix(daisy(testSet[testSet$ClinSigSimple == 0,][1:100,],metric = "gower"))
colnames(gower.matUnknown)<-1:100
rownames(gower.matUnknown) <- 1:100

## LOGISTIC REGRESSION
trainGLM <- glm(ClinSigSimple ~., data = trainSet)

