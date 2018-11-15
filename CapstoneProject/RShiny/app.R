# Author : Leila Erbay
# Purpose : RShiny App for Capstone Project


### TO BE UPDATED
### allow interaction of top % that user would want to see
##PAGE 1: PIE CHART FOR 2 other interactions 

## Necessary package to create RShiny
#install.packages("shiny")
#install.packages("markdown")
#install.packages("easycsv")
# install.packages("DT")
#install.packages('htmltools')
#install.packages('shinydashboard')
# install.packages('reshape2')
# install.packages('ggplot2')
# install.packages("rsconnect")

library(shiny)
library(markdown)
library(easycsv)
library(DT)
library(htmltools)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(rsconnect)

# setAccountInfo(name='lerbay',
#                           token='69B94272D87D1D60356F353E4ABF15CF',
#                           secret='DVbnUVfGEOFyjavQLXedv8iDKlynvSa1x3YwxL+9')
# 
# deployApp('/Users/LeilaErbay/Desktop/LevelNeu2018/Capstone/RShiny')
setwd('/Users/LeilaErbay/Desktop/LevelNeu2018/Capstone/RShiny')
source('/Users/LeilaErbay/Desktop/LevelNeu2018/Capstone/Helper.R', local = T)
#source('../Cassa_query.R, local = T)


### UI for the RSHINY APP
ui <- fluidPage(navbarPage(
  "Select Your View",
  
  ######## PAGE 1: GENE CONTR TABLE #####################
  tabPanel(
    "Total Counts",
    
    ############ GENE AND TOTAL CONTR ################
    fluidRow(
      h3("Gene and Total Contribution"),
      sidebarLayout(
        sidebarPanel(selectInput(
          inputId = "var",
          label = h3("Select variable to display"),
          choices = c(
            "Outliers" = 'out',
            "Top 1%" = 'top1',
            "Bottom 1%" = 'bot1'
          )
        ))
        ,
        mainPanel(
          textOutput('total'),
          textOutput('selected'),
          textOutput('selected2'),
          br(),
          plotOutput('pieGeneTot')
            
        )
      )
        
  
    )
    ,
    ############ PUB AND TOTAL NUM GENES ################
    fluidRow(
      h3("Publications and Number of Genes"),
      sidebarLayout(
        sidebarPanel(selectInput(
          inputId = "var2",
          label = h3("Select variable to display"),
          choices = c(
            "Outliers" = 'outPub',
            "Top 1%" = 'topPub',
            "Bottom 1%" = 'botPub'
          )
        ))
        ,
        mainPanel(
          textOutput('totalPub'),
          textOutput('selectedPub'),
          textOutput('selectedPub2'),
          br(),
          plotOutput('piePubTot')
                 
        )
      )
    ),
    
    ############ GENE AND TOTAL NUM OF PUBS ################
    fluidRow(
      h3("Genes and Number of Publication Appearances "),
      sidebarLayout(
        sidebarPanel(selectInput(
          inputId = "var3",
          label = h3("Select variable to display"),
          choices = c(
            "Outliers" = 'outGene',
            "Top 1%" = 'topGene',
            "Bottom 1%" = 'botGene'
          )
        ))
        ,
        mainPanel(
          textOutput('totalGene'),
          textOutput('selectedGene'),
          textOutput('selectedGene2'),
          br(),
          plotOutput('piePubGeneTot')
                 
        )
      )
    )
  ),

    
  ####### PAGE 2 : DISEASE NAMES ###################
  tabPanel("Disease Names",

             h3("List of Possible Diseases declared by NIH"),
             dataTableOutput("disease")
             
             
           ),
  ############ PAGE 3 ###################################
  tabPanel(
    "Gene Aggregations",
    fluidRow(
      h3("GeneID: Number of Reports of Each Phenotypic Change "),
      
      selectInput(
        inputId = "geneId",
        label = h3("Select GeneID you wish to explore"),
        choices = ("Gene ID" = unique(gpVs$GeneID)),
        selected = 2
      ),
      column(6,
      plotOutput("genePlot")),
      column(6,
      plotOutput('geneDistr')
      )
    ),
    fluidRow(
      h3("GeneID: Number of Reports for Associated Variant Type"),
      
      column(6,
      plotOutput("genePlot2")),
      column(6,
      plotOutput('geneDistr2'))
    ),
    fluidRow(
      h3("GeneID: Number of Reports for Associated Variant Name"),
      plotOutput("genePlot3")

    )
    
  ),
  ############ PAGE 4 ###################################
  tabPanel(
    "Number of Submitters",
    fluidRow(
      h3("Clinical Significance and Simple Clinical Signifiance"),
      selectInput(
        inputId = "geneDetID",
        label = h3("Select GeneID you wish to explore"),
        choices = ("Gene ID" = unique(clinSimSub$GeneID)),
        selected = 2
      )
    ),
    fluidRow(
      column(6,
      plotOutput("geneOut")),
      column(6,
             plotOutput('distrGeneOut')
      )
    ),
    fluidRow(
      column(6,
        plotOutput("geneOut2")),
      column(6, 
             plotOutput('distrGeneOut2'))
    )
      
  ),
  ############ PAGE 5 ###################################
  tabPanel("Pathogenicity Counts",
    fluidRow(
      selectInput(
        inputId = "geneId5",
        label = h3("Select GeneID you wish to explore"),
        choices = ("Gene ID" = unique(clinSimRec$GeneID)),
        selected = 2
      )
    ),
    fluidRow(
     
        plotOutput("geneOut5")
    )
    ),
  ################## PAGE 6 #####################
  tabPanel("Similarity Matrix",
    h3("Similarity Matrices Within Groups Based on Pathogenicity Classification"),
    fluidRow(
      column(6,
             textOutput('titlePatho'),
             plotOutput("pathoMatrix")
        ),
      column(6,
             textOutput('titleNon'),
             plotOutput('nonPathoMatrix')
             )
    ),
    br(),
    fluidRow(
      column(6, align = "center",
             textOutput('titleUnknown'),
             plotOutput('UnknownMatrix')
      )
    )
    
    
  )
  
  
))




### DATA FOR THE RSHINY APP


server <- function(input, output) {
  ############### PAGE 1 ################################
  ############ GENE AND TOTAL CONTR ################
  output$total <- renderText({
    outText <- nrow(total_contr)
    paste("Total Number of Counts: \t", outText)
  })
  output$selected <- renderText({
    outText <- switch(
      input$var,
      'out' = length(boxplot.stats(total_contr$X.total_gene_contr.)$out),
      'top1' = length(unique(total_contr[total_contr$X.total_gene_contr. >= quantile(total_contr$X.total_gene_contr., .99), 1])),
      'bot1' = length(unique(total_contr[total_contr$X.total_gene_contr. <= quantile(total_contr$X.total_gene_contr., .01), 1]))
    )
    
    paste("Number of Counts: \t", outText)
  })
  output$selected2 <- renderText({
    outText2 <- switch(
      input$var,
      'out' = length(boxplot.stats(total_contr$X.total_gene_contr.)$out) /
        nrow(total_contr),
      'top1' = length(unique(total_contr[total_contr$X.total_gene_contr. >= quantile(total_contr$X.total_gene_contr., .99), 1])) /
        nrow(total_contr),
      'bot1' = length(unique(total_contr[total_contr$X.total_gene_contr. <= quantile(total_contr$X.total_gene_contr., .01), 1])) /
        nrow(total_contr)
    )
    paste("The porportion of counts : \t",  outText2)
  })
  output$pieGeneTot <- renderPlot({
    table <- data.frame(types = c('OUT', 'IN'),
                        values = c(length(boxplot.stats(total_contr$X.total_gene_contr.)$out) /
                          nrow(total_contr),
                                   (nrow(total_contr) - length(boxplot.stats(total_contr$X.total_gene_contr.)$out))/ nrow(total_contr)))
    
    ggplot(table, aes(x= "", y = values, fill = types)) + 
          geom_bar(width = 1, stat = "identity") +
          coord_polar(theta = "y", start = 0) +
           scale_fill_manual(values = c("Green", "Red")) +
           labs(x = "", y = "", title = "Outliers for Gene Contribution \n",
                          fill = "Outliers vs Rest") + 
          theme(plot.title = element_text(hjust = 0.5), 
                            legend.title = element_text(hjust = 0.5, face="bold", size = 10))
   
    
  })
  
  ############ PUBS AND #GENES ################
  output$totalPub <- renderText({
    outText <- nrow(total_genes)
    paste("Total Number of Counts: \t", outText)
  })
  output$selectedPub <- renderText({
    outText <- switch(
      input$var2,
      'outPub' = length(boxplot.stats(total_genes$X.num_genes.)$out),
      'topPub' = length(unique(total_genes[total_genes$X.num_genes. >= quantile(total_genes$X.num_genes., .99), 1])),
      'botPub' = length(unique(total_genes[total_genes$X.num_genes. <= quantile(total_genes$X.num_genes., .01), 1]))
    )
    
    paste("Number of Counts: \t", outText)
  })
  
  output$selectedPub2 <- renderText({
    outText2 <- switch(
      input$var2,
      'outPub' = length(boxplot.stats(total_genes$X.num_genes.)$out) /
        nrow(total_genes),
      'topPub' = length(unique(total_genes[total_genes$X.num_genes. >= quantile(total_genes$X.num_genes., .99), 1])) /
        nrow(total_genes),
      'botPub' = length(unique(total_genes[total_genes$X.num_genes. <= quantile(total_genes$X.num_genes., .01), 1])) /
        nrow(total_genes)
    )
    paste("The porportion of counts : \t",  outText2)
  })
  
  output$piePubTot <- renderPlot({
    table <- data.frame(types = c('OUT', 'IN'),
                        values = c(length(boxplot.stats(total_genes$X.num_genes.)$out) /
                                     nrow(total_genes),
                                   (nrow(total_genes) - length(boxplot.stats(total_genes$X.num_genes.)$out))/ nrow(total_genes)))
    
    ggplot(table, aes(x= "", y = values, fill = types)) + 
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y", start = 0) +
      scale_fill_manual(values = c("Green", "Red")) +
      labs(x = "", y = "", title = "Outliers for Total Number of Genes per Publication \n",
           fill = "Outliers vs Rest") + 
      theme(plot.title = element_text(hjust = 0.5), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 10))
    
  })
  
  ############ GENE AND TOTAL NUM of PUBS ################
  output$totalGene <- renderText({
    outText <- nrow(num_pubs_per_gene)
    paste("Total Number of Counts: \t", outText)
  })
  output$selectedGene <- renderText({
    outText <- switch(
      input$var2,
      'outPub' = length(boxplot.stats(num_pubs_per_gene$X.num_pubs.)$out),
      'topPub' = length(unique(num_pubs_per_gene[num_pubs_per_gene$X.num_pubs. >= quantile(num_pubs_per_gene$X.num_pubs., .99), 1])),
      'botPub' = length(unique(num_pubs_per_gene[num_pubs_per_gene$X.num_pubs. <= quantile(num_pubs_per_gene$X.num_pubs., .01), 1]))
    )
    
    paste("Number of Counts: \t", outText)
  })
  
  output$selectedGene2 <- renderText({
    outText2 <- switch(
      input$var3,
      'outGene' = length(boxplot.stats(num_pubs_per_gene$X.num_pubs.)$out) /
        nrow(num_pubs_per_gene),
      'topGene' = length(unique(num_pubs_per_gene[num_pubs_per_gene$X.num_pubs. >= quantile(num_pubs_per_gene$X.num_pubs., .99), 1])) /
        nrow(num_pubs_per_gene),
      'botGene' = length(unique(num_pubs_per_gene[num_pubs_per_gene$X.num_pubs. <= quantile(num_pubs_per_gene$X.num_pubs., .01), 1])) /
        nrow(num_pubs_per_gene)
    )
    paste("The porportion of counts : \t",  outText2)
  })
  
  output$piePubGeneTot <- renderPlot({
    table <- data.frame(types = c('OUT', 'IN'),
                        values = c(length(boxplot.stats(num_pubs_per_gene$X.num_pubs.)$out) /
                                     nrow(num_pubs_per_gene),
                                   (nrow(num_pubs_per_gene) - length(boxplot.stats(num_pubs_per_gene$X.num_pubs.)$out))/
                                     nrow(num_pubs_per_gene)))
    
    ggplot(table, aes(x= "", y = values, fill = types)) + 
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y", start = 0) +
      scale_fill_manual(values = c("Green", "Red")) +
      labs(x = "", y = "", title = "Outliers for Total Number of Publications per Gene \n",
           fill = "Outliers vs Rest") + 
      theme(plot.title = element_text(hjust = 0.5), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 10))
  })
  
  ############ GENE AND TOTAL NUM of PUBS ################
  output$totalGene <- renderText({
    outText <- nrow(num_pubs_per_gene)
    paste("Total Number of Counts: \t", outText)
  })
  
  
  ############ PAGE 2 ###################################
  output$disease <- renderDataTable({
    datatable(
      disease_names,
      rownames = FALSE,
      colnames = names(disease_names),
      filter = "none",
      selection = "none",
      width= "10"
    )
  })
  
  ############ PAGE 3 ###################################
  output$genePlot <- renderPlot({
    ggplot(data= gpVs[gpVs$GeneID == input$geneId,],aes( x=PhenotypeList, y= NumberOfVariants, fill=PhenotypeList ))+
      geom_bar(stat="identity") + 
      geom_text(aes(label=NumberOfVariants), vjust=-0.3, size=3.5)+
      labs(x = "Phenotypes Associated with Gene Variant", y = "Number of Records", title = "Number of Records Associated with Each Phenotype",
           fill = "Legend") 
  
  })
  output$geneDistr <- renderPlot({
    aggPhenos <- aggregate(NumberOfVariants~PhenotypeList, data = gpVs, FUN = sum)
    ggplot(data= aggPhenos,aes( x=PhenotypeList, y= NumberOfVariants, col = 'red'))+
      geom_bar(stat="identity") + 
      labs(x = "Phenotypes Associated with Gene Variant", y = "Number of Records", title = "Distribution",
           fill = "") +
      theme(axis.text.x=element_blank())
  })
    
  output$genePlot2 <- renderPlot({
    
    ggplot(data= variantType[variantType$GeneID == input$geneId, ],
           aes( x=variant_type, y= NumberOfVariants, fill=variant_type ))+
      geom_bar(stat="identity") + 
      geom_text(aes(label=NumberOfVariants), vjust=-0.3, size=3.5)+
      labs(x = "Variant Type on Gene", y = "Number of Records", title = "Number of Records Associated with Each Variant Type",
           fill = "Legend")
    
  })
  output$geneDistr2 <- renderPlot({
    aggPhenos <- aggregate(NumberOfVariants~variant_type, data = variantType, FUN = sum)
    ggplot(data= aggPhenos,aes( x=variant_type, y= NumberOfVariants, col = 'red'))+
      geom_bar(stat="identity") + 
      labs(x = "Phenotypes Associated with Gene Variant", y = "Number of Records", title = "Distribution",
           fill = "") +
      theme(axis.text.x=element_blank())
  })
  
  output$genePlot3 <- renderPlot({
    ggplot(data= variantName[variantName$GeneID == input$geneId, ],
           aes( x=variant_name, y= NumberSubmitters, fill=variant_name ))+
      geom_bar(stat="identity") + 
      geom_text(aes(label=NumberSubmitters), vjust=-0.3, size=3.5)+
      labs(x = "Variant Name on Gene", y = "Number of Submitters", title = "Number of Submitters Associated with Each Variant Type",
           fill = "Legend")+
      theme(axis.text.x=element_blank())
  })

  ############ PAGE 4 ###################################
  output$geneOut <- renderPlot({
    ggplot(data= clinSigSub[clinSigSub$GeneID == input$geneDetID,],aes( x=ClinicalSignificance,
                                                                       y= NumberSubmitters, fill=ClinicalSignificance))+
      geom_bar(stat="identity") + 
      geom_text(aes(label=NumberSubmitters), vjust=-0.3, size=3.5)+
      labs(x = "ClinicalSignifance",
           y = "Number of Submitters", title = "Disease and Frequency Found on Gene ",
           fill = "Legend")+ theme(axis.text.x=element_blank())
    
  })
  output$distrGeneOut <- renderPlot({
    data <- aggregate(NumberSubmitters~ClinicalSignificance, data =clinSigSub, FUN = sum )
    ggplot(data= data,aes( x=ClinicalSignificance,y= NumberSubmitters, col='red'))+
      geom_bar(stat="identity") + 
      labs(x = "ClinicalSignifance",
           y = "Number of Submitters", title = "Distribution", col = 'Legend')+
      theme(axis.text.x=element_blank())+  theme(axis.ticks.x=element_blank())
    
  })
  output$geneOut2 <- renderPlot({
    ggplot(data= clinSimSub[clinSimSub$GeneID == input$geneDetID,],
           aes( x=ClinSigSimple,y= NumberSubmitters, fill=ClinSigSimple))+
      geom_bar(stat="identity") + 
      labs(x = "Simple Clinical Significance",
           y = "Number of Submitters", title = "Disease and Frequency Found on Gene ID ", fill = 'Legend')+
      theme(axis.ticks.x=element_blank())
  
    
  })
  output$distrGeneOut2 <- renderPlot({
    data <- aggregate(NumberSubmitters~ClinSigSimple, data =clinSimSub, FUN = sum )
    ggplot(data= data,aes( x=ClinSigSimple,y= NumberSubmitters, col='black'))+
      geom_bar(stat="identity") + 
      labs(x = "Simple Clinical Signifance",
           y = "Number of Submitters", title = "Distribution", col = 'Legend')+  theme(axis.ticks.x=element_blank())
    
  })
############### PAGE 5 ###################################
  #### MORE TO BE ADDED
  output$geneOut5 <- renderPlot({
    ggplot(data= clinSimRec[clinSimRec$GeneID == input$geneId5,],
           aes( x=ClinSigSimple,y= NumberOfRecords, fill = ClinSigSimple))+
      geom_bar(stat="identity") + 
      geom_text(aes(label=NumberOfRecords), vjust=-0.3, size=3.5)+
      labs(x = "Simple Clinical Significance",
           y = "Number of Records", title = "Disease and Frequency Found on Gene ",
           fill = "Legend")
  })

############# PAGE 6 #########################
  output$titlePatho <- renderText({
    paste('Similarity within Pathogenic Data')
  })
  output$pathoMatrix <- renderPlot({
    melted_corPatho <- melt(gower.matPatho)
    ggplot(data = melted_corPatho, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+
      labs(x = "Data Points",
           y = "Data Points",
           fill = "Relationship")
  })
  
  output$titleNon <- renderText({
    paste('Similarity within Non-Pathogenic Data')
  })
  output$nonPathoMatrix <- renderPlot({
    melted_corPatho <- melt(gower.matNonPatho)
    ggplot(data = melted_corPatho, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+ labs(x = "Data Points",
                        y = "Data Points",
                        fill = "Relationship")
  })
  
  output$titleUnknown <- renderText({
    paste('Similarity within Unknown Pathogenicity Data')
  })
  output$UnknownMatrix <- renderPlot({
    melted_corPatho <- melt(gower.matUnknown)
    ggplot(data = melted_corPatho, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+ labs(x = "Data Points",
                        y = "Data Points",
                        fill = "Relationship")
  })
}

shinyApp(ui = ui, server = server)




