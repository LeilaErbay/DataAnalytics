### AUTHOR LEILA ERBAY
# LOAD DATA FRAMES

#install.packages("easycsv")

library(easycsv)


total_genes <- read.delim('total_genes',
                          header = T, stringsAsFactors = FALSE, quote = "", sep = "\t")

total_contr <- read.delim('total_contr',
                          header = T, stringsAsFactors = FALSE, quote = "", sep = "\t")

num_pubs_per_gene <- read.delim('num_pubs_per_gene',
                                header = T, stringsAsFactors = FALSE, quote = "", sep = "\t")

most_freq_genes <- unique(num_pubs_per_gene[num_pubs_per_gene$X.num_pubs. > quantile(num_pubs_per_gene$X.num_pubs., .99), 1])

