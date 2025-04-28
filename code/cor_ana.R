
# ====Merge===== # 
# Read metadata
meta_endpoints_data <- read.csv("../document/case9/Meta_endpoint_table.tsv",sep = "\t",
                                row.names = 1 # set the first column as the rownames index
)

# remove na
na_index <- is.na(as.numeric(meta_endpoints_data["Body weight (g)", ])) | is.na(as.numeric(meta_endpoints_data["Liver fat droplets (%)", ]))
no_na_index <- !na_index
sum(na_index)

head(meta_endpoints_data)

# Read otu
taxa_table <- read.csv("../document/case7/taxa_table.tsv", sep = "\t",
                      row.names = 1 # set the first column as the rownames index
                      )
# merge the data based on rownames, setting `by = "row.names"`
merged_data <- merge(t(meta_endpoints_data),t(taxa_table), by = "row.names")

# select those metadata or bacteria that you want to do correlation
# Here, we just randomly select the first 5 metafeature and first 5 bacteria otu
list_of_metafeature <- rownames(meta_endpoints_data)[1:5]
list_of_bacteria    <- rownames(taxa_table)[1:5]

# 
vector = c() 

for (i in 1:length(list_of_metafeature)){
  for (j in 1:length(list_of_bacteria)){
    res = cor.test(x = merged_data[,list_of_metafeature[i]], 
                   y = merged_data[,list_of_bacteria[j]],
                   method = "spearman" )
    
    vector2 = c(list_of_metafeature[i],
                list_of_bacteria[j],
                res$method,
                res$estimate,
                res$p.value,
                res$statistic,
                nrow(merged_data))
    vector = rbind(vector,vector2)
  }
}

colnames(vector) = c("Feature","Bacteria","Method","r","p","t-stat","sample-size")

cor_df <- as.data.frame(vector)

# For those students who want to do higher level (bacteria, genus species) analysis
# use aggregate taxa from microbiomeMarker() package
# microbiomeMarker::aggregate_taxa()

taxonomic_assign_table <- read.csv("../document/case7/taxonomic_assignment_table_revised.tsv", sep = "\t", row.names = 1) # we  will directly use the revised one

library(phyloseq)

sample_meta <- read.csv("../document/case7/sample_metadata_table.tsv",sep = "\t", row.names = 1)

physeq <- phyloseq(otu_table(taxa_table, taxa_are_rows = T), 
                   tax_table(as.matrix(taxonomic_assign_table)),
                   sample_data(sample_meta) )
physeq

genus_table <- microbiomeMarker::aggregate_taxa(x = physeq,level = "Genus")

new_taxa_table_genus <- otu_table(genus_table)

# Then you can do the same thing as what we have done in the otu part, try with your self.



