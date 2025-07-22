library(tidyverse)

setwd("/Users/User/Desktop/MOBOT_Boltonia")

psam_file <- "./../REUProject_LargeFiles/Data/PCadapt_output/Boltonia_decurrens_100kb0.8.psam"
pop_map_file <- "./Data/population_id.csv"
output_file <- "./../REUProject_LargeFiles/Data/PCadapt_output/Boltonia_decurrens_100kb0.8.withpop.psam"       # output psam file with Pop column

# Read .psam file
psam <- read.table(psam_file, header = FALSE, stringsAsFactors = FALSE)

if (ncol(psam) == 1) {
  psam <- data.frame(
    FID = psam$V1,
    IID = psam$V2,
    PAT = psam$V3,
    MAT = 0,
    SEX = 0,
    PHENOTYPE = 0,
    stringsAsFactors = FALSE
  )
} else if (ncol(psam) == 2) {
  # Assume columns are FID IID, add placeholders for others
  psam <- data.frame(
    FID = psam$V1,
    IID = as.integer(sub("Boltonia_0*", "", psam$V1)),
    PAT ="NONE",
    MAT = "NONE",
    SEX = "NONE",
    PHENOTYPE = "NONE",
    stringsAsFactors = FALSE
  )
} else {
  # If psam already has 6 or more columns, just rename the first 6 if no header
  colnames(psam)[1:6] <- c("FID", "IID", "PAT", "MAT", "SEX", "PHENOTYPE")
  psam <- psam[,1:6]  # keep only first 6 columns
}

# Read population mapping CSV
pop_map <- read.csv(pop_map_file, stringsAsFactors = FALSE)
pop_map <- pop_map |> 
  select(-Pop)

# Merge to add Pop column
psam_pop <- merge(psam, pop_map, by = "IID", all.x = TRUE)

# Fill missing Pop with "NA"
psam_pop$Sample_Group[is.na(psam_pop$Sample_Group)] <- "NA"

psam_pop <- psam_pop |> select(FID, everything())

# Write updated .psam file
write.table(psam_pop, output_file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
