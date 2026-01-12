library(tidyverse)
library(rtracklayer)
library(Biostrings)
library(UniprotR)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

genes_in_ranges <- function(ranges_tb, gff_path, feature = "gene") {
  query_gr <- ranges_tb%>%
    mutate(CHROM = paste0("Chr_", CHROM))%>%
    makeGRangesFromDataFrame(
      seqnames.field     = "CHROM",
      start.field        = "START",
      end.field          = "END",
      keep.extra.columns = TRUE
    )
  
  gff <- import(gff_path)
  genes_gr <- gff[gff$type == feature]
  
  hits <- findOverlaps(query_gr, genes_gr, ignore.strand = TRUE)
  
  tibble(
    region_ID   = mcols(query_gr)$ID[queryHits(hits)],
    region_chr  = as.character(seqnames(query_gr)[queryHits(hits)]),
    region_start = start(query_gr)[queryHits(hits)],
    region_end   = end(query_gr)[queryHits(hits)],
    gene_chr    = as.character(seqnames(genes_gr)[subjectHits(hits)]),
    gene_start  = start(genes_gr)[subjectHits(hits)],
    gene_end    = end(genes_gr)[subjectHits(hits)],
    gene_strand = as.character(strand(genes_gr)[subjectHits(hits)]),
    gene_id     = mcols(genes_gr)$ID[subjectHits(hits)],
    gene_name   = mcols(genes_gr)$Name[subjectHits(hits)]
  )
}

annotate_blast_hits <- function(
    blast_file = "./data/GWAS/GLM_sig_markers_Stem_Length_protein.out",
    get_function_fun = GetProteinFunction
) {
  # Needs: readr, dplyr, stringr, tibble loaded
  
  # 1. Read BLAST output
  blast_out <- readr::read_tsv(
    blast_file,
    col_names = c(
      "qseqid","sacc","pident","length","qcovs",
      "evalue","bitscore","stitle","staxids","sscinames","sskingdoms"
    ),
    col_types = readr::cols(
      qseqid     = readr::col_character(),
      sacc       = readr::col_character(),
      pident     = readr::col_double(),
      length     = readr::col_integer(),
      qcovs      = readr::col_double(),
      evalue     = readr::col_double(),
      bitscore   = readr::col_double(),
      stitle     = readr::col_character(),
      staxids    = readr::col_character(),
      sscinames  = readr::col_character(),
      sskingdoms = readr::col_character()
    )
  )
  
  # 2. Keep best hit (max bitscore) per query and parse UniProt fields
  best_hits <- blast_out %>%
    dplyr::group_by(qseqid) %>%
    dplyr::slice_max(order_by = bitscore, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # accession and entry name from the first chunk
      uniprot_acc   = stringr::str_match(stitle, "^[^|]+\\|([^|]+)\\|")[,2],
      uniprot_entry = stringr::str_match(stitle, "^[^|]+\\|[^|]+\\|([^ ]+)")[,2],
      
      # full description after the first space
      full_desc = stringr::str_replace(stitle, "^[^ ]+\\s+", ""),
      
      # protein name (before OS=...)
      protein_name = stringr::str_squish(stringr::str_replace(full_desc, " OS=.*$", "")),
      
      # organism, taxid, gene, PE, SV
      organism = stringr::str_match(stitle, " OS=([^=]+?) OX=")[,2],
      taxid    = stringr::str_match(stitle, " OX=(\\d+)")[,2],
      gene     = stringr::str_match(stitle, " GN=([^ =]+)")[,2],
      PE       = stringr::str_match(stitle, " PE=(\\d+)")[,2],
      SV       = stringr::str_match(stitle, " SV=(\\d+)")[,2]
    ) %>%
    dplyr::select(
      qseqid,
      gene,
      protein_name,
      uniprot_acc,
      sacc,
      organism,
      taxid,
      pident,
      qcovs,
      evalue,
      bitscore,
      stitle,
      sscinames,
      sskingdoms,
      PE,
      SV,
      dplyr::everything()
    )
  
  # 3. Get UniProt functional annotations
  ids <- best_hits$uniprot_acc
  
  func_df <- get_function_fun(ids)  # e.g. GetProteinFunction()
  
  func_df2 <- func_df %>%
    tibble::rownames_to_column(var = "uniprot_acc") %>%
    dplyr::select(uniprot_acc, `Function..CC.`)
  
  # 4. Join back to best_hits and clean up Function text
  best_hits_annot <- best_hits %>%
    dplyr::left_join(func_df2, by = "uniprot_acc") %>%
    dplyr::select(
      qseqid,
      gene,
      protein_name,
      uniprot_acc,
      organism,
      Function = `Function..CC.`,
      sacc,
      taxid,
      pident,
      qcovs,
      evalue,
      bitscore,
      stitle,
      sscinames,
      sskingdoms,
      PE,
      SV,
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      Function = stringr::str_remove(Function, "^FUNCTION: ")
    )
  
  return(best_hits_annot)
}


intersect_decurrens_tb <- read_tsv("./data/asteroides_decurrens/intersect_decurrens_tb.tsv")
result_tb <- genes_in_ranges(intersect_decurrens_tb, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")


# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

# writeXStringSet(sel, filepath = "./data/asteroides_decurrens/intersect_decurrens_window_proteins.fa")

blast_out <- read_tsv(
  "./data/asteroides_decurrens/intersect_decurrens_window_proteins.out",
  col_names = c(
    "qseqid","sacc","pident","length","qcovs",
    "evalue","bitscore","stitle","staxids","sscinames","sskingdoms"
  ),
  col_types = cols(
    qseqid     = col_character(),
    sacc       = col_character(),
    pident     = col_double(),
    length     = col_integer(),
    qcovs      = col_double(),
    evalue     = col_double(),
    bitscore   = col_double(),
    stitle     = col_character(),
    staxids    = col_character(),
    sscinames  = col_character(),
    sskingdoms = col_character()
  )
)

best_hits <- blast_out %>%
  group_by(qseqid) %>%
  slice_max(order_by = bitscore, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    # accession and entry name from the first chunk
    uniprot_acc   = str_match(stitle, "^[^|]+\\|([^|]+)\\|")[,2],
    uniprot_entry = str_match(stitle, "^[^|]+\\|[^|]+\\|([^ ]+)")[,2],
    
    # full description after the first space
    full_desc = str_replace(stitle, "^[^ ]+\\s+", ""),
    
    # protein name (before OS=...)
    protein_name = str_squish(str_replace(full_desc, " OS=.*$", "")),
    
    # organism, taxid, gene, PE, SV
    organism = str_match(stitle, " OS=([^=]+?) OX=")[,2],
    taxid    = str_match(stitle, " OX=(\\d+)")[,2],
    gene     = str_match(stitle, " GN=([^ =]+)")[,2],
    PE       = str_match(stitle, " PE=(\\d+)")[,2],
    SV       = str_match(stitle, " SV=(\\d+)")[,2]
  )%>%
  select(
    qseqid,
    gene,
    protein_name,
    uniprot_acc,
    sacc,
    organism,
    taxid,
    pident,
    qcovs,
    evalue,
    bitscore,
    stitle,
    sscinames,
    sskingdoms,
    PE,
    SV,
    everything()
  )

colnames(best_hits)

ids <- best_hits$uniprot_acc

func_df <- GetProteinFunction(ids)        # function text

# 1. Move rownames (UniProt IDs) into a column
func_df2 <- func_df %>%
  rownames_to_column(var = "uniprot_acc") %>% 
  select(uniprot_acc, `Function..CC.`)

# 2. Left-join onto your best_hits table
best_hits_annot <- best_hits %>%
  left_join(func_df2, by = "uniprot_acc")%>%
  select(
    qseqid,
    gene,
    protein_name,
    uniprot_acc,
    organism,
    Function = `Function..CC.`,
    sacc,
    taxid,
    pident,
    qcovs,
    evalue,
    bitscore,
    stitle,
    sscinames,
    sskingdoms,
    PE,
    SV,
    everything()
  )%>%
  mutate(Function = str_remove(Function, "FUNCTION: "))

interp <- read.delim("./data/asteroides_decurrens/tier1_interpretation.tsv", stringsAsFactors = FALSE)%>%
  select(qseqid, niche_module, concise_function, floodplain_vs_upland_interpretation)
  
merged <- merge(best_hits_annot[,1:6], interp, by = "qseqid", all.x = TRUE)%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))

write_tsv(merged, "./data/asteroides_decurrens/intersect_decurrens_window_proteins_function.tsv")
writexl::write_xlsx(merged, "./data/asteroides_decurrens/intersect_decurrens_window_proteins_function.xlsx")

merged_sig <- merged %>% 
  filter(!is.na(niche_module))%>%
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, niche_module, concise_function, floodplain_vs_upland_interpretation)

writexl::write_xlsx(merged_sig, "./data/asteroides_decurrens/intersect_decurrens_window_proteins_function_candidate.xlsx")

###############################
###############################
# Stem_Length

intersect_decurrens_tb <- read_tsv("./data/GWAS/GLM_sig_markers_all_window.tsv")%>%
  filter(GWAS_TRAIT == "Stem_Length")

result_tb <- genes_in_ranges(intersect_decurrens_tb, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")%>%
  group_by(gene_chr, gene_start, gene_end, gene_strand, gene_id)%>%
  summarise_all(.funs = "first")


# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

# writeXStringSet(sel, filepath = "./data/GWAS/GLM_sig_markers_Stem_Length_protein.tsv")


blast_out <- read_tsv(
  "./data/GWAS/GLM_sig_markers_Stem_Length_protein.out",
  col_names = c(
    "qseqid","sacc","pident","length","qcovs",
    "evalue","bitscore","stitle","staxids","sscinames","sskingdoms"
  ),
  col_types = cols(
    qseqid     = col_character(),
    sacc       = col_character(),
    pident     = col_double(),
    length     = col_integer(),
    qcovs      = col_double(),
    evalue     = col_double(),
    bitscore   = col_double(),
    stitle     = col_character(),
    staxids    = col_character(),
    sscinames  = col_character(),
    sskingdoms = col_character()
  )
)

best_hits <- blast_out %>%
  group_by(qseqid) %>%
  slice_max(order_by = bitscore, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    # accession and entry name from the first chunk
    uniprot_acc   = str_match(stitle, "^[^|]+\\|([^|]+)\\|")[,2],
    uniprot_entry = str_match(stitle, "^[^|]+\\|[^|]+\\|([^ ]+)")[,2],
    
    # full description after the first space
    full_desc = str_replace(stitle, "^[^ ]+\\s+", ""),
    
    # protein name (before OS=...)
    protein_name = str_squish(str_replace(full_desc, " OS=.*$", "")),
    
    # organism, taxid, gene, PE, SV
    organism = str_match(stitle, " OS=([^=]+?) OX=")[,2],
    taxid    = str_match(stitle, " OX=(\\d+)")[,2],
    gene     = str_match(stitle, " GN=([^ =]+)")[,2],
    PE       = str_match(stitle, " PE=(\\d+)")[,2],
    SV       = str_match(stitle, " SV=(\\d+)")[,2]
  )%>%
  select(
    qseqid,
    gene,
    protein_name,
    uniprot_acc,
    sacc,
    organism,
    taxid,
    pident,
    qcovs,
    evalue,
    bitscore,
    stitle,
    sscinames,
    sskingdoms,
    PE,
    SV,
    everything()
  )

colnames(best_hits)

ids <- best_hits$uniprot_acc

func_df <- GetProteinFunction(ids)        # function text

# 1. Move rownames (UniProt IDs) into a column
func_df2 <- func_df %>%
  rownames_to_column(var = "uniprot_acc") %>% 
  select(uniprot_acc, `Function..CC.`)

# 2. Left-join onto your best_hits table
best_hits_annot <- best_hits %>%
  left_join(func_df2, by = "uniprot_acc")%>%
  select(
    qseqid,
    gene,
    protein_name,
    uniprot_acc,
    organism,
    Function = `Function..CC.`,
    sacc,
    taxid,
    pident,
    qcovs,
    evalue,
    bitscore,
    stitle,
    sscinames,
    sskingdoms,
    PE,
    SV,
    everything()
  )%>%
  mutate(Function = str_remove(Function, "FUNCTION: "))

interp <- read.delim("./data/GWAS/Stem_Length_interpretation.tsv", stringsAsFactors = FALSE)%>%
  dplyr::filter(tier == "Tier1")%>%
  select(qseqid, Stem_Length_interpretation = rationale)

merged <- left_join(interp, best_hits_annot[,1:6], by = "qseqid")%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))

write_tsv(best_hits_annot, "./data/GWAS/GLM_sig_markers_Stem_Length_protein_function.tsv")
writexl::write_xlsx(merged, "./data/GWAS/GLM_sig_markers_Stem_Length_protein_function.xlsx")

merged_sig <- merged %>% 
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, Function , Stem_Length_interpretation)

writexl::write_xlsx(merged_sig, "./data/GWAS/GLM_sig_markers_Stem_Length_protein_function_candidate.xlsx")

##########################################################################################
# FlowerDays_2025

intersect_decurrens_tb <- read_tsv("./data/GWAS/GLM_sig_markers_all_window.tsv")%>%
  filter(GWAS_TRAIT == "FlowerDays_2025")

result_tb <- genes_in_ranges(intersect_decurrens_tb, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")%>%
  group_by(gene_chr, gene_start, gene_end, gene_strand, gene_id)%>%
  summarise_all(.funs = "first")

# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

# writeXStringSet(sel, filepath = "./data/GWAS/GLM_sig_markers_FlowerDays_2025_protein.tsv")

best_hits_annot <- annotate_blast_hits("./data/GWAS/GLM_sig_markers_FlowerDays_2025_protein.out")


interp <- read.delim("./data/GWAS/FlowerDays_2025_interpretation.tsv", stringsAsFactors = FALSE)%>%
  select(qseqid, concise_function,	Flowering_time_interpretation)

merged <- left_join(best_hits_annot[,1:6], interp, by = "qseqid")%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))

write_tsv(best_hits_annot, "./data/GWAS/GLM_sig_markers_FlowerDays_2025_protein_function.tsv")
writexl::write_xlsx(merged, "./data/GWAS/GLM_sig_markers_FlowerDays_2025_protein_function.xlsx")

merged_sig <- merged %>% 
  filter(!is.na(Flowering_time_interpretation))%>%
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, Function , Flowering_time_interpretation)

writexl::write_xlsx(merged_sig, "./data/GWAS/GLM_sig_markers_FlowerDays_2025_protein_function_candidate.xlsx")



########################
########################
# Num_Stems

intersect_decurrens_tb <- read_tsv("./data/GWAS/GLM_sig_markers_all_window.tsv")%>%
  filter(GWAS_TRAIT == "Num_Stems")

result_tb <- genes_in_ranges(intersect_decurrens_tb, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")%>%
  group_by(gene_chr, gene_start, gene_end, gene_strand, gene_id)%>%
  summarise_all(.funs = "first")

# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

writeXStringSet(sel, filepath = "./data/GWAS/GLM_sig_markers_Num_Stems_protein.tsv")

best_hits_annot <- annotate_blast_hits("./data/GWAS/GLM_sig_markers_Num_Stems_protein.out")


interp <- read.delim("./data/GWAS/Num_Stems_interpretation.tsv", stringsAsFactors = FALSE)%>%
  select(qseqid, concise_function,	Flowering_stem_interpretation)

merged <- left_join(best_hits_annot[,1:6], interp, by = "qseqid")%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))

write_tsv(best_hits_annot, "./data/GWAS/GLM_sig_markers_Num_Stems_protein_function.tsv")
writexl::write_xlsx(merged, "./data/GWAS/GLM_sig_markers_Num_Stems_protein_function.xlsx")

merged_sig <- merged %>% 
  filter(!is.na(Flowering_stem_interpretation))%>%
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, Function , Flowering_stem_interpretation)

writexl::write_xlsx(merged_sig, "./data/GWAS/GLM_sig_markers_Num_Stems_protein_function_candidate.xlsx")

##########################################
########################
# LFMM + PCadapt

intersect_decurrens_tb <- read_tsv("./data/LFMM_PCadapt/LFMM_PCadapt_sig.tsv")%>%
  mutate(CHROM = chr, START = POSITION - 5000L, END = POSITION + 5000L)

result_tb <- genes_in_ranges(intersect_decurrens_tb, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")%>%
  group_by(gene_chr, gene_start, gene_end, gene_strand, gene_id)%>%
  summarise_all(.funs = function(x){dplyr::first(x)})

# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

writeXStringSet(sel, filepath = "./data/LFMM_PCadapt/LFMM_PCadapt_sig_protein.tsv")

best_hits_annot <- annotate_blast_hits("./data/LFMM_PCadapt/LFMM_PCadapt_sig_protein.out")


interp <- read_csv("./data/LFMM_PCadapt/LFMM_PCadapt_interpretation.csv")%>%
  select(qseqid, concise_function,	Interpretation = `why candidate`)

merged <- left_join(best_hits_annot[,1:6], interp, by = "qseqid")%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))%>%
  arrange(gene_chr, gene_start)

write_tsv(best_hits_annot[,1:6], "./data/LFMM_PCadapt/LFMM_PCadapt_sig_protein_function.tsv")
writexl::write_xlsx(merged, "./data/LFMM_PCadapt/LFMM_PCadapt_sig_protein_function.xlsx")

merged_sig <- merged %>% 
  filter(!is.na(Interpretation))%>%
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, Function , Interpretation)

writexl::write_xlsx(merged_sig, "./data/LFMM_PCadapt/LFMM_PCadapt_sig_protein_function_candidate.xlsx")

##########################################
########################
# BayPass

intersect_decurrens_tb <- read_tsv("./data/BayPass/intersect_sig_tbl.tsv")%>%
  select(CHROM = CHR, POSITION = POS)%>%
  mutate(START = POSITION - 5000L, END = POSITION + 5000L)

result_tb <- genes_in_ranges(intersect_decurrens_tb, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")%>%
  group_by(gene_chr, gene_start, gene_end, gene_strand, gene_id)%>%
  summarise_all(.funs = function(x){dplyr::first(x)})

# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

writeXStringSet(sel, filepath = "./data/BayPass/BayPass_sig_protein.tsv")

best_hits_annot <- annotate_blast_hits("./data/BayPass/BayPass_sig_protein.out")


interp <- read_delim("./data/BayPass/BayPass_interpretation.tsv", delim = "|", trim_ws = TRUE)[,2:5]%>%
  select(qseqid, concise_function,	Local_adaptation_interpretation)

merged <- left_join(best_hits_annot[,1:6], interp, by = "qseqid")%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))

write_tsv(best_hits_annot[,1:6], "./data/BayPass/BayPass_sig_protein_function.tsv")
writexl::write_xlsx(merged, "./data/BayPass/BayPass_sig_protein_function.xlsx")

merged_sig <- merged %>% 
  filter(!is.na(Local_adaptation_interpretation))%>%
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, Function , Local_adaptation_interpretation)

writexl::write_xlsx(merged_sig, "./data/BayPass/BayPass_sig_protein_function_candidate.xlsx")

##########################################
##########################################
########################
# Structural variation on Chr_2

intersect_decurrens_tb <- tibble(CHROM = 1,
                                 START = 17.5e6,
                                 END = 19e6)
  
result_tb <- genes_in_ranges(intersect_decurrens_tb, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")%>%
  group_by(gene_chr, gene_start, gene_end, gene_strand, gene_id)%>%
  summarise_all(.funs = function(x){dplyr::first(x)})

# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

writeXStringSet(sel, filepath = "./data/BayPass/structural_variant_protein.tsv")

best_hits_annot <- annotate_blast_hits("./data/BayPass/structural_variant_protein.out")


adapt_interp <- read_tsv("./data/BayPass/adaptation_candidates.tsv")%>%
  select(qseqid, concise_function,	adaptation_relevance)

merged <- left_join(best_hits_annot[,1:6], adapt_interp, by = "qseqid")%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))

write_tsv(merged, "./data/BayPass/structural_adaptation_candidates.tsv")
writexl::write_xlsx(merged, "./data/BayPass/structural_adaptation_candidates.xlsx")

merged_sig <- merged %>% 
  filter(!is.na(adaptation_relevance))%>%
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, Function, concise_function,adaptation_relevance)

writexl::write_xlsx(merged_sig, "./data/BayPass/structural_adaptation_candidates.xlsx")

structure_interp <- read_tsv("./data/BayPass/structural_variant_mechanism.tsv")%>%
  select(qseqid, concise_function,	SV_mechanism_relevance)

merged <- left_join(best_hits_annot[,1:6], structure_interp, by = "qseqid")%>%
  left_join(result_tb, by = c("qseqid" = "gene_id"))

write_tsv(merged, "./data/BayPass/structural_mechanism_candidates.tsv")
writexl::write_xlsx(merged, "./data/BayPass/structural_mechanism_candidates.xlsx")

merged_sig <- merged %>% 
  filter(!is.na(SV_mechanism_relevance))%>%
  select(qseqid, gene, gene_chr, gene_start, gene_end, gene_strand,protein_name, uniprot_acc, organism, Function, concise_function,SV_mechanism_relevance)

writexl::write_xlsx(merged_sig, "./data/BayPass/structural_mechanism_candidates.xlsx")

##########################################

# 1. Unique gene IDs from your result tibble
gene_ids <- unique(result_tb$gene_id)

# 2. Read your protein FASTA file
#    Change this path to your actual file
prot <- readAAStringSet("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_protein.fasta")
# strip transcript suffix like .t1, .t2
prot_gene_id <- sub("\\.t\\d+$", "", names(prot))

# one transcript per gene: first match
keep_idx <- match(gene_ids, prot_gene_id)
sel <- prot[keep_idx[!is.na(keep_idx)]]

# rename to plain gene IDs
names(sel) <- gene_ids[!is.na(keep_idx)]

writeXStringSet(sel, filepath = "./data/GWAS/GLM_sig_markers_Num_Stems_protein.tsv")
###########
blast_out <- read_tsv(
  "./data/asteroides_decurrens/intersect_decurrens_window_proteins.out",
  col_names = c(
    "qseqid","sacc","pident","length","qcovs",
    "evalue","bitscore","stitle","staxids","sscinames","sskingdoms"
  ),
  col_types = cols(
    qseqid     = col_character(),
    sacc       = col_character(),
    pident     = col_double(),
    length     = col_integer(),
    qcovs      = col_double(),
    evalue     = col_double(),
    bitscore   = col_double(),
    stitle     = col_character(),
    staxids    = col_character(),
    sscinames  = col_character(),
    sskingdoms = col_character()
  )
)

best_hits <- blast_out %>%
  group_by(qseqid) %>%
  slice_max(order_by = bitscore, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    # accession and entry name from the first chunk
    uniprot_acc   = str_match(stitle, "^[^|]+\\|([^|]+)\\|")[,2],
    uniprot_entry = str_match(stitle, "^[^|]+\\|[^|]+\\|([^ ]+)")[,2],
    
    # full description after the first space
    full_desc = str_replace(stitle, "^[^ ]+\\s+", ""),
    
    # protein name (before OS=...)
    protein_name = str_squish(str_replace(full_desc, " OS=.*$", "")),
    
    # organism, taxid, gene, PE, SV
    organism = str_match(stitle, " OS=([^=]+?) OX=")[,2],
    taxid    = str_match(stitle, " OX=(\\d+)")[,2],
    gene     = str_match(stitle, " GN=([^ =]+)")[,2],
    PE       = str_match(stitle, " PE=(\\d+)")[,2],
    SV       = str_match(stitle, " SV=(\\d+)")[,2]
  )%>%
  select(
    qseqid,
    gene,
    protein_name,
    uniprot_acc,
    sacc,
    organism,
    taxid,
    pident,
    qcovs,
    evalue,
    bitscore,
    stitle,
    sscinames,
    sskingdoms,
    PE,
    SV,
    everything()
  )

colnames(best_hits)

ids <- best_hits$uniprot_acc

func_df <- GetProteinFunction(ids)        # function text

# 1. Move rownames (UniProt IDs) into a column
func_df2 <- func_df %>%
  rownames_to_column(var = "uniprot_acc") %>% 
  select(uniprot_acc, `Function..CC.`)

# 2. Left-join onto your best_hits table
best_hits_annot <- best_hits %>%
  left_join(func_df2, by = "uniprot_acc")%>%
  select(
    qseqid,
    gene,
    protein_name,
    uniprot_acc,
    organism,
    Function = `Function..CC.`,
    sacc,
    taxid,
    pident,
    qcovs,
    evalue,
    bitscore,
    stitle,
    sscinames,
    sskingdoms,
    PE,
    SV,
    everything()
  )%>%
  mutate(Function = str_remove(Function, "FUNCTION: "))

interp <- read.delim("./data/asteroides_decurrens/tier1_interpretation.tsv", stringsAsFactors = FALSE)%>%
  select(qseqid, niche_module, concise_function, floodplain_vs_upland_interpretation)

merged <- merge(best_hits_annot[,1:6], interp, by = "qseqid", all.x = TRUE)

write_tsv(merged, "./data/asteroides_decurrens/intersect_decurrens_window_proteins_function.tsv")
writexl::write_xlsx(merged, "./data/asteroides_decurrens/intersect_decurrens_window_proteins_function.xlsx")
##########################
gff <- import("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3")  # <-- change this path

# keep only gene features (change "gene" to "mRNA" or whatever feature you want)
genes_gr <- gff[gff$type == "gene"]

query_gr <- read_tsv("./data/asteroides_decurrens/intersect_decurrens_tb.tsv")%>%
  mutate(CHROM = paste0("Chr_", CHROM))%>%
  makeGRangesFromDataFrame(
    seqnames.field     = "CHROM",
    start.field        = "START",
    end.field          = "END",
    keep.extra.columns = TRUE
  )

hits <- findOverlaps(query_gr, genes_gr, ignore.strand = TRUE)

result_tb <- tibble(
  region_ID   = mcols(query_gr)$ID[queryHits(hits)],
  region_chr  = as.character(seqnames(query_gr)[queryHits(hits)]),
  region_start = start(query_gr)[queryHits(hits)],
  region_end   = end(query_gr)[queryHits(hits)],
  gene_chr    = as.character(seqnames(genes_gr)[subjectHits(hits)]),
  gene_start  = start(genes_gr)[subjectHits(hits)],
  gene_end    = end(genes_gr)[subjectHits(hits)],
  gene_strand = as.character(strand(genes_gr)[subjectHits(hits)]),
  gene_id     = mcols(genes_gr)$ID[subjectHits(hits)],    # attribute "ID" from GFF
  gene_name   = mcols(genes_gr)$Name[subjectHits(hits)]   # if "Name" exists
)

result_tb
