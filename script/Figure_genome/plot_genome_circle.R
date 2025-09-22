library(tinyfuncr)
library(tidyverse)
library(seqinr) 
library(R.utils)
library(circlize)
library(Polychrome)
library(dichromat)
library(colorspace)

mypal <- kelly.colors(9)
swatch(mypal)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

fasta_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0.fasta"

gff3_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0_braker.gff3"

fai_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0.fasta.fai"

repeat_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0.fasta.out.gff"

karyotype <- read_tsv(fai_file, col_names = FALSE)%>%
  mutate(START = 1)%>%
  select(X1, START, X2)
write_tsv(karyotype, paste0(fasta_file, ".karyotype"), col_names = FALSE)

cytoband <- read_tsv(fai_file, col_names = FALSE)%>%
  mutate(START = 0)%>%
  select(X1, START, X2)%>%
  mutate(X4=NA, X5=NA)
write_tsv(cytoband, paste0(fasta_file, ".cytoband"), col_names = FALSE)

cytoband_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0.fasta.cytoband"

karyotype_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0.fasta.karyotype"

hap1_gene_density <- calcDensityGFF(gff3_file, karyotype_file, feature = "gene", window = 5e+05)


hap1_gene_density_f <- hap1_gene_density %>%
  filter(!str_detect(Chr, "h1tg"))%>%
  mutate(Chr = factor(Chr, levels = str_sort(unique(Chr), numeric = TRUE)))

p <- ggplot(data = hap1_gene_density_f, aes(x = (Start + End)/2, ymax = Count))+
  geom_ribbon(ymin = 0, fill = "blue", alpha = 0.8)+
  scale_y_continuous(limits = c(0,100))+
  theme_bw()+
  facet_wrap(.~Chr)

p


hap1_repeat_density <- calcDensityGFF(repeat_file, karyotype_file, feature = "dispersed_repeat", window = 5e+05)


hap1_repeat_density_f <- hap1_repeat_density %>%
  filter(!str_detect(Chr, "h1tg"))%>%
  mutate(Chr = factor(Chr, levels = str_sort(unique(Chr), numeric = TRUE)))%>%
  filter(Count > 50)

p <- ggplot(data = hap1_repeat_density_f, aes(x = (Start + End)/2, ymax = Count))+
  geom_ribbon(ymin = 0, fill = "blue", alpha = 0.8)+
  scale_y_continuous()+
  theme_bw()+
  facet_wrap(.~Chr)

p

refseq=read.fasta(fasta_file)

get_GC <- function(x = refseq, window_size=5e5){
  refseq_names=names(x)
  
  get_GC_chr <- function(i, window_size, refseq_names, x = refseq){
    # i=1
    Chr <- c()
    Start <- c()
    End <- c()
    GC_content <- c()
    
    fasta=x[[i]]
    fasta_name=refseq_names[[i]]
    fasta_length=length(fasta)-1
    
    mod_window_size=ifelse(fasta_length < window_size, fasta_length, window_size)
    starts <- seq(1, length(fasta), by = window_size)
    ends <- ifelse((starts + mod_window_size - 1) <= fasta_length, (starts + mod_window_size - 1), fasta_length + 1)
    
    n <- length(starts)    # Find the length of the vector "starts"
    
    for (i in 1:n) {
      chunk <- fasta[starts[i]:ends[i]]
      chunkGC <- GC(chunk)
      Start <- append(Start, starts[i])
      End <- append(End, ends[i])
      GC_content <- append(GC_content, ifelse(is.na(chunkGC),0,chunkGC))
    }
    output <- tibble(Chr = fasta_name, Start = Start, End = End, GC_content = GC_content)
    return(output)
  }
  
  hap1_GC_content <- list()
  
  for (i in 1:length(x)){
    print(i)
    hap1_GC_content[[i]] <- get_GC_chr(i, window_size, refseq_names, x)
  }

  return(bind_rows(hap1_GC_content))
}

hap1_GC_content <- get_GC()

hap1_GC_content_f <- hap1_GC_content %>%
  filter(!str_detect(Chr, "h1tg"))%>%
  mutate(Chr = factor(Chr, levels = str_sort(unique(Chr), numeric = TRUE)))

p <- ggplot(data = hap1_GC_content_f, aes(x = (Start + End)/2, ymax = GC_content))+
  geom_ribbon(ymin = 0, fill = "blue", alpha = 0.8)+
  scale_y_continuous(limits = c(0.3,0.375))+
  theme_bw()+
  facet_wrap(.~Chr)

p

# synteny_data <- read_tsv("/Users/kuowenhsi/Genespace/syntenicBlocks_default.txt")%>%
#   filter(gen1 == "Trepens", gen2 == "Trepens") %>%
#   select(chr1, chr2, startBp1, endBp1, startBp2, endBp2) %>%
#   filter(chr1 != chr2)%>%
#   group_by(chr1, chr2, startBp1, endBp1, startBp2, endBp2)%>%
#   mutate(color = str_sort(c(chr1, chr2), numeric = TRUE)[[1]])%>%
#   ungroup()%>%
#   mutate(color_index = as.integer(str_remove(color, "drTriRepe4Chr")))
# 
# synteny_bed1 <- synteny_data %>%
#   select(chr = chr1, start = startBp1, end = endBp1, color, color_index)
# 
# synteny_bed2 <- synteny_data %>%
#   select(chr = chr2, start = startBp2, end = endBp2, color, color_index)

#################

colByChrs <- c("#62322E", "#C60000", "#FF7500", "#FEDF99", "#BEF3F9",
               "#43B8FF", "#204DBF", "#9C63E1", "#F4BDFF")

genespace_color <- colorRampPalette(colByChrs)(8)

#################


chr_label <- read_tsv(cytoband_file, col_names = c("Chr", "Start", "End", "X1", "X2"))%>%
  filter(!str_detect(Chr, "h1tg"))

# text_color <- c("black", "white", "black", rep("white", 13))
rec_color <- rep("darkolivegreen2", 9)

png("./figures/Boltonia_hap1_1.0_genome_circle_small.png", width = 5, height = 5, units = "in", res = 600)

circos.par("track.height" = 0.1, start.degree = 90, track.margin = c(0.005, 0.005), gap.after=c(rep(1, 8), 16))
circos.initializeWithIdeogram(cytoband = cytoband_file, plotType = c("axis"), chromosome.index = c(paste0("Chr_", 1:9)))

# Write text at center using base R graphics
text(0, 0, "Boltonia decurrens\n437.8 Mbp\nN50 = 47 Mbp", cex = 1, font = 2)


circos.info()
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  chr = CELL_META$sector.index
  xlim = CELL_META$xlim
  ylim = CELL_META$ylim
  circos.rect(xlim[1], 0, xlim[2], 1, col = rec_color[[CELL_META$sector.numeric.index]], border = "gray65")
  circos.text(mean(xlim), mean(ylim), str_remove(chr, "Chr_"), cex = 0.7, col = "black",
              facing = "inside", niceFacing = TRUE)
}, track.height = 0.07, bg.border = NA)

#First label, depending on final plot resolution and gap size you'll have to tune the positions
circos.text(sector.index="Chr_1",track.index = 2,
            0,
            get.cell.meta.data("ycenter"), 
            labels = "Chr",facing = "downward", 
            niceFacing = TRUE, pos = 2, offset = 0.1,cex = 0.6)

circos.track(ylim = c(0.3, 0.375), panel.fun = function(x, y) {
  hap1_GC_content_f_chr <- hap1_GC_content_f[hap1_GC_content_f$Chr == CELL_META$sector.index,]
  circos.lines(x = (hap1_GC_content_f_chr$Start + hap1_GC_content_f_chr$End)/2, y = hap1_GC_content_f_chr$GC_content, type = "l", col = "deepskyblue3", area = TRUE, border = NA)
}, track.height = 0.08, bg.border = "gray65")

circos.text(sector.index="Chr_1",track.index = 3,
            0,
            get.cell.meta.data("ycenter"), 
            labels = "GC",facing = "downward", 
            niceFacing = TRUE, pos = 2, offset = 0.1,cex = 0.6)

circos.track(ylim = c(0, 75), panel.fun = function(x, y) {
  hap1_gene_density_f_chr <- hap1_gene_density_f[hap1_gene_density_f$Chr == CELL_META$sector.index,]
  circos.lines(x = (hap1_gene_density_f_chr$Start + hap1_gene_density_f_chr$End)/2, y = hap1_gene_density_f_chr$Count, type = "l", col = "chartreuse4", area = TRUE, border = NA)
}, track.height = 0.08, bg.border = "gray65")

circos.text(sector.index="Chr_1",track.index = 4,
            0,
            get.cell.meta.data("ycenter"), 
            labels = "Gene",facing = "downward", 
            niceFacing = TRUE, pos = 2, offset = 0.1, cex = 0.6)

circos.track(ylim = c(496, 1557), panel.fun = function(x, y) {
  hap1_repeat_density_f_chr <- hap1_repeat_density_f[hap1_repeat_density_f$Chr == CELL_META$sector.index,]
  circos.lines(x = (hap1_repeat_density_f_chr$Start + hap1_repeat_density_f_chr$End)/2, y = hap1_repeat_density_f_chr$Count, type = "l", col = "darkgoldenrod4", area = TRUE, border = NA)
}, track.height = 0.08, bg.border = "gray65")

circos.text(sector.index="Chr_1",track.index = 5,
            0,
            get.cell.meta.data("ycenter"), 
            labels = "Repeat",facing = "downward", 
            niceFacing = TRUE, pos = 2, offset = 0.1,cex = 0.6)


# circos.genomicLink(synteny_bed1, synteny_bed2, border = NA, col = adjust_transparency(genespace_color[synteny_bed1$color_index], 0.3))

circos.clear()

dev.off()
