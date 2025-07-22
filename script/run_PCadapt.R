library(tidyverse)
library(pcadapt)
library(qqman)
library(qvalue)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

genetic_data <- read.pcadapt("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8.bed", type = "bed")

marker_position <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8.bim", col_names = c("CHR", "ID", "FILTER", "POSITION", "REF", "ALT"), col_types = cols("i", "c", "c", "i", "c", "c"))

head(marker_position)

for (i in 2:10) {
  x <- pcadapt(input = genetic_data, K = i)
  PCadapt_out <- marker_position %>%
    mutate(pvalue = x$pvalues)%>%
    drop_na()
  
  write_tsv(PCadapt_out, paste0("./data/PCadapt_K_", i, "_20250720.txt"))
  
  p_QQ <- ggplot(data = tibble(sample = x$pvalues), aes(sample = -log(sample)))+
    stat_qq(aes(x = after_stat(theoretical)/log(10), y = after_stat(sample)/log(10)), distribution = qexp, size = 0.5)+
    stat_qq_line(aes(x = after_stat(x)/log(10), y = after_stat(y)/log(10)), distribution = qexp)+
    geom_hline(yintercept = -log10(5e-8), color = "red")+
    ggtitle(paste("PCadapt K = ", i))+
    xlab(expression("Theoretical"~"-"*log[10]*"(p)"))+
    ylab(expression("Observed"~"-"*log[10]*"(p)"))+
    theme_bw()+
    theme(panel.grid = element_blank())
  
  ggsave(paste0("PCadapt_K_", i, "_QQ.png"), width = 5, height = 5)
}



x <- pcadapt(input = genetic_data, K = 2)

plot(x, option = "manhattan")
plot(x, option = "qqplot")
names(x)

hist(x$pvalues, xlab = "p-values", main = NULL, breaks = 50, col = "orange")

x$gif
adj.pvalues <- pchisq(x$stat/1.9, df=2, lower = FALSE)
hist(adj.pvalues, xlab = "adjusted p-values", main = NULL, breaks = 50, col = "green")





plot(x, option = "screeplot")
plot(x, option = "screeplot", K = 10) + theme_bw()

ggsave("PCadapt_screeplot.png", width = 5, height = 5)

plot(x, option = "scores")
plot(x, option = "manhattan")




marker_position

PCadapt_out <- bind_cols(marker_position, tibble(pvalue = x$pvalues))

head(PCadapt_out)


write_tsv(PCadapt_out, "PCadapt_output_20230613.txt")



manhattan(PCadapt_out, chr="CHR", bp="POSITION", snp="ID", p="pvalue", main = paste("PCadapt K =", 2), highlight = AcLi_snps$snp)


plot(x, option = "qqplot")
hist(x$pvalues, xlab = "p-values", main = NULL, breaks = 50, col = "orange")
plot(x, option = "stat.distribution")


qval <- qvalue(x$pvalues)$qvalues
alpha <- 0.1
outliers <- which(qval < alpha)
length(outliers)
outliers
x$pvalues


padj <- p.adjust(x$pvalues,method="BH")
hist(padj, xlab = "p-values", main = NULL, breaks = 50, col = "orange")
alpha <- 0.1
outliers <- which(padj < alpha)
length(outliers)

padj <- p.adjust(x$pvalues,method="bonferroni")
hist(padj, xlab = "p-values", main = NULL, breaks = 50, col = "orange")
alpha <- 0.1
outliers <- which(padj < alpha)
length(outliers)
