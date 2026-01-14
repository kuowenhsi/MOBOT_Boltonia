library(tidyverse)
require("argparse")
require("scales")

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

parser <- ArgumentParser(description = "Make spectra-cn plots. Line, filled, and stacked spectra-cn plots will be generated.")
parser$add_argument("-x", "--xdim", type="double", default=6, help="width of plot [default %(default)s]")
parser$add_argument("-y", "--ydim", type="double", default=5, help="height of plot [default %(default)s]")
parser$add_argument("-m", "--xmax", type="integer", default=0, help="maximum limit for k-mer multiplicity [default (x where y=peak) * 2.1]")
parser$add_argument("-n", "--ymax", type="integer", default=0, help="maximum limit for k-mer count [default (y where y=peak) * 1.1]")

unique(spectra_asm$Assembly)

spectra_asm <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Merqury/Boltonia_435_merqury.spectra-asm.hist")%>%
  mutate(Assembly = factor(Assembly, levels = c("read-only", "Boltonia_hap1_1.0-only", "Boltonia_hap2_1.0-only", "shared"),
                           labels = c("Read-only", "Primary haplotig-only", "Alternative haplotig-only", "Shared")))

fancy_scientific <- function(d) {
  # turn in to character string in scientific notation
  d <- format(d, scientific = TRUE)
  # quote the part before the exponent to keep all the digits and turn the 'e+' into 10^ format
  d <- gsub("^(.*)e\\+", "'\\1'%*%10^", d)
  # convert 0x10^00 to 0
  d <- gsub("\\'0[\\.0]*\\'(.*)", "'0'", d)
  # return this as an expression
  parse(text=d)
}

gray = "black"
red = "#E41A1C"
blue = "#377EB8" # light blue = "#56B4E9"
green = "#4DAF4A"
purple = "#984EA3"  # purple = "#CC79A7"
orange = "#FF7F00"  # orange = "#E69F00"
yellow = "#FFFF33"

p <- ggplot(data=spectra_asm, aes(x=kmer_multiplicity, y=Count)) +
  geom_ribbon(aes(ymin=0, ymax=pmax(Count,0), fill=Assembly, colour=Assembly), alpha=0.4, linetype=1) +
  # plot_zero_fill(zero=zero) +
  # plot_cutoff(cutoff) +
  theme_bw() +
  theme(legend.position = c(0.75,0.83), legend.title = element_blank(),
        panel.grid = element_blank())+
  # theme(legend.text = element_text(size=11),
  #       legend.position = c(0.95,0.95),  # Modify this if the legend is covering your favorite circle
  #       legend.background = element_rect(size=0.1, linetype="solid", colour ="black"),
  #       legend.box.just = "right",
  #       legend.justification = c("right", "top"),
  #       axis.title=element_text(size=14,face="bold"),
  #       axis.text=element_text(size=12))+
  scale_color_manual(values = c(gray, red, blue, green, purple, orange), name="") +
  scale_fill_manual(values = c(gray, red, blue, green, purple, orange), name="") +
  scale_x_continuous("k-mer multiplicity")+
  scale_y_continuous(expression("Count (1 x "*10^6~")"), labels=function(x)format(x/1e6, nsmall = 1)) +
  coord_cartesian(xlim=c(0,37), ylim=c(0,15e6), expand = FALSE)
p

ggsave("./figures/Boltonia_diploid_spectra_asm.png", height = 5, width = 5, dpi = 600)


##############

Hap1_1.4_Hap2_1.1.spectra <- read_tsv("Hap1_1.4_Hap2_1.1.spectra-cn.hist")%>%
  mutate(Copies = factor(Copies, levels = c("read-only", "1", "2", "3", "4", ">4"),
                           labels = c("Read-only", "1 copy", "2 copies", "3 copies", "4 copies", "> 4 copies")))



p <- ggplot(data=Hap1_1.4_Hap2_1.1.spectra, aes(x=kmer_multiplicity, y=Count)) +
  geom_ribbon(aes(ymin=0, ymax=pmax(Count,0), fill=Copies, colour=Copies), alpha=0.4, linetype=1) +
  # plot_zero_fill(zero=zero) +
  # plot_cutoff(cutoff) +
  theme_bw() +
  theme(legend.position = c(0.75,0.80), legend.title = element_blank(),
        panel.grid = element_blank())+
  # theme(legend.text = element_text(size=11),
  #       legend.position = c(0.95,0.95),  # Modify this if the legend is covering your favorite circle
  #       legend.background = element_rect(size=0.1, linetype="solid", colour ="black"),
  #       legend.box.just = "right",
  #       legend.justification = c("right", "top"),
  #       axis.title=element_text(size=14,face="bold"),
  #       axis.text=element_text(size=12))+
  scale_color_manual(values = c(gray, red, blue, green, purple, orange), name="") +
  scale_fill_manual(values = c(gray, red, blue, green, purple, orange), name="") +
  scale_x_continuous("k-mer multiplicity")+
  scale_y_continuous(expression("Count (1 x "*10^6~")"), labels=function(x)format(x/1e6, nsmall = 1)) +
  coord_cartesian(xlim=c(0,200), ylim=c(0,15e6))
p

ggsave("Hap1_1.4_Hap2_1.1.spectra_cn.png", height = 5, width = 5)
