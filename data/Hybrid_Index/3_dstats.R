

library(ggplot2)
library(admixtools)
library(ape)
library(ggtree)
library(ggpubr)
#IMPORTANT: first inlcude population in fam file
# sets<-read.table("hybridization/scripts/fstat_analyses/SETS.txt")
# fam<-read.table("Boltonia_fstats.fam")
# sets$V1[match(fam$V2,sets$V1)] == fam$V2
# fam$V1 <- sets$V2[match(fam$V2,sets$V1)]
# write.table(fam, row.names = F, col.names = F, quote = F,sep = "\t", file = "Boltonia_fstats.fam")
geno_prefix= "hybridization/admixtools/Boltonia_fstats"

f2_blocks <- f2_from_geno(pref=geno_prefix, format = "plink",
  blgsize = 10000)
pops <- dimnames(f2_blocks)[[1]]
testpops <- c(paste0("decurrens_0",1:9),paste0("decurrens_",10:17))
#explore all arrangements systematically, varying focal population one at a time
all_arrangements <- list() 
for(pop in testpops){
    tmp <- qpdstat(data=f2_blocks,
        pop1 = pop,
        pop2 = testpops[!testpops == pop],
        pop3 = "asteroides",
        pop4 = "apalachicolensis",
        f4mode = FALSE)
    tmp$focal_population <- pop
    all_arrangements[[pop]] <- tmp
}
all_arrangements<- do.call(rbind.data.frame, all_arrangements)
#correct pvalues
all_arrangements$fdr_p <- p.adjust(all_arrangements$p, method = "fdr")
#plot these results
all_arrangements$significance <-ifelse(all_arrangements$fdr_p <= 0.01, "p_val < 0.01", "p_val > 0.01")
#
dplot<-ggplot(all_arrangements)+
    geom_point(aes(x=focal_population, y=z, color=significance))+
    scale_color_manual(values=c( "tomato","grey"))+
    xlab("Focal population (P1)")+ ylab("Z-score")+labs(color="FDR adjusted p-value")+
    geom_hline(yintercept = c(-3,3), linetype=3)+
    theme_classic2()+
    theme(axis.text.x = element_text(angle=90),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "bottom")

high<-all_arrangements[all_arrangements$z <= -5,]
high[,c("pop1","pop2")]

#make test topology
tree_text<-"(((decurrens_other\nP2,decurrens_focal\nP1),B._asteroides\nP3), Outgroup);"
tree<-read.tree(text = tree_text)

t<-ggtree(tree,right = FALSE,layout = "rectangular")+
    geom_tiplab(angle=0, size=4, hjust = 0)+xlim(c(0,3.5))+
     theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


svg("hybridization/admixtools/dstats_test.svg")
 ggarrange(t,dplot,nrow=2, heights = c(0.5,1))
dev.off()


