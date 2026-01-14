
library(admixtools)

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
ags_h0 <- find_graphs(
      data = f2_blocks,
      outpop = "apalachicolensis",
      max_admix = 0,
      numgraphs = 100,
      reject_f4z = 4)
save(ags_h0, file = "hybridization/admixtools/find_graphs_h0.Rdata")

ags_h0$graph[[which.min(ags_h0$score)]]

ags_h1 <- find_graphs(
      data = f2_blocks,
      initgraph = ags_h0$graph[[which.min(ags_h0$score)]],
      outpop = "apalachicolensis",
      max_admix = 1,
      numgraphs = 100,
      reject_f4z = 4)
save(ags_h1, file = "hybridization/admixtools/find_graphs_h1.Rdata")

ags_h2 <- find_graphs(
      data = f2_blocks,
      initgraph = ags_h1$graph[[which.min(ags_h1$score)]],
      outpop = "apalachicolensis",
      max_admix = 2,
      numgraphs = 100,
      reject_f4z = 4)
save(ags_h2, file = "hybridization/admixtools/find_graphs_h2.Rdata")

ags_h3 <- find_graphs(
      data = f2_blocks,
      initgraph = ags_h2$graph[[which.min(ags_h2$score)]],
      outpop = "apalachicolensis",
      max_admix = 3,
      numgraphs = 100,
      reject_f4z = 4)
save(ags_h3, file = "hybridization/admixtools/find_graphs_h3.Rdata")

ags_h4 <- find_graphs(
      data = f2_blocks,
      initgraph = ags_h3$graph[[which.min(ags_h3$score)]],
      outpop = "apalachicolensis",
      max_admix = 4,
      numgraphs = 100,
      reject_f4z = 4)
save(ags_h4, file = "hybridization/admixtools/find_graphs_h4.Rdata")



ags_h5 <- find_graphs(
      data = f2_blocks,
      initgraph = ags_h4$graph[[which.min(ags_h4$score)]],
      outpop = "apalachicolensis",
      max_admix = 5,
      numgraphs = 100,
      reject_f4z = 4)
save(ags_h5, file = "hybridization/admixtools/find_graphs_h5.Rdata")


args(insert_admix)
##TO run this script in cluster use:
bsub -n 4 -Is -M 10G -R 'rusage[mem=10G]' -G compute-christine.e.edwards -q general-interactive -a 'docker(kuowenhsi/wen_fastsimcoal2:2.2)' /bin/bash

pdf("hybridization/admixtools/best_graphs_h5.pdf")
plot_graph(ags_h5$graph[[which.min(ags_h5$score)]])
dev.off()