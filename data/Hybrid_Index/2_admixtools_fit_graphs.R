
library(admixtools)
#load genotype data
geno_prefix= "hybridization/admixtools/Boltonia_fstats"
f2_blocks <- f2_from_geno(pref=geno_prefix, format = "plink",blgsize = 10000)
pops <- dimnames(f2_blocks)[[1]]

# load graphs
for(h in 0:5){
 load(paste0("hybridization/admixtools/find_graphs_h",h,".Rdata"))
}
ags_h0$max_admix <- 0
ags_h1$max_admix <- 1
ags_h2$max_admix <- 2
ags_h3$max_admix <- 3
ags_h4$max_admix <- 4
ags_h5$max_admix <- 5
all_graphs <- mget(paste0("ags_h", 0:5))
all_graphs <- do.call(rbind,all_graphs)
all_graphs$numadmix <- sapply(all_graphs$graph, numadmix)
#get the best graph per each max_admix
library(dplyr)
best_graphs <- all_graphs %>%
  group_by(max_admix) %>%
  slice_min(order_by = score, n = 1)
test_graphs <- best_graphs$graph
#Make a graph to test the asteroides --> decurrens_01 gene flow
#we will use graph at max_admix = 0 as the base (null hypothesis)
best_h0_graph <- ags_h0$graph[[which.min(ags_h0$score)]]
best_h0_edges <- ags_h0$edges[[which.min(ags_h0$score)]]
test_graphs[[7]] <- insert_admix(
  graph       = best_h0_graph,
  source_from = best_h0_edges$from[best_h0_edges$to == "asteroides"],
  source_to   = "asteroides",
  dest_from   = best_h0_edges$from[best_h0_edges$to == "decurrens_01"],
  dest_to     = "decurrens_01",
  substitute  = FALSE
)
names(test_graphs) <- c(paste0("ags_h", 0:5), "ags_h1_intersp")
#fit all graphs using bootstrap approach
qpgraph_fits <-lapply(test_graphs, function(x) qpgraph(f2_blocks, graph = x))


pdf("hybridization/admixtools/best_graphs_all.pdf")
plot_graph(qpgraph_fits$ags_h1_intersp$edges)
plot_graph(qpgraph_fits$ags_h0$edges)
plot_graph(qpgraph_fits$ags_h1$edges)
plot_graph(qpgraph_fits$ags_h2$edges)
plot_graph(qpgraph_fits$ags_h3$edges)
plot_graph(qpgraph_fits$ags_h4$edges)
plot_graph(qpgraph_fits$ags_h5$edges)
dev.off()


lapply(qpgraph_fits, function(x) x$score)

svg("hybridization/admixtools/best_graph_h5.svg", width = 14, height = 14)
plot_graph(qpgraph_fits$ags_h5$edges, color=F,textsize = 4)
dev.off()

# write the admixture graph at h=0 as a reasonable poppulation tree
source(source("C:/Users/aedua/Documents/Weinmannia/PROJECTS/2_hybridization/central_andes/scripts/Phylogenomics/admixtools/find_level1_networks.R"))

backbone <- get_major_tree(edges_df = qpgraph_fits$ags_h0$edges, graph= edges_to_igraph(qpgraph_fits$ags_h0$edges))
write.table(backbone$newick_string,"hybridization/Dsuite/poptree_admixtools_h0.nwk", quote = F, row.names = F, col.names = F)