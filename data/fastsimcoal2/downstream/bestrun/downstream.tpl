//Parameters for the coalescence simulation program : fastsimcoal2
6 samples to simulate :
//Population effective sizes (number of genes; haploid)
N_A
N_B
N_C
N_D
N_E
N_F
//Sample sizes and samples age (edit these integers as needed)
106
70
234
90
144
164
//Growth rates: negative growth implies expansion (0 = constant)
0
0
0
0
0
0
//Number of migration matrices : 0 implies no migration between demes
2
//Migration matrix 0
0 MIG_BA MIG_CA 0 0 0
0 0 MIG_CB MIG_DB 0 0
0 0 0 MIG_DC MIG_EC 0
0 0 0 0 MIG_ED MIG_FD
0 0 0 0 0 MIG_FE
0 0 0 0 0 0	
//Migration matrix 1
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
//historical event: time, source, sink, migrants, new deme size, growth rate, migr mat index
5 historical event
TDIV 1 0 1 1 0 1
TDIV 2 0 1 1 0 1
TDIV 3 0 1 1 0 1
TDIV 4 0 1 1 0 1
TDIV 5 0 1 1 0 1
//Number of independent loci [chromosome]
1 0
//Per chromosome: Number of contiguous linkage blocks
1
//Per block: data type, #loci, recomb rate, mut rate
FREQ 1 0 2.5e-8
