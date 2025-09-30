setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

library(tidyverse)
#!/usr/bin/env Rscript

## GenomeScope: Fast Genome Analysis from Unassembled Short Reads
## This is the automated script for computing genome characteristics
## from a kmer histogram file, k-mer size, and ploidy

## Load libraries for non-linear least squares and argument parser
library('minpack.lm')
library('argparse')

## Load the genomescope library
library('genomescope')

## Number of rounds before giving up
NUM_ROUNDS=4

## Coverage steps to trim off between rounds
START_SHIFT=5

## Typical cutoff for sequencing error
TYPICAL_ERROR = 15

## Max rounds on NLS
MAX_ITERATIONS=200

## Overrule if two scores are within this percent (0.05 = 5%) but larger difference in het
SCORE_CLOSE = 0.20

## Overrule heterozygosity if there is a large difference in het rate
SCORE_HET_FOLD_DIFFERENCE = 10

## Suppress the warnings if the modeling goes crazy, those are in try/catch blocks anyways
options(warn=-1)

## Colors for plots
COLOR_BGCOLOR  = "light grey"
COLOR_HIST     = "#56B4E9"
COLOR_2pPEAK   = "black"
COLOR_pPEAK    = "#F0E442"
COLOR_ERRORS   = "#D55E00"
COLOR_KMERPEAK = "gray75"
COLOR_RESIDUAL = "purple"
COLOR_COVTHRES = "red"

## Given mean +/- stderr, report min and max value within 2 SE
###############################################################################

min_max <- function(table) {
  return (c(max(0,table[1] - 2*table[2]), table[1]+ 2*table[2]))
}

min_max1 <- function(table) {
  return (c(max(0,table[1] - 2*table[2]), min(1, table[1]+ 2*table[2])))
}

## Main program starts here
###############################################################################

parser <- ArgumentParser()
parser$add_argument("-v", "--version", action="store_true", default=FALSE, help="print the version and exit")
parser$add_argument("-i", "--input", help = "input histogram file")
parser$add_argument("-o", "--output", help = "output directory name")
parser$add_argument("-p", "--ploidy", type = "integer", default = 2, help = "ploidy (1, 2, 3, 4, 5, or 6) for model to use [default 2]")
parser$add_argument("-k", "--kmer_length", type = "integer", default = 21, help = "kmer length used to calculate kmer spectra [default 21]")
parser$add_argument("-n", "--name_prefix", default = "", help = "optional name_prefix for output files")
parser$add_argument("-l", "--lambda", "--kcov", "--kmercov", type = "integer", default=-1, help = "optional initial kmercov estimate for model to use")
parser$add_argument("-m", "--max_kmercov", type = "integer", default=-1, help = "optional maximum kmer coverage threshold (kmers with coverage greater than max_kmercov are ignored by the model)")
parser$add_argument("--verbose", action="store_true", default=FALSE, help = "optional flag to print messages during execution")
parser$add_argument("--no_unique_sequence", action="store_true", default=FALSE, help = "optional flag to turn off yellow unique sequence line in plots")
parser$add_argument("-t", "--topology", type = "integer", default = 0, help = "ADVANCED: flag for topology for model to use")
parser$add_argument("--initial_repetitiveness", type="character", default = -1, help = "ADVANCED: flag to set initial value for repetitiveness")
parser$add_argument("--initial_heterozygosities", type="character", default = -1, help = "ADVANCED: flag to set initial values for nucleotide heterozygosity rates")
parser$add_argument("--transform_exp", type="integer", default=1, help = "ADVANCED: parameter for the exponent when fitting a transformed (x**transform_exp*y vs. x) kmer histogram [default 1]")
parser$add_argument("--testing", action="store_true", default=FALSE, help = "ADVANCED: flag to create testing.tsv file with model parameters")
parser$add_argument("--true_params", type="character", default = -1, help = "ADVANCED: flag to state true simulated parameters for testing mode")
parser$add_argument("--trace_flag", action="store_true", default=FALSE, help = "ADVANCED: flag to turn on printing of iteration progress of nlsLM function")
parser$add_argument("--num_rounds", type = "integer", default = 4, help = "ADVANCED: parameter for the number of optimization rounds")
parser$add_argument("--fitted_hist", action="store_true", default=FALSE, help = "ADVANCED: generates a fitted histogram for kmer multiplicity 0-4 and a lookup table of probabilities")
parser$add_argument("--start_shift", type = "integer", default=START_SHIFT, help = "ADVANCED: coverage shifts to exclude between fitting rounds")
parser$add_argument("--typical_error", type = "integer", default=TYPICAL_ERROR, help = "ADVANCED: typical level of sequencing error")


arguments <- parser$parse_args()
version_message <- "GenomeScope 2.0\n"

arguments$input <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/GenomeScope/jellyfish_out/Boltonia_bc2046.histo"
arguments$output <- "./figures/GenomeScope"
arguments

if (arguments$version) {
  cat(version_message)
  quit()
}

if (is.null(arguments$input) | is.null(arguments$output)) {
  cat("USAGE: genomescope.R -i input_histogram_file -o output_dir -p ploidy -k kmer_length\n")
  cat("OPTIONAL PARAMETERS: -n 'name_prefix' -l lambda -m max_kmercov --verbose --no_unique_sequence\n")
  cat("ADVANCED PARAMETERS: -t topology --initial_repetitiveness init_d --initial_heterozygosities init_r1,init_r2,...,init_rx --transform_exp t_exp --testing --true_params --trace_flag --num_rounds --fitted_hist\n")
  cat("HELP: genomescope.R --help\n")
} else {
  
  ## Load the arguments from the user
  histfile    <- arguments$input
  foldername  <- arguments$output
  p           <- arguments$ploidy
  k           <- arguments$kmer_length
  if (arguments$name_prefix != "") {
    arguments$name_prefix = paste0(arguments$name_prefix,"_")
  }
  estKmercov  <- arguments$lambda
  max_kmercov <- arguments$max_kmercov
  VERBOSE     <- arguments$verbose
  NO_UNIQUE_SEQUENCE <- arguments$no_unique_sequence
  topology    <- arguments$topology
  d_init      <- arguments$initial_repetitiveness
  r_inits     <- arguments$initial_heterozygosities
  transform_exp <- arguments$transform_exp
  TESTING     <- arguments$testing
  TRUE_PARAMS <- arguments$true_params
  TRACE_FLAG <- arguments$trace_flag
  NUM_ROUNDS <- arguments$num_rounds
  FITTED_HIST <- arguments$fitted_hist
  START_SHIFT <- arguments$start_shift
  TYPICAL_ERROR <- arguments$typical_error
  
  cat(paste("GenomeScope analyzing ", histfile, " p=", p, " k=", k, " outdir=", foldername, "\n", sep=""))
  
  dir.create(foldername, showWarnings=FALSE)
  
  ## Initialize the status
  progressFilename <- paste(foldername,"/", arguments$name_prefix, "progress.txt",sep="")
  cat("starting", file=progressFilename, sep="\n")
  
  kmer_prof <- read.csv(file=histfile,sep="", header=FALSE,colClasses=c("numeric","numeric"))
  
  minkmerx = 1;
  if (kmer_prof[1,1] == 0) {
    if (VERBOSE) {cat("Histogram starts with zero, reseting minkmerx\n")}
    minkmerx = 2;
  }
  
  kmer_prof_orig <- kmer_prof # kmer_prof_orig will now store all including the last position
  kmer_prof <- kmer_prof[c(minkmerx:(nrow(kmer_prof)-1)),] #get rid of the last position
  
  ## try to find the local minimum between errors and the first (heterozygous) peak
  kmer_trans = as.numeric(kmer_prof[,1])**transform_exp*as.numeric(kmer_prof[,2])
  start <- tail(which(kmer_trans[1:TYPICAL_ERROR]==min(kmer_trans[1:TYPICAL_ERROR])),n=1)
  start_max <- start + which(kmer_trans[start:length(kmer_trans)]==max(kmer_trans[start:length(kmer_trans)])) - 1
  
  maxCovIndex = -1
  
  ## Figure out which kmers to exclude, if any
  if(max_kmercov == -1) {
    maxCovIndex <- length(kmer_prof[,1])
    max_kmercov <- kmer_prof[maxCovIndex,1]
  }
  else {
    ## Figure out the index we should use for this coverage length
    x <- kmer_prof[,1]
    maxCovIndex <- length(x[x<=max_kmercov])
  }
  
  if (VERBOSE) {cat(paste("using max_kmercov:", max_kmercov, " with index:", maxCovIndex, "\n"))}
  
  # terminate after NUM_ROUND iterations, store best result so far in container
  round <- 0
  best_container <- list(NULL,0)
  
  while(round < NUM_ROUNDS) {
    cat(paste("round", round, "trimming to", start, "trying 2p peak model... "), file=progressFilename, sep="", append=TRUE)
    if (VERBOSE) {cat(paste("round", round, "trimming to", start, "trying 2p peak model... \n"))}
    
    ## Reset the input trimming off low frequency error kmers
    kmer_prof=kmer_prof_orig[1:maxCovIndex,]
    x <- kmer_prof[start:maxCovIndex,1]
    y <- kmer_prof[start:maxCovIndex,2]
    
    model_peaks <- estimate_Genome_peakp(kmer_prof, x, y, k, p, topology, estKmercov, round, foldername, arguments)
    
    if (!is.null(model_peaks[[1]])) {
      cat(paste("converged. score: ", model_peaks[[2]]$all[[1]]), file=progressFilename, sep="\n", append=TRUE)
      
      if (VERBOSE) {
        mdir = paste(foldername, "/round", round, sep="")
        dir.create(mdir, showWarnings=FALSE)
        report_results(kmer_prof,kmer_prof_orig, k, p, model_peaks, mdir, arguments, TRUE)
      }
    }
    else {
      cat(paste("unconverged"), file=progressFilename, sep="\n", append=TRUE)
    }
    
    #check if this result is better than previous
    if (!is.null(model_peaks[[1]])) {
      if (is.null(best_container[[1]])) {
        if (VERBOSE) {cat("no previous best, updating best\n")}
        best_container = model_peaks
      }
      else {
        best_container_score = best_container[[1]]$m$deviance()
        model_peaks_score = model_peaks[[1]]$m$deviance()
        pdiff = abs(model_peaks_score - best_container_score) / max(model_peaks_score, best_container_score)
        
        if (pdiff < SCORE_CLOSE) {
          hetm = model_peaks[[1]]$ahet
          hetb = best_container[[1]]$ahet
          
          #if (hetb * SCORE_HET_FOLD_DIFFERENCE < hetm) {
          if (hetb + 0.01 < hetm) {
            if (VERBOSE) {cat("model has significantly higher heterozygosity but similar score, overruling\n")}
          }
          #else if (hetm * SCORE_HET_FOLD_DIFFERENCE < hetb) {
          else if (hetm + 0.01 < hetb) {
            if (VERBOSE) {cat("previous best has significantly higher heterozygosity and similar score, keeping\n")}
            best_container = model_peaks
          }
          else if (model_peaks_score < best_container_score) {
            if (VERBOSE) {cat("score is marginally better but het rate is not extremely different, updating\n")}
            best_container = model_peaks
          }
        }
        else if (model_peaks_score < best_container_score) {
          if (VERBOSE) {cat("score is significantly better, updating\n")}
          best_container = model_peaks
        }
      }
    }
    
    ## Ignore a larger number of kmers as errors
    start <- start + START_SHIFT
    round <- round + 1
  }
  ## Report the results, note using the original full profile
  report_results(kmer_prof,kmer_prof_orig, k, p, best_container, foldername, arguments, FALSE)
  #  if (!is.null(best_container[[1]])) {
  #    print('model score')
  #    print(best_container[[2]]$all[[1]])
  #    print(best_container[[1]]$m$deviance())
  #  }
}

#####################################

x=kmer_prof_orig[[1]]
y_orig=kmer_prof_orig[[2]]
y = as.numeric(x)**transform_exp*as.numeric(y_orig)
kmer_hist_transform = kmer_prof_orig
kmer_hist_transform$V2 = as.numeric(kmer_hist_transform$V1)**transform_exp * as.numeric(kmer_hist_transform$V2)
model = best_container[[1]]
ISCROPPED = abs(nrow(kmer_prof) - nrow(kmer_prof_orig)) > 1 # the current version has difference one even when cutoff was not used (the last position is excluded for the fit but restored for the genome size est)

#automatically zoom into the relevant regions of the plot, ignore first 15 positions
xmax=length(x)
start_orig=which(y_orig == min(y_orig[1:TYPICAL_ERROR]))
start=which(y == min(y[1:TYPICAL_ERROR]))
zoomx=x[start:(xmax-1)]
zoomy_orig=y_orig[start_orig:(xmax-1)]
zoomy=y[start:(xmax-1)]

## allow for a little space above max value past the noise
y_limit_orig = max(zoomy_orig[start_orig:length(zoomy_orig)])*1.1
y_limit = max(zoomy[start:length(zoomy)])*1.1

x_limit_orig = which(y_orig == max(y_orig[start_orig:length(zoomx)])) * 3
x_limit = which(y == max(y[start:length(zoomx)])) * 3

if (min(zoomy_orig) > zoomy_orig[1]){
  x_limit_orig=max(which(zoomy_orig<zoomy_orig[1])[2],600)
}
if (min(zoomy) > zoomy[1]){
  x_limit=max(which(zoomy<zoomy[1])[2],600)
}

if (!is.null(model))
{
  model_sum=summary(model)
  kcov = min_max(model_sum$coefficients['kmercov',])[1]
  x_limit_orig = max(kcov*(2*p+1.1), x_limit_orig)
  x_limit = max(kcov*(2*p+1.1), x_limit)
  if (model$top==0) {
    p_to_num_r = c(0, 1, 2, 4, 6, 10)
  } else {
    p_to_num_r = c(0, 1, 2, 3, 4, 5)
  }
} else {
  if (topology==0) {
    p_to_num_r = c(0, 1, 2, 4, 6, 10)
  } else {
    p_to_num_r = c(0, 1, 2, 3, 4, 5)
  }
}

## Uncomment this to enforce a specific number
# x_limit=150

## Features to report
het=c(-1,-1)
homo=c(-1,-1)
num_r = p_to_num_r[p]
if (p > 1) {
  hets = lapply(1:(num_r), function(x) c(-1, -1))
  ahets = lapply(1:(num_r), function(x) -1)
}
amd = -1
akcov = -1
adups = -1
amlen = -1
atotal_len = -1
top = -1
total_len=c(-1,-1)
repeat_len=c(-1,-1)
unique_len=c(-1,-1)
dups=c(-1,-1)
error_rate=c(-1,-1)
model_status="fail"

model_fit_unique      = c(0,0,0)
model_fit_full        = c(0,0,0)
model_fit_all         = c(0,0,0)
model_fit_allscore    = c(0,0,0)
model_fit_fullscore   = c(0,0,0)
model_fit_uniquescore = c(0,0,0)

plot_size=2000
font_size=1.2
resolution=300





## Plot the distribution, and hopefully with the model fit
ylabel_orig = "Frequency"
if (transform_exp == 1) {
  ylabel_transform = "Coverage*Frequency"
} else {
  ylabel_transform = paste("Coverage^", transform_exp, "*Frequency", sep="")
}



##################
  
  x=kmer_prof_orig[[1]]
  y=kmer_prof_orig[[2]]
  y_transform = as.numeric(x)**transform_exp*as.numeric(y)
  
  ## The model converged!
  pred=predict(model, newdata=data.frame(x))
  
  ## Compute the genome characteristics
  model_sum=summary(model)
  #print(model_sum)
  
  ## save the model to a file
  capture.output(model_sum, file=paste(foldername,"/", arguments$name_prefix, "model.txt", sep=""))
  
  ## Identify key values
  top   = model$top
  hets  = model$hets
  het   = model$het
  ahets = model$ahets
  ahet  = model$ahet
  homo  = model$homo
  ahomo = model$ahomo
  
  dups = model$dups
  kcov = model$kcov
  mlen = model$mlen
  md   = model$md
  
  adups = model$adups
  akcov = model$akcov
  amlen = model$amlen
  amd   = model$amd
  
  ## Compute error rate, by counting kmers unexplained by model through first peak
  ## truncate errors as soon as it goes to zero, dont allow it to go back up
  error_xcutoff = max(1, floor(kcov[1]))
  error_xcutoff_ind = tail(which(x<=error_xcutoff),n=1)
  if (length(error_xcutoff_ind)==0) {error_xcutoff_ind=1}
  
  error_kmers = x[1:error_xcutoff_ind]**(-transform_exp)*(y_transform[1:error_xcutoff_ind] - pred[1:error_xcutoff_ind])
  
  first_zero = -1
  
  for (i in 1:error_xcutoff_ind)
  {
    if (first_zero == -1)
    {
      if (error_kmers[i] < 1.0)
      {
        first_zero = i
        if (VERBOSE) {cat(paste("Truncating errors at", i, "\n"))}
      }
    }
    else
    {
      error_kmers[i] = 0
    }
  }
  
  if (first_zero == -1)
  {
    first_zero = error_xcutoff_ind
    if (VERBOSE) {cat(paste("Truncating errors at", error_xcutoff_ind, "\n"))}
  }
  
  ## Rather than "0", set to be some very small number so log-log plot looks okay
  error_kmers = pmax(error_kmers, 1e-10)
  
  total_error_kmers = sum(as.numeric(error_kmers) * as.numeric(x[1:error_xcutoff_ind]))
  
  total_kmers = sum(as.numeric(x)*as.numeric(y))
  
  error_rate = 1-(1-(total_error_kmers/total_kmers))**(1/k)
  error_rate = c(error_rate, error_rate)
  
  # print(paste("Total:", total_kmers, "Total err: ", total_error_kmers, "kcov:", kcov)) # sanity testing lines :-)
  
  total_len = (total_kmers-total_error_kmers)/(p*kcov)
  atotal_len = (total_kmers-total_error_kmers)/(p*akcov)
  
  # print(paste("Total:", total_len, "Atotal: ", atotal_len)) # sanity testing  lines :-)
  
  ## find kmers that fit the p peak model (no repeats)
  if (p==1)
  {
    unique_hist = amlen*predict1_1_unique(k, amd, akcov, adups, x)
  }
  if (p==2)
  {
    unique_hist = amlen*predict2_1_unique(ahets[[1]], k, amd, akcov, adups, x)
  }
  if (p==3)
  {
    unique_hist = amlen*predict3_1_unique(ahets[[1]], ahets[[2]], k, amd, akcov, adups, x)
  }
  if (p==4)
  {
    if (top==0) {
      unique_hist = amlen*predict4_0_unique(ahets[[1]], ahets[[2]], ahets[[3]], ahets[[4]], k, amd, akcov, adups, x)
    } else {
      unique_hist = eval(parse(text = paste("amlen*predict4_", top, "_unique(ahets[[1]], ahets[[2]], ahets[[3]], k, amd, akcov, adups, x)", sep="")))
    }
  }
  if (p==5)
  {
    if (top==0) {
      unique_hist = amlen*predict5_0_unique(ahets[[1]], ahets[[2]], ahets[[3]], ahets[[4]], ahets[[5]], ahets[[6]], k, amd, akcov, adups, x)
    } else {
      unique_hist = eval(parse(text = paste("amlen*predict5_", top, "_unique(ahets[[1]], ahets[[2]], ahets[[3]], ahets[[4]], k, amd, akcov, adups, x)", sep="")))
    }
  }
  if (p==6)
  {
    if (top==0) {
      unique_hist = amlen*predict6_0_unique(ahets[[1]], ahets[[2]], ahets[[3]], ahets[[4]], ahets[[5]], ahets[[6]], ahets[[7]], ahets[[8]], ahets[[9]], ahets[[10]], k, amd, akcov, adups, x)
    } else {
      unique_hist = eval(parse(text = paste("amlen*predict6_", top, "_unique(ahets[[1]], ahets[[2]], ahets[[3]], ahets[[4]], ahets[[5]], k, amd, akcov, adups, x)", sep="")))
    }
  }
  
  r0 = 1-ahet #aa
  t0 = r0**k #AA
  s0 = t0 #AA
  s1 = 1-t0 #AB
  alpha_1 = (1-amd)*(2*s1) + amd*(2*s0*s1 + 2*s1**2)
  alpha_2 = (1-amd)*(s0) + amd*(s1**2)
  alpha_3 = amd*(2*s0*s1)
  alpha_4 = amd*(s0**2)
  
  one_hist = alpha_1 * dnbinom(x, size = akcov*1 / adups, mu = akcov*1)
  two_hist = alpha_2 * dnbinom(x, size = akcov*p / adups, mu = akcov*p)
  thr_hist = alpha_3 * dnbinom(x, size = akcov*3 / adups, mu = akcov*3)
  fou_hist = alpha_4 * dnbinom(x, size = akcov*2*p / adups, mu = akcov*2*p)
  
  unique_hist_transform = x**transform_exp*unique_hist
  
  unique_kmers = sum(as.numeric(x)*as.numeric(unique_hist))
  repeat_kmers = max(0, total_kmers - unique_kmers - total_error_kmers)
  
  repeat_len=repeat_kmers/(p*kcov)
  if (repeat_kmers == 0) {
    unique_len = total_len
  } else {
    unique_len=unique_kmers/(p*kcov)
  }
  
  score = best_container[[2]]
  
  model_fit_allscore    = score$allscore
  model_fit_fullscore   = score$fullscore
  model_fit_uniquescore = score$uniquescore
  
  model_fit_all    = score$all
  model_fit_full   = score$full
  model_fit_unique = score$unique
  
  residual_transform = y_transform - pred
  residual = x**(-transform_exp)*residual_transform
  
  hetline_simple = paste0("heterozygosity: ", format(100*ahet, digits=3), "%")
  
  if (p==1) {
    hetline = paste0("a:", format(100*ahomo, digits=3), "%")
  }
  if (p==2) {
    hetline = paste0("aa:", format(100*ahomo,      digits=3), "% ",
                     "ab:", format(100*ahets[[1]], digits=3), "%")
  }
  if (p==3) {
    hetline = paste0("aaa:", format(100*ahomo,      digits=3), "% ",
                     "aab:", format(100*ahets[[1]], digits=3), "% ",
                     "abc:", format(100*ahets[[2]], digits=3), "%")
  }
  if (p==4) {
    if (top==0) {
      hetline = paste0("aaaa:", format(100*ahomo,      digits=3), "% ",
                       "aaab:", format(100*ahets[[1]], digits=3), "% ",
                       "aabb:", format(100*ahets[[2]], digits=3), "% ",
                       "aabc:", format(100*ahets[[3]], digits=3), "% ",
                       "abcd:", format(100*ahets[[4]], digits=3), "%")
    } else {
      hetline = paste0("aaaa:",                       format(100*ahomo,      digits=3), "% ",
                       switch(top, "aaab:", "aabb:"), format(100*ahets[[1]], digits=3), "% ",
                       "aabc:",                       format(100*ahets[[2]], digits=3), "% ",
                       "abcd:",                       format(100*ahets[[3]], digits=3), "%")
    }
  }
  if (p==5) {
    if (top==0) {
      hetline = paste0("aaaaa:", format(100*ahomo,      digits=3), "% ",
                       "aaaab:", format(100*ahets[[1]], digits=3), "% ",
                       "aaabb:", format(100*ahets[[2]], digits=3), "% ",
                       "aaabc:", format(100*ahets[[3]], digits=3), "% ",'\n',
                       "aabbc:", format(100*ahets[[4]], digits=3), "% ",
                       "aabcd:", format(100*ahets[[5]], digits=3), "% ",
                       "abcde:", format(100*ahets[[6]], digits=3), "%")
    } else {
      hetline = paste0("aaaaa:",                                                      format(100*ahomo,      digits=3), "% ",
                       switch(top, "aaaab:", "aaaab:", "aaabb:", "aaabb:", "aaabb:"), format(100*ahets[[1]], digits=3), "% ",
                       switch(top, "aaabc:", "aabbc:", "aaabc:", "aabcc:", "aabcc:"), format(100*ahets[[2]], digits=3), "% ",'\n',
                       switch(top, "aabcd:", "aabcd:", "aabcd:", "aabcd:", "abcdd:"), format(100*ahets[[3]], digits=3), "% ",
                       "abcde:",                                                      format(100*ahets[[4]], digits=3), "%")
    }
  }
  if (p==6) {
    if (top==0) {
      hetline = paste0("aaaaaa:", format(100*ahomo, digits=3), "% ",
                       "aaaaab:", format(100*ahets[[1]], digits=3), "% ",
                       "aaaabb:", format(100*ahets[[2]], digits=3), "% ",
                       "aaabbb:", format(100*ahets[[3]], digits=3), "% ",'\n',
                       "aaaabc:", format(100*ahets[[4]], digits=3), "% ",
                       "aaabbc:", format(100*ahets[[5]], digits=3), "% ",
                       "aabbcc:", format(100*ahets[[6]], digits=3), "% ",
                       "aaabcd:", format(100*ahets[[7]], digits=3), "% ",'\n',
                       "aabbcd:", format(100*ahets[[8]], digits=3), "% ",
                       "aabcde:", format(100*ahets[[9]], digits=3), "% ",
                       "abcdef:", format(100*ahets[[10]], digits=3), "%")
    } else {
      hetline = paste0("aaaaaa:", format(100*ahomo, digits=3), "% ",
                       switch(top, "aaaaab:", "aaaaab:", "aaaaab:", "aaaaab:", "aaaaab:", "aaaabb:", "aaaabb:", "aaaabb:", "aaaabb:", "aaaabb:", "aaaabb:", "aaaabb:", "aaaabb:", "aaabbb:", "aaabbb:", "aaabbb:"), format(100*ahets[[1]], digits=3), "% ",
                       switch(top, "aaaabc:", "aaaabc:", "aaabbc:", "aaabbc:", "aaabbc:", "aaaabc:", "aaaabc:", "aaabcc:", "aaabcc:", "aaabcc:", "aabbcc:", "aabbcc:", "aabbcc:", "aaabbc:", "aaabbc:", "aaabbc:"), format(100*ahets[[2]], digits=3), "% ",'\n',
                       switch(top, "aaabcd:", "aabbcd:", "aaabcd:", "aabccd:", "aabccd:", "aaabcd:", "aabbcd:", "aaabcd:", "aabcdd:", "aabcdd:", "aabbcd:", "aabcdd:", "aabcdd:", "aaabcd:", "aabccd:", "aabccd:"), format(100*ahets[[3]], digits=3), "% ",
                       switch(top, "aabcde:", "aabcde:", "aabcde:", "aabcde:", "abcdde:", "aabcde:", "aabcde:", "aabcde:", "aabcde:", "abcdee:", "aabcde:", "aabcde:", "abcdee:", "aabcde:", "aabcde:", "abcdde:"), format(100*ahets[[4]], digits=3), "% ",
                       "abcdef:", format(100*ahets[[5]], digits=3), "%")
    }
  }
  
  if (p >= 5) {
    hetline = hetline_simple
  }
  
  if (!IN_VERBOSE) {
    cat(paste0(hetline,"\n"))
  }
  


#####################


par(mar = c(5.1,4.1,6.1,2.1))
plot(kmer_prof_orig, type="n", main="GenomeScope Profile\n\n\n",
     xlab="Coverage", ylab=ylabel_orig, ylim=c(0,y_limit_orig), xlim=c(0,x_limit_orig),
     cex.lab=font_size, cex.axis=font_size, cex.main=font_size, cex.sub=font_size)
#rect(0, 0, max(kmer_hist_orig[[1]])*1.1 , max(kmer_hist_orig[[2]])*1.1, col=COLOR_BGCOLOR)
rect(0, 0, x_limit_orig*1.1 , y_limit_orig*1.1, col=COLOR_BGCOLOR)
points(kmer_prof_orig, type="h", col=COLOR_HIST, lwd=2)
#  if( ISCROPPED ){
#    abline(v=length(kmer_hist[,1]),col=COLOR_COVTHRES,lty="dashed", lwd=3)
#  }
box(col="black")

## Finish Log plot
title(paste("\n\nlen:",  prettyNum(atotal_len, big.mark=","),
            "bp",
            " uniq:", format(100*(unique_len[1]/total_len[1]), digits=3),
            "% ", "\n",
            hetline, "\n",
            " kcov:", format(akcov, digits=3),
            " err:",   format(100*error_rate[1], digits=3),
            "% ",
            " dup:",  format(adups, digits=3),
            " ",
            " k:",   format(k, digits=3),
            " p:",   format(p, digits=3),
            sep=""),
      cex.main=.85)

## Mark the modes of the peaks
abline(v=akcov * (1:(2*p)), col=COLOR_KMERPEAK, lty=2)

## Draw just the unique portion of the model
if (!NO_UNIQUE_SEQUENCE) {
  lines(x, unique_hist, col=COLOR_pPEAK, lty=1, lwd=3)
}
lines(x, x**(-transform_exp)*pred, col=COLOR_2pPEAK, lwd=3)
lines(x[1:error_xcutoff_ind], error_kmers, lwd=3, col=COLOR_ERRORS)

if (VERBOSE) {
  lines(x, residual, col=COLOR_RESIDUAL, lwd=3)
}


legend("topright",legend = c("observed", "full model", "unique sequence", "errors", "kmer-peaks"),lty=c("solid", "solid", "solid", "solid", "dashed"),lwd=c(3,3,3,3,2),col=c(COLOR_HIST, COLOR_2pPEAK, COLOR_pPEAK, COLOR_ERRORS, COLOR_KMERPEAK),bg="white")




#######################

# Prepare data frames
df_hist   <- data.frame(x = kmer_prof_orig[,1], y = kmer_prof_orig[,2])
df_full   <- data.frame(x = x, y = x^(-transform_exp) * pred)
df_err    <- data.frame(x = x[seq_len(error_xcutoff_ind)], 
                        y = error_kmers[seq_len(error_xcutoff_ind)])
df_peaks <- data.frame(x = akcov * seq_len(n_peaks))
if (!NO_UNIQUE_SEQUENCE) {
  df_unique <- data.frame(x = x, y = unique_hist)
}
if (VERBOSE) {
  df_resid  <- data.frame(x = x, y = residual)
}

# Title/subtitle text (kept as in your original)
subtitle_text <- paste(
  "len:",  prettyNum(atotal_len, big.mark = ","), "bp",
  " uniq:", format(100 * (unique_len[1]/total_len[1]), digits = 3), "%", "\n",
  hetline, "\n",
  "kcov:", format(akcov, digits = 3),
  " err:",  format(100 * error_rate[1], digits = 3), "% ",
  " dup:",  format(adups, digits = 3), " ",
  " k:",    format(k, digits = 3),
  " p:",    format(p, digits = 3),
  sep = ""
)

# Colors mapped via manual scale
named_cols <- c(
  "observed"        = COLOR_HIST,
  "full model"      = COLOR_2pPEAK,
  "unique sequence" = COLOR_pPEAK,
  "errors"          = COLOR_ERRORS,
  "residual"        = COLOR_RESIDUAL,
  "kmer-peaks"      = COLOR_KMERPEAK
)

# Base plot with background rectangle
p1 <- ggplot() +
  # annotate("rect",
  #          xmin = 0, xmax = x_limit_orig * 1.1,
  #          ymin = 0, ymax = y_limit_orig * 1.1,
  #          fill = COLOR_BGCOLOR) +
  # Histogram as vertical stems (type="h")
  geom_segment(
    data = df_hist,
    aes(x = x, xend = x, y = 0, yend = y, color = "observed"),
    linewidth = 0.6
  ) +
  # Full model
  geom_line(
    data = df_full,
    aes(x = x, y = y, color = "full model"),
    linewidth = 1
  ) +
  # Unique portion (optional)
  { if (!NO_UNIQUE_SEQUENCE)
    geom_line(
      data = df_unique,
      aes(x = x, y = y, color = "unique sequence"),
      linewidth = 1
    )
    else NULL } +
  # Errors
  geom_line(
    data = df_err,
    aes(x = x, y = y, color = "errors"),
    linewidth = 1
  ) +
  # Peak markers
  geom_segment(
    data = df_peaks,
    aes(x = x, xend = x, y = 0, yend = Inf,color = "kmer-peaks"),
    linetype = 2,
    size = 0.8
  ) +
  coord_cartesian(xlim = c(0, x_limit_orig), ylim = c(0, y_limit_orig), expand = FALSE) +
  labs(
    x = "Coverage",
    y = ylabel_orig,
    color = NULL
  ) +
  scale_color_manual(values = named_cols,
                     breaks = c("observed", "full model", "unique sequence", "errors", "kmer-peaks"),
                     labels = c("observed", "full model", "unique sequence", "errors", "kmer-peaks")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle= element_text(margin = margin(t = 6)),
    panel.grid   = element_blank(),
    # Roughly mimic your cex.*; adjust if you want tighter control
    axis.title   = element_text(size = 11 * font_size),
    axis.text    = element_text(size = 10 * font_size),
    plot.title.position = "plot",
    legend.position = c(0.7,0.8),
    legend.background = element_rect(fill = NULL),
    legend.key.width  = unit(1.5, "cm")
  )


p1

# Save as PNG matching your pixel size and resolution

ggsave("./figures/GenomeScope/Boltonia_GenomeScope_pac_k21.png",
       width = 5, height = 5,
       dpi = 600, bg = "white")




###########


## ---------- Data prep ----------
df_hist <- data.frame(
  x = kmer_prof_orig[, 1],
  y = kmer_prof_orig[, 2]
)

df_full <- data.frame(
  x = x,
  y = x^(-transform_exp) * pred
)

df_err <- data.frame(
  x = x[seq_len(error_xcutoff_ind)],
  y = error_kmers[seq_len(error_xcutoff_ind)]
)

if (!NO_UNIQUE_SEQUENCE) {
  df_unique <- data.frame(x = x, y = unique_hist)
}

if (VERBOSE) {
  df_resid <- data.frame(x = x, y = residual)
}

## ---------- Text ----------
subtitle_text <- paste(
  "len:",  prettyNum(atotal_len, big.mark = ","), "bp",
  " uniq:", format(100 * (unique_len[1] / total_len[1]), digits = 3), "%", "\n",
  hetline, "\n",
  " kcov:", format(akcov, digits = 3),
  " err:",  format(100 * error_rate[1], digits = 3), "% ",
  " dup:",  format(adups, digits = 3), " ",
  " k:",    format(k, digits = 3),
  " p:",    format(p, digits = 3),
  sep = ""
)

## ---------- Colors & legend ----------
named_cols <- c(
  "observed"        = COLOR_HIST,
  "full model"      = COLOR_2pPEAK,
  "unique sequence" = COLOR_pPEAK,
  "errors"          = COLOR_ERRORS,
  "residual"        = COLOR_RESIDUAL,
  "kmer-peaks"      = COLOR_KMERPEAK
)

# Safely compute number of peak markers from numeric p
p_num   <- suppressWarnings(as.numeric(p))
n_peaks <- if (is.finite(p_num)) max(0, floor(2 * p_num)) else 0

# Build peak df (even if empty; we gate with n_peaks > 0)
if (n_peaks > 0) {
  df_peaks <- data.frame(x = akcov * seq_len(n_peaks))
}

# Build legend breaks dynamically (include only what we're actually plotting)
legend_breaks <- c(
  "observed",
  "full model",
  if (!NO_UNIQUE_SEQUENCE) "unique sequence",
  "errors",
  if (VERBOSE) "residual",
  "kmer-peaks"
)

## ---------- Plot ----------
g <- ggplot() +
  
  # Histogram as vertical stems (type="h")
  geom_segment(
    data = df_hist,
    aes(x = x, xend = x, y = 0, yend = y, color = "observed"),
    size = 0.6
  ) +
  
  # Full model
  geom_line(
    data = df_full,
    aes(x = x, y = y, color = "full model"),
    size = 1
  ) +
  
  # Unique portion (optional)
  { if (!NO_UNIQUE_SEQUENCE)
    geom_line(
      data = df_unique,
      aes(x = x, y = y, color = "unique sequence"),
      size = 1
    )
    else NULL } +
  
  # Errors
  geom_line(
    data = df_err,
    aes(x = x, y = y, color = "errors"),
    size = 1
  ) +
  
  # Residual (optional)
  { if (VERBOSE)
    geom_line(
      data = df_resid,
      aes(x = x, y = y, color = "residual"),
      size = 1
    )
    else NULL } +
  
  # Peak markers (mapped so they appear in legend)
  { if (n_peaks > 0)
    geom_vline(
      data = df_peaks,
      aes(xintercept = x, color = "kmer-peaks"), linetype = 2,
      size = 0.8,
      show.legend = TRUE
    )
    else NULL } +
  
  coord_cartesian(
    xlim = c(0, x_limit_orig),
    ylim = c(0, y_limit_orig)
  ) +
  labs(
    title    = "GenomeScope Profile",
    subtitle = subtitle_text,
    x = "Coverage",
    y = ylabel_orig,
    color = NULL,
    linetype = NULL
  ) +
  
  # Scales & legend
  scale_color_manual(
    values = named_cols,
    breaks = legend_breaks
  ) +
  # scale_linetype_manual(
  #   values = c("kmer-peaks" = "dashed"),
  #   breaks = "kmer-peaks"
  # ) +
  # guides(
  #   color = guide_legend(override.aes = list(size = 1.2))
  # ) +
  
  # Theme
  theme_bw() +
  theme(
    panel.grid         = element_blank(),
    plot.title         = element_text(hjust = 0.5),
    plot.subtitle      = element_text(margin = margin(t = 6)),
    plot.title.position= "plot",
    legend.position    = "top",
    # approximate your cex.* settings
    axis.title         = element_text(size = 11 * font_size),
    axis.text          = element_text(size = 10 * font_size)
  )

g

## ---------- Save PNG ----------
outfile <- file.path(foldername, paste0(arguments$name_prefix, "linear_plot.png"))
ggsave(
  filename = outfile,
  plot     = g,
  width    = plot_size / resolution,  # inches
  height   = plot_size / resolution,  # inches
  dpi      = resolution,
  bg       = "white"
)
