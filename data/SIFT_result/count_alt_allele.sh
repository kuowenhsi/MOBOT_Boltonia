#! /bin/bash
set -euo pipefail

# USAGE:
#   bash count_alt_allele.sh INPUT.vcf[.gz] > out.tsv
#
# OUTPUT columns:
#   Sample  ALT_Allele_Sum  NonMissingGenotypes  TotalAlleles

in="$1"
fixed="${in%.vcf}.fixed.vcf"

# 0) Remove the invalid SIFT header line that breaks parsing
grep -v '^##SIFT_Threshold:' "$in" > "$fixed"

# 1) Get sample names (one per line)
#    Also strip any accidental "[123]" prefix if present in the names themselves.
bcftools query -l "$fixed" \
| sed 's/^\[[0-9]\+\]//' > samples.txt

ns=$(wc -l < samples.txt)

# 2) Count ALT dosage (only allele "1": 0/0->0, 0/1->1, 1/1->2), plus non-missing and total alleles
#    NOTE: bcftools prints each row as "GT<TAB>GT< TAB >...< TAB >" (trailing tab). We trim it.
bcftools query -f '[%GT\t]\n' "$fixed" \
| awk -v OFS="\t" -v NS="$ns" '
BEGIN{
  # Read sample names into samp[1..NS]
  while ((getline s < "samples.txt") > 0) {
    samp[++ns_read] = s
  }
  if (ns_read != NS) {
    printf("ERROR: ns_read (%d) != NS (%d) from samples.txt\n", ns_read, NS) > "/dev/stderr"
    exit 1
  }
  print "Sample","ALT_Allele_Sum","NonMissingGenotypes","TotalAlleles"
}
{
  # Drop the trailing empty field caused by the final tab, if present
  nf = NF
  if ($nf == "") nf--

  # Sanity check: each data line should have exactly NS genotype fields after trimming
  if (nf != NS) {
    printf("WARN: field count %d != expected %d on line %d; skipping line\n", nf, NS, NR) > "/dev/stderr"
    next
  }

  # Iterate samples 1..NS
  for (i=1; i<=NS; i++) {
    gt = $i
    gsub(/\|/,"/",gt)  # normalize phased to unphased

    # Skip missing genotypes
    if (gt=="" || gt=="." || gt=="./." || gt==".|.") continue

    nobs[i]++

    # Count ALT dosage for biallelics: number of "1" alleles in the GT
    n = split(gt, a, "/")
    d = 0
    for (k=1; k<=n; k++) if (a[k]=="1") d++
    sum[i] += d
  }
}
END{
  for (i=1; i<=NS; i++) {
    nm = (nobs[i]?nobs[i]:0)
    ta = 2*nm
    s  = (sum[i]?sum[i]:0)
    print samp[i], s, nm, ta
  }
}'
