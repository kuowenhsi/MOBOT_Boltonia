#! /bin/bash
set -euo pipefail

# USAGE:
#   bash count_heterozygous_alt.sh INPUT.vcf[.gz] > out.tsv
#
# OUTPUT columns:
#   Sample  Heterozygous_ALT_Sites  Heterozygous_ALT_Alleles  NonMissingGenotypes  TotalAlleles
#
# Notes:
# - Counts a site if GT is exactly 0/1 or 1/0 (or phased 0|1, 1|0).
# - Skips missing GTs and multiallelic calls (any allele > 1).
# - NonMissingGenotypes = callable GTs (not restricted to deleterious sites).

in="$1"
fixed="${in%.vcf}.fixed.vcf"

# 0) Remove the invalid SIFT header line that breaks parsing (if present)
grep -v '^##SIFT_Threshold:' "$in" > "$fixed"

# 1) Get sample names (one per line)
#    Also strip any accidental "[123]" prefix if present in the names themselves.
bcftools query -l "$fixed" \
| sed 's/^\[[0-9]\+\]//' > samples.txt

ns=$(wc -l < samples.txt)

# 2) Parse GTs and count heterozygous ALT=1 per sample
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
  print "Sample","Heterozygous_ALT_Sites","Heterozygous_ALT_Alleles","NonMissingGenotypes","TotalAlleles"
}
{
  # Drop trailing empty field from final tab, if present
  nf = NF
  if ($nf == "") nf--

  if (nf != NS) {
    printf("WARN: field count %d != expected %d on line %d; skipping line\n", nf, NS, NR) > "/dev/stderr"
    next
  }

  for (i=1; i<=NS; i++) {
    gt = $i
    gsub(/\|/,"/",gt)  # normalize phased to unphased

    # Skip missing genotypes
    if (gt=="" || gt=="." || gt=="./." || gt==".|.") continue

    nobs[i]++

    # Identify heterozygous ALT=1: exactly one "0" and one "1", no other alleles
    n = split(gt, a, "/")
    if (n < 2) continue  # assume diploid accounting

    zeros = ones = others = 0
    for (k=1; k<=n; k++) {
      if (a[k] == "0") zeros++
      else if (a[k] == "1") ones++
      else others++   # catches multiallelic (2,3,...) or unexpected tokens
    }

    if (zeros == 1 && ones == 1 && others == 0) {
      het[i]++
    }
  }
}
END{
  for (i=1; i<=NS; i++) {
    nm = (nobs[i]?nobs[i]:0)
    ta = 2*nm
    h  = (het[i]?het[i]:0)
    # Each heterozygous site contributes exactly 1 ALT allele
    print samp[i], h, h, nm, ta
  }
}'
