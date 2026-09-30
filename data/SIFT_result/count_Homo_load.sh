#! /bin/bash
set -euo pipefail

# USAGE:
#   bash count_homozygous_alt.sh INPUT.vcf[.gz] > out.tsv
#
# OUTPUT columns:
#   Sample  Homozygous_ALT_Sites  Homozygous_ALT_Alleles  NonMissingGenotypes  TotalAlleles
#
# Notes:
# - Treats "1/1" (or "1|1") as homozygous ALT=1.
# - Ignores multiallelic ALT>1 (e.g., "2/2"); mirrors the original script's focus on ALT=1.
# - NonMissingGenotypes counts callable genotypes (not just at deleterious sites).

in="$1"
fixed="${in%.vcf}.fixed.vcf"

# 0) Remove the invalid SIFT header line that breaks parsing
grep -v '^##SIFT_Threshold:' "$in" > "$fixed"

# 1) Get sample names (one per line)
#    Also strip any accidental "[123]" prefix if present in the names themselves.
bcftools query -l "$fixed" \
| sed 's/^\[[0-9]\+\]//' > samples.txt

ns=$(wc -l < samples.txt)

# 2) Parse GTs and count homozygous ALT=1 per sample
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
  print "Sample","Homozygous_ALT_Sites","Homozygous_ALT_Alleles","NonMissingGenotypes","TotalAlleles"
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

  for (i=1; i<=NS; i++) {
    gt = $i
    gsub(/\|/,"/",gt)  # normalize phased to unphased

    # Skip missing genotypes
    if (gt=="" || gt=="." || gt=="./." || gt==".|.") continue

    nobs[i]++

    # Count homozygous ALT=1 (e.g., 1/1)
    n = split(gt, a, "/")
    is_hom1 = 0
    if (n >= 2) {
      is_hom1 = 1
      for (k=1; k<=n; k++) if (a[k] != "1") { is_hom1 = 0; break }
    }
    # If haploid data existed, you could decide how to treat "1" here; we assume diploid.

    if (is_hom1) hom[i]++
  }
}
END{
  for (i=1; i<=NS; i++) {
    nm = (nobs[i]?nobs[i]:0)
    ta = 2*nm
    h  = (hom[i]?hom[i]:0)
    print samp[i], h, 2*h, nm, ta
  }
}'
