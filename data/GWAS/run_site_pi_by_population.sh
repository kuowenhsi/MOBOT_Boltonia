#!/usr/bin/env bash
set -euo pipefail

############################################
# Calculate population-level site-pi with VCFtools
#
# Usage:
#   bash run_site_pi_by_population.sh input.vcf.gz popmap.txt output_prefix
#
# Input popmap format:
#   sample1   Pop1
#   sample2   Pop1
#   sample3   Pop2
#   ...
#
# Header is allowed.
#
# Final output:
#   output_prefix.population_site_pi.csv
# with columns:
#   Pop,CHROM,POSITION,PI
############################################

if [[ $# -lt 3 ]]; then
    echo "Usage: bash $0 <input.vcf.gz> <popmap.txt> <output_prefix>"
    exit 1
fi

VCF="$1"
POPMAP="$2"
OUTPREFIX="$3"

# Check dependencies
command -v vcftools >/dev/null 2>&1 || { echo "Error: vcftools not found in PATH"; exit 1; }
command -v awk >/dev/null 2>&1 || { echo "Error: awk not found in PATH"; exit 1; }

# Check input files
[[ -f "$VCF" ]] || { echo "Error: VCF file not found: $VCF"; exit 1; }
[[ -f "$POPMAP" ]] || { echo "Error: popmap file not found: $POPMAP"; exit 1; }

# Working directories
WORKDIR="${OUTPREFIX}_site_pi_work"
KEEPDIR="${WORKDIR}/keep_files"
RAWOUTDIR="${WORKDIR}/vcftools_outputs"

mkdir -p "$KEEPDIR" "$RAWOUTDIR"

echo "Creating per-population keep files from: $POPMAP"

# Create keep files
# Accepts whitespace-delimited popmap with optional header and/or comment lines
awk -v outdir="$KEEPDIR" '
BEGIN {
    OFS="\t"
}
function trim(x) {
    gsub(/^[ \t]+|[ \t]+$/, "", x)
    return x
}
function sanitize(x) {
    gsub(/[^A-Za-z0-9._-]/, "_", x)
    return x
}
{
    if ($0 ~ /^[ \t]*$/) next            # skip empty lines
    if ($0 ~ /^[ \t]*#/) next            # skip comment lines

    sample = trim($1)
    pop    = trim($2)

    # skip likely header lines
    lower1 = tolower(sample)
    lower2 = tolower(pop)
    if (lower1 == "sample" || lower1 == "sampleid" || lower1 == "id") next
    if (lower2 == "population" || lower2 == "pop" || lower2 == "group") next

    if (sample == "" || pop == "") next

    safe_pop = sanitize(pop)

    print sample >> (outdir "/" safe_pop ".keep")
    popname[safe_pop] = pop
}
END {
    for (p in popname) {
        print p "\t" popname[p]
    }
}
' "$POPMAP" > "${WORKDIR}/population_name_map.tsv"

if [[ ! -s "${WORKDIR}/population_name_map.tsv" ]]; then
    echo "Error: no valid population/sample entries found in popmap."
    exit 1
fi

echo "Population keep files created:"
cut -f2 "${WORKDIR}/population_name_map.tsv" | sort

# Final merged output
FINAL_OUT="${OUTPREFIX}.population_site_pi.csv"
echo "Pop,CHROM,POSITION,PI" > "$FINAL_OUT"

echo "Running vcftools --site-pi for each population..."

while IFS=$'\t' read -r SAFE_POP ORIGINAL_POP; do
    KEEPFILE="${KEEPDIR}/${SAFE_POP}.keep"
    OUTBASE="${RAWOUTDIR}/${SAFE_POP}"

    echo "  Processing population: ${ORIGINAL_POP}"

    vcftools \
        --gzvcf "$VCF" \
        --keep "$KEEPFILE" \
        --site-pi \
        --out "$OUTBASE"

    SITEPI_FILE="${OUTBASE}.sites.pi"

    if [[ ! -f "$SITEPI_FILE" ]]; then
        echo "Warning: expected output not found for ${ORIGINAL_POP}: ${SITEPI_FILE}"
        continue
    fi

    # Append to final CSV
    # Expected vcftools output columns: CHROM POS PI
    awk -v pop="$ORIGINAL_POP" 'BEGIN{OFS=","}
        NR > 1 {
            print pop, $1, $2, $3
        }
    ' "$SITEPI_FILE" >> "$FINAL_OUT"

done < "${WORKDIR}/population_name_map.tsv"

echo "Done."
echo "Final merged output:"
echo "  $FINAL_OUT"
echo
echo "Intermediate files are in:"
echo "  $WORKDIR"
