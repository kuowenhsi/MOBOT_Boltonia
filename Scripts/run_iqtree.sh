#!/bin/bash

# Usage: ./vcf_to_tree.sh input.vcf [min_samples]
# Example: ./vcf_to_tree.sh mydata.vcf 10

# Check input arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 input.vcf [min_samples (default=1)]"
    exit 1
fi

VCF_FILE="$1"
MIN_SAMPLES="${2:-1}"  # Default is 1 if not provided

# Check if VCF file exists
if [ ! -f "$VCF_FILE" ]; then
    echo "Error: File '$VCF_FILE' not found!"
    exit 1
fi

# Derive prefix (e.g., myfile.vcf → myfile)
VCF_BASENAME=$(basename "$VCF_FILE" .vcf)

# Output folder is same as input location
OUTPUT_FOLDER=$(dirname "$VCF_FILE")

# Output PHYLIP file name
PHYLIP_FILE="$OUTPUT_FOLDER/${VCF_BASENAME}.min${MIN_SAMPLES}.phy"

# Step 1: Convert VCF to PHYLIP (only if not already present)
if [ -f "$PHYLIP_FILE" ]; then
    echo "PHYLIP file already exists: $PHYLIP_FILE — skipping conversion."
else
    echo "Converting VCF to PHYLIP format with -m $MIN_SAMPLES ..."
    vcf2phylip \
        -i "$VCF_FILE" \
        -m "$MIN_SAMPLES" \
        --output-folder "$OUTPUT_FOLDER" \
        --output-prefix "$VCF_BASENAME"

    echo "COMPLETED - Converting VCF to PHYLIP format with -m $MIN_SAMPLES ..."

    # Check if PHYLIP file was created
    if [ ! -f "$PHYLIP_FILE" ]; then
        echo "Error: PHYLIP file not found: $PHYLIP_FILE"
        exit 1
    fi
fi

# Step 2: Run IQ-TREE
echo "Running IQ-TREE on $PHYLIP_FILE ..."
iqtree -s "$PHYLIP_FILE" -m GTR+G -nt 24

echo "✅ Done! Tree file: ${PHYLIP_FILE}.treefile"

