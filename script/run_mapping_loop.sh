cd "/mnt/host/c/Users/User/Desktop/REUProject/Boltonia_toy"
set -e

REF=$1  # Reference genome path passed as argument

# Loop over all R1 fastq files
for R1 in Boltonia_*_R1.fastq.gz; do
  # Derive corresponding R2 filename
  R2="${R1/_R1.fastq.gz/_R2.fastq.gz}"

  # Check if R2 exists
  if [ ! -f "$R2" ]; then
    echo "⚠️  Skipping $R1: corresponding R2 file $R2 not found."
    continue
  fi

  # Extract base name for output
  PREFIX=$(basename "$R1" | sed 's/_R1.fastq.gz//')

  # Skip if final output exists
  if [ -f "${PREFIX}.HQ.RG.bam" ]; then
    echo "⏩ Skipping $PREFIX: already processed."
    continue
  fi

  echo "🔄 Processing sample: $PREFIX"
  echo "Input R1: $R1"
  echo "Input R2: $R2"
  echo "Reference: $REF"

  # 1. Align reads
  bwa mem -t 4 "$REF" "$R1" "$R2" > "${PREFIX}.sam"

  # 2. Convert to BAM
  samtools view -b -S -h "${PREFIX}.sam" > "${PREFIX}.bam"
  rm -f "${PREFIX}.sam"

  # 3. Sort BAM
  samtools sort "${PREFIX}.bam" -o "${PREFIX}.sort.bam"
  rm -f "${PREFIX}.bam"

  # 4. Index sorted BAM
  samtools index "${PREFIX}.sort.bam"

  # 5. Mark duplicates
  gatk --java-options "-Xmx8g" MarkDuplicates \
    -I "${PREFIX}.sort.bam" \
    -O "${PREFIX}.dedup.bam" \
    -M "${PREFIX}.metrics.txt"
  rm -f "${PREFIX}.sort.bam" "${PREFIX}.sort.bam.bai"

  # 6. Filter reads (remove unmapped, low quality, duplicates, etc.)
  samtools view -F 3844 -b "${PREFIX}.dedup.bam" > "${PREFIX}.filtered.bam"
  rm -f "${PREFIX}.dedup.bam"
  samtools index "${PREFIX}.filtered.bam"

  # 7. Keep only MAPQ ≥ 30
  samtools view -q 30 -b "${PREFIX}.filtered.bam" > "${PREFIX}.HQ.bam"
  rm -f "${PREFIX}.filtered.bam" "${PREFIX}.filtered.bam.bai"
  samtools index "${PREFIX}.HQ.bam"

  # 8. Add Read Groups
  RGID=$PREFIX
  RGLB=$(echo "$PREFIX" | awk -F'_' '{print $3}')
  RGPL="ILLUMINA"
  RGPU=$(echo "$PREFIX" | awk -F'_' '{print $(NF-1)"_"$NF}')
  RGSM=$(echo "$PREFIX" | awk -F'_' '{print $1"_"$2}')

  gatk AddOrReplaceReadGroups \
    -I "${PREFIX}.HQ.bam" \
    -O "${PREFIX}.HQ.RG.bam" \
    -RGID "$RGID" \
    -RGLB "$RGLB" \
    -RGPL "$RGPL" \
    -RGPU "$RGPU" \
    -RGSM "$RGSM"

  rm -f "${PREFIX}.HQ.bam" "${PREFIX}.HQ.bam.bai"

  # 9. Final index
  samtools index "${PREFIX}.HQ.RG.bam"

  echo "✅ Completed: ${PREFIX}.HQ.RG.bam"
done

echo "🏁 All done!"