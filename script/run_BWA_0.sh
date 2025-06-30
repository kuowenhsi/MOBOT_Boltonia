#!/bin/bash
cd /storage1/fs1/christine.e.edwards/Active/Wen/IMLS_Illumina/Boltonia_trimmed
nbcor=8
set -e
parallel -j ${nbcor} bwa mem -t 2 /storage1/fs1/christine.e.edwards/Active/Wen/IMLS_Ref/Boltonia_hap1/Boltonia_hap1_1.0.fasta {}_R1.fastq.gz {}_R2.fastq.gz '>' {}.sam ::: $(ls ./Boltonia_parallel_0/*_R1.fastq.gz | sed 's/_R1.fastq.gz//')
parallel -j ${nbcor} samtools view -b -S -h {}.sam '>' {}.temp.bam ::: $(ls ./Boltonia_parallel_0/*.sam | sed 's/.sam//')
rm -f ./Boltonia_parallel_0/*.sam
parallel -j ${nbcor} samtools sort {}.temp.bam -o {}.sort.bam ::: $(ls ./Boltonia_parallel_0/*.temp.bam | sed 's/.temp.bam//')
rm -f ./Boltonia_parallel_0/*.temp.bam
parallel -j ${nbcor} samtools index {}.sort.bam ::: $(ls ./Boltonia_parallel_0/*.sort.bam | sed 's/.sort.bam//')
run_markduplicates() {
  local prefix=$1
  local attempt=0
  until [ $attempt -ge 3 ]; do
    gatk --java-options "-Xmx8g" MarkDuplicates \
      -I ${prefix}.sort.bam \
      -O ${prefix}.dedup.bam \
      -M ${prefix}.metrics.txt && break
    attempt=$((attempt+1))
    echo "Retrying $prefix (attempt $attempt)..."
    sleep 10
  done
  if [ $attempt -eq 3 ]; then
    echo "MarkDuplicates failed for $prefix after 3 attempts." >&2
    exit 1
  fi
}

export -f run_markduplicates
parallel --jobs 4 run_markduplicates ::: $(ls ./Boltonia_parallel_0/*.sort.bam | sed 's/.sort.bam//')
rm -f ./Boltonia_parallel_0/*.sort.bam
parallel --jobs ${nbcor} samtools view -F 3844 -b {}.dedup.bam '>' {}.filtered.bam ::: $(ls ./Boltonia_parallel_0/*.dedup.bam | sed 's/.dedup.bam//')
rm -f ./Boltonia_parallel_0/*.dedup.bam
parallel --jobs ${nbcor} samtools index {}.filtered.bam ::: $(ls ./Boltonia_parallel_0/*.filtered.bam | sed 's/.filtered.bam//')
parallel --jobs ${nbcor} samtools view -q 30 -b {}.filtered.bam '>' {}.HQ.bam ::: $(ls ./Boltonia_parallel_0/*.filtered.bam | sed 's/.filtered.bam//')
rm -f ./Boltonia_parallel_0/*.filtered.bam
parallel --jobs ${nbcor} samtools index {}.HQ.bam ::: $(ls ./Boltonia_parallel_0/*.HQ.bam | sed 's/.HQ.bam//')
for INPUT_BAM in $(ls ./Boltonia_parallel_0/*.HQ.bam | sed 's/.HQ.bam//'); do
  RGID=$(basename $INPUT_BAM)
  RGLB=$(basename $INPUT_BAM | awk -F'_' '{print $3}')
  RGPL="ILLUMINA"
  RGPU=$(basename $INPUT_BAM | awk -F'_' '{print $(NF-1)"_"$NF}')
  RGSM=$(basename $INPUT_BAM | awk -F'_' '{print $1"_"$2}')
  echo "Processing: $INPUT_BAM with RGLB=$RGLB"
  gatk AddOrReplaceReadGroups \
    -I ${INPUT_BAM}.HQ.bam \
    -O ${INPUT_BAM}.HQ.RG.bam \
    -RGID $RGID \
    -RGLB $RGLB \
    -RGPL $RGPL \
    -RGPU $RGPU \
    -RGSM $RGSM
done
rm -f ./Boltonia_parallel_0/*.HQ.bam ./Boltonia_parallel_0/*.bai
parallel --jobs ${nbcor} samtools index {}.HQ.RG.bam ::: $(ls ./Boltonia_parallel_0/*.HQ.RG.bam | sed 's/.HQ.RG.bam//')
