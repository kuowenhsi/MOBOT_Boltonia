#!/bin/bash

INPUT_DIR=/data/Boltonia_toy
OUTPUT_DIR=$INPUT_DIR/fastqc_results
count=0
processed_any=false

for file in "$INPUT_DIR"/*.fastq.gz; do
  base=$(basename "$file" .fastq.gz)
  output_html="$OUTPUT_DIR/${base}_fastqc.html"

  if [ -f "$output_html" ]; then
    continue
  fi

  echo "Processing: $file"
  fastqc -o "$OUTPUT_DIR" "$file"
  count=$((count + 1))
  processed_any=true

  if [ $count -ge 10 ]; then
    break
  fi
done

if [ "$processed_any" = false ]; then
  echo "No more files left to process, job complete!"
fi