
path_vcf0=/storage1/fs1/christine.e.edwards/Active/Wen/IMLS_Illumina/Boltonia_vcf/alloutgroup/after_advance_ID/
plink2=/storage1/fs1/christine.e.edwards/Active/aguirre/plink2/plink2
sample_list=/storage1/fs1/christine.e.edwards/Active/aguirre/boltonia/scripts/samples_for_analysis.txt
path_out=/storage1/fs1/christine.e.edwards/Active/aguirre/boltonia/subset_data

cd $path_out 
#STEP 1: convert vcf to plink2 no subsetting yet
for VCF in $path_vcf0/*.vcf.gz; do
    base_name=$(basename "${VCF}" .vcf.gz)
    $plink2 --vcf "${VCF}" --make-pgen --out "${base_name}_full"
done


# STEP 2: merge  per chromosome
for CHR in 1 2 3 4 5 6 7 8 9
do
ls *full.pgen | grep Chr_$CHR | sed 's/.pgen$//' > tmp_merge_list.txt
$plink2 --pmerge-list tmp_merge_list.txt  --out Boltonia_Chr_${CHR}_full_merged
done


# Step 3: Subset samples
for CHR in $(ls *full_merged.pgen | sed 's/.pgen$//')
do
$plink2 --pfile $CHR --keep $sample_list --make-pgen --out ${CHR}_subsetted
done


# Step 4: LD pruning
for CHR in $(ls *full_merged_subsetted.pgen | sed 's/.pgen$//')
do
$plink2 --pfile $CHR --indep-pairwise 50 5 0.2 -make-pgen --out ${CHR}_LD
done

# Step 5: Export LD-pruned VCF
for CHR in $(ls *_full_merged_subsetted_LD.pgen | sed 's/.pgen$//')
do
$plink2 --pfile ${CHR} --extract ${CHR}.prune.in --export vcf --out ${CHR}_pruned
done

#Since it does not take that long to run all these commands I will eliminate the intermediate files
mkdir LD_pruned_vcf
mv *pruned.* LD_pruned_vcf
rm *