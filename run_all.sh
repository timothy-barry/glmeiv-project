# This script sets up and runs all analyses. It 1) creates
# a nextflow.config file in the current directory (which can be used to set several
# advanced options to simulatr), 2) sets up the offsite (or data) directory
# structure, and 3) runs the simulations.

# source LOCAL_GLMEIV_DATA_DIR and REMOTE_GLMEIV_DATA_DIR
source ~/.research_config

# 1. Create a nextflow config file. Create a "work" directory in
# the top-level glmeiv data directory.
if [ -f nextflow.config ]; then
  rm nextflow.config
fi
touch nextflow.config
echo workDir = \"$LOCAL_GLMEIV_DATA_DIR\work\" >> nextflow.config

# 2. Initialize the offsite directory structure
sim_dir=$LOCAL_GLMEIV_DATA_DIR"private/simulations"
analysis_dir=$LOCAL_GLMEIV_DATA_DIR"private/data_analysis"
mkdir -p $sim_dir

# 3. Create the simulatr specifier objects
# Rscript simulations/create_simulatr_spec_objects.R FALSE
# Rscript simulations/create_simulatr_spec_objects_scaled.R TRUE

# 4. Run the simulations
# i.
# if [[ ! -f $sim_dir"/raw_result_1.rds" ]]
# then
# $SIMULATR -f $sim_dir"/sim_spec_1.rds" -r $sim_dir"/raw_result_1.rds"
# fi
# ii.
# if [[ ! -f $sim_dir"/raw_result_2.rds" ]]
# then
# $SIMULATR -f $sim_dir"/sim_spec_2.rds" -r $sim_dir"/raw_result_2.rds"
# fi
# iii.
# if [[ ! -f $sim_dir"/raw_result_3.rds" ]]
# then
# $SIMULATR -f $sim_dir"/sim_spec_3.rds" -r $sim_dir"/raw_result_3.rds"
# fi
# iv.
# if [[ ! -f $sim_dir"/raw_result_4.rds" ]]
# then
# $SIMULATR -f $sim_dir"/sim_spec_4.rds" -r $sim_dir"/raw_result_4.rds"
# fi
# v.
# if [[ ! -f $sim_dir"/raw_result_5.rds" ]]
# then
# $SIMULATR -f $sim_dir"/sim_spec_5.rds" -r $sim_dir"/raw_result_5.rds"
# fi
# vi.
# if [[ ! -f $sim_dir"/raw_result_6.rds" ]]
# then
# $SIMULATR -f $sim_dir"/sim_spec_6.rds" -r $sim_dir"/raw_result_6.rds"
# fi
# vii.
# if [[ ! -f $sim_dir"/raw_result_7.rds" ]]
# then
# $SIMULATR -f $sim_dir"/sim_spec_7.rds" -r $sim_dir"/raw_result_7.rds"
# fi

#5. Run nextflow pipeline
processed_dir=$LOCAL_GASPERINI_2019_DATA_DIR"at-scale/processed/"

# file paths to data
gene_exp_fp=$processed_dir"gasp_scale_gene_expressions.odm"
gene_exp_meta=$processed_dir"gasp_scale_gene_metadata.rds"
gRNA_counts_fp=$processed_dir"gasp_scale_gRNA_counts.odm"
gRNA_counts_meta=$processed_dir"gasp_scale_gRNA_metadata.rds"
pairs_fp=$processed_dir"gRNA_gene_pairs_sample.rds"

# path to result dir
result_dir=$LOCAL_GLMEIV_DATA_DIR"private/data_analysis"

nextflow $LOCAL_CODE_DIR"glmeiv-pipeline/main.nf" --gene_expressions_fp $gene_exp_fp --gene_expressions_meta $gene_exp_meta --gRNA_counts_fp $gRNA_counts_fp --gRNA_counts_meta $gRNA_counts_meta --pairs_fp $pairs_fp --result_dir $result_dir
