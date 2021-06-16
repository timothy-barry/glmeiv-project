# This script sets up all analyses reported in the glmeiv paper. It 1) creates
# a nextflow.config file in the current directory (which can be used to set several
# advanced options to simulatr) and 2) sets up to offsite (or data) directory
# structure.

# source LOCAL_GLMEIV_DATA_DIR and REMOTE_GLMEIV_DATA_DIR
source ~/.research_config

# 1. Create a nextflow config file. Create a "work" directory in
# the top-level glmeiv data directory.
if [ ! -f nextflow.config ]; then
  touch nextflow.config
  echo workDir = \"$LOCAL_GLMEIV_DATA_DIR\work\" >> nextflow.config
fi

# 2. Initialize the offsite directory structure
sim_dir=$LOCAL_GLMEIV_DATA_DIR"private/simulations"
mkdir -p $sim_dir $LOCAL_GLMEIV_DATA_DIR"public"

# 3. Simulations section
# i. Create the simulatr_specifier objects
Rscript simulations/create_simulatr_spec_objects.R
# ii. Run the simulations
$SIMULATR -f $sim_dir"/sim_spec_1.rds" -b 10
