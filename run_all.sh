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
remote_sim_dir=$REMOTE_GLMEIV_DATA_DIR"private/simulations"
mkdir -p $sim_dir $LOCAL_GLMEIV_DATA_DIR"public"

# 3. Create the simulatr specifier objects
Rscript simulations/create_simulatr_spec_objects.R

# 4. Run the simulations
# i.
if [[ ! -f $sim_dir"/raw_result_1.rds" ]]
then
$SIMULATR -f $sim_dir"/sim_spec_1.rds" -r $sim_dir"/raw_result_1.rds"
fi

if [[ ! -f $sim_dir"/raw_result_2.rds" ]]
then
$SIMULATR -f $sim_dir"/sim_spec_2.rds" -r $sim_dir"/raw_result_2.rds"
fi
