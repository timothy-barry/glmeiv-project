# This script takes two arguments: machine name (uberduo or local) and an optional
# boolean (TRUE or FALSE). The machine name specifies the machine on which the
# script is being run; the optional boolean indicates whether to run on a small
# test example (TRUE) or the full data (FALSE).

# ex: bash run_simulation.bash local TRUE

case $1 in
  local)
  simulation_dir="/Users/timbarry/Box/glm-eiv/simulation_dir"
  ;;
  uberduo)
  simulation_dir="/raid6/Tim/glmeiv_offsite/simulation_dir"
  ;;
esac


# 1. generate the synthetic data
n_row=$(Rscript "setup_simulation.R" $simulation_dir ${2:-TRUE})

# 2. run the simulation in parallel with xargs
seq 1 $n_row | xargs -n 1 -P 50 -I {} Rscript "fit_simulated_data.R" $simulation_dir {}

# 3. combine the results
Rscript "combine_results.R" $simulation_dir
