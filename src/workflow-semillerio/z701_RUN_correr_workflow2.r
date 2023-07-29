# Corrida general del workflow Semillerio
#Todos terminan en 2
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# corrida de cada paso del workflow
source("~/labo2023r/src/workflow-semillerio/z711_CA_reparar_dataset2.r")
source("~/labo2023r/src/workflow-semillerio/z721_DR_corregir_drifting2.r")
source("~/labo2023r/src/workflow-semillerio/z731_FE_historia2.r")
source("~/labo2023r/src/workflow-semillerio/z741_TS_training_strategy2.r")
source("~/labo2023r/src/workflow-semillerio/z751_HT_lightgbm2.r")
source("~/labo2023r/src/workflow-semillerio/z771_ZZ_final_semillerio2.r")