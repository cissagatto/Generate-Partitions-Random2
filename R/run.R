##############################################################################
# Generate Random Partitions version 2                                       #
# Copyright (C) 2021                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
# Ferrandin | Federal University of Sao Carlos                               #
# (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos | Computer Department  #
# (DC: https://site.dc.ufscar.br/) | Program of Post Graduation in Computer  #
# Science (PPG-CC: http://ppgcc.dc.ufscar.br/) | Bioinformatics and Machine  #
# Learning Group (BIOMAL: http://www.biomal.ufscar.br/)                      #
#                                                                            #
##############################################################################


###########################################################################
#
###########################################################################
FolderRoot = "~/Generate-Partitions-Random2"
FolderScripts = "~/Generate-Partitions-Random2/R"


##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             # 
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
executeGPR2 <- function(ds,
                        dataset_name,
                        number_dataset, 
                        number_cores, 
                        number_folds, 
                        folderResults){
  
  
  # LOAD LIBRARIES
  setwd(FolderScripts)
  source("libraries.R")
  
  setwd(FolderScripts)
  source("utils.R")
  
  setwd(FolderScripts)
  source("generateR2.R")
  
  diretorios <- directories(dataset_name, folderResults)
  
  if(number_cores == 0){
    
    cat("\n\n##########################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please      #")
    cat("\n# choose a value greater than or equal to 1.               #")
    cat("\n############################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n##########################################################")
      cat("\n# Running Sequentially!                                    #")
      cat("\n############################################################\n\n")
    } else {
      cat("\n\n############################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!         #")
      cat("\n##############################################################\n\n")
    }
  }
  cl = cl
  
  retorno = list()
  
  
  cat("\n\n##########################################################")
  cat("\n# RUN: get labels                                          #")
  cat("\n##############################################################\n\n")
  arquivo = paste(diretorios$folderNamesLabels, "/" ,
                  dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  
  
  cat("\n\n##########################################################")
  cat("\n# RUN: Get the label space                                 #")
  cat("\n###########################################################\n\n")
  timeLabelSpace = system.time(resLS <- labelSpace(ds,
                                                   dataset_name,
                                                   number_dataset, 
                                                   number_cores, 
                                                   number_folds, 
                                                   folderResults))
  
  
  cat("\n\n#########################################################")
  cat("\n# RUN: Get partitions random2                             #")
  cat("\n###########################################################\n\n")
  timeGPR2 = system.time(resGPR2 <- generateR2(ds,
                                               dataset_name,
                                               number_dataset, 
                                               number_cores, 
                                               number_folds, 
                                               folderResults,
                                               namesLabels,
                                               resLS))
  
  
  cat("\n\n##########################################################")
  cat("\n# RUN: Runtime                                             #")
  cat("\n###########################################################\n\n")
  timesExecute = rbind(timeLabelSpace, timeGPR2)
  setwd(diretorios$folderOutputDataset)
  write.csv(timesExecute, paste(dataset_name, "-RunTime-GPR2.csv", sep=""))
  
  
  cat("\n\n#########################################################")
  cat("\n# RUN: Stop Parallel                                      #")
  cat("\n###########################################################\n\n")
  on.exit(stopCluster(cl))
  
  
  gc()
  cat("\n\n#########################################################")
  cat("\n# RUN: END OF GPR2                                       #") 
  cat("\n###########################################################\n\n")
  
  
  gc()
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################