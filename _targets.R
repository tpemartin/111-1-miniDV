# 引入執行此.R檔所需套件
library(targets)
library(econDV2) # 使用 %t=% 所需
# 引入幫手函數
source("./R/icelandSupport.R")
# 設定完成targets plan所需套件
tar_option_set(packages=c("ggplot2", "dplyr", "econDV2", "martinStyle2"))
# targets plan
list(
  style2 %t=% martinStyle2::Style2(),
  colors %t=% Colors(),
  project %t=% createProjectIcelandData(),
  project2 %t=% getAreaLevels(project),
  project3 %t=% pivotLongdata(project2),
  
  ## plot -----
  pltPopulation1 %t=% plotPopulation1(project, colors, style2),
  pltPopulation1Descending %t=% makeAreaDescending(pltPopulation1, project2),
  pltPopulation1Ascending %t=% makeAreaAcending(pltPopulation1, project2),
  pltPopulation1AscendingHighlightB2 %t=% highlightB2(pltPopulation1Ascending, project, colors),
  pltAreaAnimal %t=% plotByAreaAnimal(project3),
  pltAreaAnimalNoColorScale %t=% plotByAreaAnimalNoColorScale(project3),
  pltAreaAnimalFinal %t=% finalPolish(pltAreaAnimal)
)
