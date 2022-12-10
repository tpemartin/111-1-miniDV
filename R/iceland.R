
source("./R/icelandSupport.R")
project = createProjectIcelandData()

martinStyle2::Style2() -> style2
martinStyle2::Colors() -> colors

pltPopulation1 = plotPopulation1()
pltPopulation1$make()


 #  levels_x

plt$make()


plt$make()


plt$make()



names(project$dataLong$value) 
unique(project$dataLong$value$animal) |> dput()

levels(project3$dataLong$value$animalCat)

project3$dataLong$value |> 
  dplyr::select(地區, animalCat) |> 
  dplyr::distinct() 


# plt$theme = theme(
#   legend.position = "top",
#   text = element_text(angle = 0),
#   axis.title = element_text(angle = 0),
#   strip.text = element_text(angel=0)
# )
# plt$theme <- NULL
# gg = plt$make() +theme(
#   legend.position = "top",
#   text = element_text(angle = 0),
#   axis.title.y = element_text(angle = 0),
#   strip.text = element_text(angel=0)
# )
# gg

plt$make()
