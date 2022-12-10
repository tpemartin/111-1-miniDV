createProjectIcelandData = function(){
  googlesheets4::read_sheet(
    ss="https://docs.google.com/spreadsheets/d/1QvzABBQP4T_1rH0e-dR32WeIcq05taPqKqAVnsmmThM/edit#gid=830350355",
    sheet="Sheet2"
  ) -> sheet
  
  project <- list()
  project$data <- list(
    import = function() googlesheets4::read_sheet(
      ss="https://docs.google.com/spreadsheets/d/1QvzABBQP4T_1rH0e-dR32WeIcq05taPqKqAVnsmmThM/edit#gid=830350355",
      sheet="Sheet2"
    ),
    value=sheet
  )
  project |> 
    computeAverageFarmValue() -> project
  return(project)
}
computeAverageFarmValue = function(project){
  project$data$value |>
    dplyr::mutate(
      dplyr::across(
        .cols="牛":"小馬",
        .fns = function(x) x/農場數
      )
    ) ->
    project$data$value
  return(project)
}
pivotLongdata = function(project){
  project$data$value |> 
    tidyr::pivot_longer(
      cols = "牛":"小馬",
      names_to = "animal",
      values_to = "quantity"
    ) |>
    dplyr::mutate(
      animal = factor(animal, 
                      levels = c("牛", "小牛", "羊", "小羊", "馬", "小馬"))  
    )-> project$dataLong$value
  project$dataLong$value$animalCat = project$dataLong$value$animal
  levels(project$dataLong$value$animalCat) <- rep(c("牛", "羊", "馬"), each=2)
  project$dataLong$value |> 
    dplyr::mutate(
      areaAnimalCat = paste0(地區,"-",animalCat)#,
      # 地區2=paste0(地區,"-",地區縮寫)
    ) -> project$dataLong$value
  unique(project$dataLong$value$areaAnimalCat) |> 
    stringr::str_extract("^[^-]+-") |>
    paste0(
      rev(c("牛","羊","馬"))) -> newLevelsAreaAnimcalCat
  newLevelsAreaAnimcalCat
  library(magrittr)
  project$dataLong$value$areaAnimalCat %<>% factor(levels=newLevelsAreaAnimcalCat)
  project$dataLong$value$地區 %<>% factor(levels=rev(getLevelsAreaByValue(project)))
  # project$dataLong$value$地區2 %<>% factor(levels=rev(getLevelsAreaByValue(project)))
  
  
  return(project)
}
getLevelsAreaByValue = function(project3){
  is.na(project3$data$value) -> whereIsNa
  project3$data$value[whereIsNa] <- 0
  
  project3$data$value |> 
    dplyr::mutate(
      cowSheeps= 牛+羊/6
    ) |>
    dplyr::arrange(
      cowSheeps
    ) |>
    dplyr::pull(地區) -> levelsAreaByValue
  levelsAreaByValue
}
test = function(){
plt$scale3 <- scale_fill_manual(
  limits = c("牛", "小牛", "羊", "小羊", "馬", "小馬"),
  values = c("#003066","#5b72ac", "#665700", "#827125", "#660200" ,"#b65240"))
}
plotByAreaAnimalNoColorScale = function(project3){
  plt = econDV2::Plot()
  plt$ggplot = ggplot()
  plt$geom <- geom_col(
    data=project3$dataLong$value,
    mapping = aes(
      x=areaAnimalCat, y=quantity, fill = animal, group=animalCat
    ),
    position="stack"
  )
  plt$coord = coord_flip()
  plt$scale4 = scale_x_discrete(
    labels = function(x) stringr::str_extract(x,'-[^-]+$') |> stringr::str_remove("-")
  )
  project3$dataLong$value |>
    dplyr::filter(
      animal %in% c("羊", "小羊")
    ) |>
    dplyr::group_by(
      地區
    ) |>
    dplyr::summarise(
      quantity= sum(quantity, na.rm=T)
    ) |>
    dplyr::pull(
      quantity
    ) |> range(na.rm=T) -> rangeQ
  
  breaks = sort(seq(0,15,by=5) |> c(3,rangeQ[[2]]))
  labels = as.character(as.integer(breaks[-length(breaks)])) ##//, round(breaks[[length(breaks)]],2))
  labels = c(labels, as.character(round(breaks[[length(breaks)]],2)))
  labels
  plt$scale5 = scale_y_continuous(expand = c(0,0), sec.axis = dup_axis(),
                                  breaks=breaks, labels=labels)
  plt$facet = facet_grid(rows = vars(地區), scales = "free_y", switch="y")
  return(plt)

}
plotByAreaAnimal = function(project3){
  
  plt = econDV2::Plot()
  plt$ggplot = ggplot()
  plt$geom <- geom_col(
    data=project3$dataLong$value,
    mapping = aes(
      x=areaAnimalCat, y=quantity, fill = animal, group=animalCat
    ),
    position="stack"
  )
  h=c(255, 71, 13)
  c=60
  l=60
  colorValues = {
    h |> purrr::map(~{
      c(econDV2::hclToHex(h=.x, c=c,l=l),
        econDV2::hclToHex(h=.x, c=c, l=l+10))}) |>
      unlist()
  }
  plt$scale3 <- scale_fill_manual(
    limits = c("牛", "小牛", "羊", "小羊", "馬", "小馬"),
    values = colorValues)
  plt$coord = coord_flip()
  plt$scale4 = scale_x_discrete(
    labels = function(x) stringr::str_extract(x,'-[^-]+$') |> stringr::str_remove("-")
  )
  project3$dataLong$value |>
    dplyr::filter(
      animal %in% c("羊", "小羊")
    ) |>
    dplyr::group_by(
      地區
    ) |>
    dplyr::summarise(
      quantity= sum(quantity, na.rm=T)
    ) |>
    dplyr::pull(
      quantity
    ) |> range(na.rm=T) -> rangeQ
  
  breaks = sort(seq(0,15,by=5) |> c(3,rangeQ[[2]]))
  labels = as.character(as.integer(breaks[-length(breaks)])) ##//, round(breaks[[length(breaks)]],2))
  labels = c(labels, as.character(round(breaks[[length(breaks)]],2)))
  labels
  plt$scale5 = scale_y_continuous(expand = c(0,0), sec.axis = dup_axis(),
                                  breaks=breaks, labels=labels)
  plt$facet = facet_grid(rows = vars(地區), scales = "free_y", switch="y")
  return(plt)
}

highlightB2 = function(plt, project, colors){
  project$data$value |>
    split(project$data$value$地區縮寫=="B2") -> project$dataWithActors$value
  
  plt$geom = geom_col(
    data=project$dataWithActors$value$`FALSE`,
    aes(
      x=地區, y=人
    ),
    fill = colors$vanGoghStarNight$night[[2]]
  )
  plt$geom2 = geom_col(
    data=project$dataWithActors$value$`TRUE`,
    aes(
      x=地區, y=人
    ), fill = colors$vanGoghStarNight$stars[[2]]
  )
}
getAnimalColors = function(){
  cl = list(
    limits = c("牛", "小牛", "羊", "小羊", "馬", "小馬"),
    values = c("#003066","#5b72ac", "#665700", "#827125", "#660200" ,"#b65240"))
  cl$values |> econDV2::hexToHcl() |> dput()
  hclValues = list(H = c(255.020713870275, 255.919030895471, 71.3759869333266, 
                         71.3078806829314, 12.5146698962646, 18.3681202872377), 
                   C = c(42.6253627250032, 52.7459217213959, 41.8095391830686, 
                         46.948631893064, 64.5099939118061, 80.5090362752642), 
                   L = c(20.332667723198, 48.5561833022203, 37.1915955566048, 
                         47.8676996850081, 19.512000053511, 47.433809811758))

  
  econDV2::hclToHex(h=h[[1]],c=c,l=l)
  
  hclValues$H[c(1,3,5)] |> round() |> dput()
  hclValues$C[c(1,3,5)] |> round() |> dput()
  hclValues$L[c(1,3,5)]
}
makeAreaDescending = function(plt, project){
  plt$scale2 = scale_x_discrete(limits=project$levels$descending$地區)
  return(plt)}
makeAreaAcending = function(plt, project){
  plt$scale2 = scale_x_discrete(limits=project$levels$ascending$地區)
  return(plt)
}
plotPopulation1 <- function(project, colors, style2){
  plt = econDV2::Plot()
  plt$ggplot=ggplot()
  plt$geom = geom_col(
    data=project$data$value,
    aes(
      x=地區, y=人
    ),
    fill = colors$vanGoghStarNight$night[[2]]
  )
  plt$xlab = xlab(NULL)
  
  plt$style = style2$scale_y_continuous()
  plt$theme = style2$theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plt)
}
getAreaLevels <- function(project){
  project$data$value |>
    dplyr::arrange(desc(人)) |>
    dplyr::pull(地區) -> project$levels$descending$地區 #  levels_x
  
  project$data$value |>
    dplyr::arrange(人) |>
    dplyr::pull(地區) -> project$levels$ascending$地區
  return(project)
}
finalPolish = function(pltAreaAnimal){
  
  pltAreaAnimal$polish <- list(
    theme(strip.text.y.left = element_text(angle = 0),
                             axis.line.y=element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.text.y=element_blank(), legend.position = "top",
          axis.line.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major.x = element_line(color="grey")),
    labs(title="何處農場最值錢？", subtitle = "數字為除以農場數的平均值"),
    xlab(NULL),ylab(NULL),
    guides(
      fill=guide_legend(title=NULL, ncol=3)
    ))
  pltAreaAnimal
}