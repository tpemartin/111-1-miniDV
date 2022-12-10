library(econDV2)
h=c(255, 71, 13)
c=60 #input$c
l=60 #input$l
dl=17 #input$dl
colorValues = {
  h |> purrr::map(~{
    c(econDV2::hclToHex(h=.x, c=c,l=l),
      econDV2::hclToHex(h=.x, c=c, l=l+dl))}) |>
    unlist()
}
pltAreaAnimalNoColorScale$make() + scale_fill_manual(
  limits = c("牛", "小牛", "羊", "小羊", "馬", "小馬"),
  values = colorValues)
