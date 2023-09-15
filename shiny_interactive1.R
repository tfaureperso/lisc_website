---
title: "Income and Housing Costs by Zip Code"
output: html_document
resource_files:
  - mamarkdowndata.rdata
- zip_mass_appdata_for_map.rds
runtime: shiny
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
options(scipen = 999)
library("DT")
library("dplyr")
library("ggplot2")
library("scales")
library("leaflet")
library("shiny")
library("glue")
library("rmarkdown")
library("sf")

load("mamarkdowndata.rdata") # loads variable markdowndata
ma_appdata_for_map <- readRDS("zip_mass_appdata_for_map.rds")
```

```{r user_select_city}

selectInput("mycities", "Choose 1 or more Massachusetts places:", choices = c("All Mass", sort(unique(markdowndata$City))), multiple = TRUE, selected = "Boston")
```
**Note: some cities may have more than one place name for Zip codes. For example, Allston, Brighton, Dorchester, and several other neighborhoods are not included in Zip code place name "Boston".**


  ```{r my_data}
mydata <- reactive({
  if("All Mass" %in% input$mycities){
    markdowndata

  } else {
    filter(markdowndata, City %in% input$mycities)
  }
})

# mymapdata <- reactive({
#   if("All Mass" %in% input$mycities){
#     ma_appdata_for_map %>%
#       mutate(
#         Highlighted = "No"
#       )
#   } else {
#     dplyr::filter(ma_appdata_for_map, City %in% input$mycities)
#   }
# })

zip_highest_income_row <- reactive({
  filter(mydata(), MedianHouseholdIncome == max(MedianHouseholdIncome, na.rm = TRUE))
})
zip_lowest_income_row <- reactive({
  filter(mydata(), MedianHouseholdIncome >= 5000) %>%
    filter(MedianHouseholdIncome == min(MedianHouseholdIncome, na.rm = TRUE))
})

selected_places <- reactive({
  if("All Mass" %in% input$mycities){
    "Massachusetts"
  } else {
    paste(input$mycities,
          sep = " ", collapse = ", ")
  }
})

```

<br />
  Zip code `r renderText(zip_highest_income_row()$ZipCode[1])` in
`r renderText(zip_highest_income_row()$City[1])`
has the highest median income in `r renderText(selected_places())` at
`r renderText(scales::dollar(zip_highest_income_row()$MedianHouseholdIncome[1]))`.

Zip code `r renderText(zip_lowest_income_row()$ZipCode[1])` in
`r renderText(zip_lowest_income_row()$City[1])` has the lowest
median income in `r renderText(selected_places())` at
`r renderText(scales::dollar(zip_lowest_income_row()$MedianHouseholdIncome[1]))`.


```{r mymap, warning=FALSE, message=FALSE}
mapdata <- reactive({
  if("All Mass" %in% input$mycities){
    ma_appdata_for_map %>%
      dplyr::select(ZipCode = GEOID, MedianHouseholdIncome = medincome, MedianMonthlyHousingCost = medmonthlyhousingcost, Population = pop, City, County = county.name, State, Lat, Long, income, housing, Pop, geometry) %>%
      mutate(
        Highlighted = "Yes"
      ) %>%
      sf::st_as_sf()
  } else {
    dplyr::filter(ma_appdata_for_map, City %in% input$mycities) %>%
      dplyr::select(ZipCode = GEOID, MedianHouseholdIncome = medincome, MedianMonthlyHousingCost = medmonthlyhousingcost, Population = pop, City, County = county.name, State, Lat, Long, income, housing, Pop, geometry) %>%
      dplyr::mutate(
        Highlighted = ifelse(City %in% input$mycities, "Yes", "No")
      ) %>%
      sf::st_as_sf()
  }
})

incomepal <- reactive({
  leaflet::colorNumeric(palette = "Greens", domain = mapdata()$MedianHouseholdIncome)
})
housingpal <- reactive({
  leaflet::colorNumeric(palette = "Greens", domain = mapdata()$MedianMonthlyHousingCost)
})

mypopups <- reactive({
  glue::glue("Zip Code: {mapdata()$ZipCode}<br />Median Household Income: {mapdata()$income}<br />Median Monthly Housing Cost: {mapdata()$housing}<br />Population: {mapdata()$Pop}<br />City: {mapdata()$City}<br />County: {mapdata()$County}")
})

```


<br />
  <h4>Map of median household incomes and monthly housing costs</h4>

  (Click a Zip code to get complete data)

```{r themap}
leaflet::renderLeaflet({
  leaflet(mapdata()) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~incomepal()(mapdata()$MedianHouseholdIncome),
                fillOpacity = 0.7,
                weight = 1.0,
                color = "black",
                smoothFactor = 0.2,
                popup = mypopups(),
                group = "Household Income"
    ) %>%
    addPolygons(fillColor = ~housingpal()(mapdata()$MedianMonthlyHousingCost),
                fillOpacity = 0.7,
                weight = 0.2,
                color = "black",
                smoothFactor = 0.2,
                popup = mypopups(),
                group = "Housing Costs"
    ) %>%
    addLayersControl(
      baseGroups=c("Household Income", "Housing Costs"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    )
})
```


<h4>Histogram for `r renderText(selected_places())` income data</h4>



  ```{r histo, warning=FALSE, message=FALSE}

renderPlot(width = 500, height = 300, {
  ggplot(mydata(), aes(x = MedianHouseholdIncome)) +
    geom_histogram(binwidth = 20000, color = "black", fill = "darkgreen") +
    theme_classic() +
    xlab("") +
    ylab("")  +
    scale_x_continuous(labels = dollar)
})

```


<h4>`r renderText(selected_places())` data</h4>



  ```{r mytable}
renderDT({
  DT::datatable(mydata(), filter = 'top') %>%
    formatCurrency(4:5, digits = 0) %>%
    formatCurrency(6, currency = "", digits = 0)
})

```






