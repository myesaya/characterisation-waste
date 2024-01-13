library (tidyverse)
library(here)
library(gt)
library(janitor)
library(gtsummary)
library(treemapify)


# -------------------------------------------------------------------------

# Load data

  composition<- read_csv(here::here("data/raw/wcs-waste-composition.csv"))
  coefficients<- read_csv(here::here("data/raw/coefficients.csv"))


# -------------------------------------------------------------------------

#join the datasets
  characterisation <- composition |>
    left_join(coefficients, by = "waste_category")

# -------------------------------------------------------------------------
  characterisation <- characterisation |>
    clean_names()


# -------------------------------------------------------------------------

  #data to be equivalent to Excel
  characterisation <- characterisation |>
    mutate(kg_weight_net=kg_weight_gross-kg_weight_tare) |>
    relocate(kg_weight_net,
             .after = kg_weight_tare) #checking new dimension

  #replace NA with 0
  characterisation <- characterisation |>
    mutate(kg_weight_tare=replace_na(kg_weight_tare, 0)) |>
    drop_na(kg_weight_gross) |>
    mutate(kg_weight_burned=capacity_kg_4*kg_weight_net/sample) |>
    relocate(kg_weight_burned,
             .after = kg_weight_net) #checking new dimension



  characterisation |>
    group_by(waste_category) |>
    summarise(total=sum(kg_weight_burned))



  characterisation |>
    group_by(waste_category) |>
    summarise(total=sum(kg_weight_net)) #why do they not match?

  characterisation |>
    summarise(total=sum(kg_weight_net)) #matches  total in the paper


# -------------------------------------------------------------------------

#summarize the kg_weight_net of all the waste categories stratified by origin
  characterisation %>%
    group_by(origin) %>%
    summarise(total = sum(kg_weight_net)) %>%
    ggplot(aes(x = "", y = total, fill = origin)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = "Waste composition by origin",
         subtitle = "Sorted waste in kg",
         caption = "Source: WCS 2020",
         fill = "Origin") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = paste0(round(total/sum(total)*100), "%")), position = position_stack(vjust = 0.5))



# -------------------------------------------------------------------------
  #pie chart for waste composition
  characterisation %>%
    group_by(waste_category) %>%
    summarise(total = sum(kg_weight_net)) %>%
    ggplot(aes(x = "", y = total, fill = waste_category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +

    labs(title = "Waste composition by category",
         subtitle = "Sorted waste in kg",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) + #this working in agreement with the paper
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = paste0(round(total/sum(total)*100), "%")), position = position_stack(vjust = 0.5))



#treemap for waste composition

  characterisation %>%
    group_by(waste_category) %>%
    summarise(total = sum(kg_weight_net)) %>%
    mutate(percent = scales::percent(total/sum(characterisation$kg_weight_net), accuracy = 0.1)) %>%
    ggplot(aes(area = total, fill = waste_category, label = paste(waste_category, "\n", percent))) +
    geom_treemap(show.legend = FALSE) +
    geom_treemap_text(colour = "black", place = "centre", size = 11,
                      aes(label = paste(waste_category, "\n", percent))) +
    labs(title = "Waste composition by category",
         subtitle = "Sorted waste by percentage",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set3")
 #treemap for waste composition with the percentages in brackets

  characterisation %>%
    group_by(waste_category) %>%
    summarise(total = sum(kg_weight_net)) %>%
    mutate(percent = scales::percent(total/sum(total), accuracy = 0.1)) %>%
    ggplot(aes(area = total, fill = waste_category, label = paste(waste_category, "\n", "(", percent, ")", sep = ""))) +
    geom_treemap(show.legend = FALSE) +
    geom_treemap_text(colour = "black", place = "centre", size = 11,
                      aes(label = paste(waste_category, "\n", "(", percent, ")", sep = ""))) +
    labs(title = "Waste composition by category",
         subtitle = "Sorted waste by percentage",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set3")
 # -------------------------------------------------------------------------
  #waste sources
  characterisation %>%
    group_by(waste_category, origin) %>%
    summarise(total = sum(kg_weight_net)) %>%
    pivot_wider(names_from = origin,
                values_from = total) %>%
    gt() #needs attention to modify values of household and market only industry is correct

  # characterisation %>%
  #   group_by(origin,waste_category) %>%
  #   summarise(total = sum(kg_weight_net)) %>%
  #   pivot_wider(names_from = origin,
  #               values_from = total) %>%
  #   gt()


 characterisation %>%
    group_by(origin,waste_category) %>%
    summarise(total = sum(kg_weight_net)) %>%
    pivot_wider(names_from = origin,
                values_from = total) %>%
    gt() #same as above but better visualization

 #add row totals
 # Assuming df is the name of your gt table
 library(gt)

 # Add row totals


 # Customize the gt table
 b <- a %>%
   tab_header(title = md("Waste composition by origin")) %>%
   tab_spanner(label = md("Waste categories"), columns = 2:4) %>%
   tab_stubhead(label = md("Origin")) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_stubhead()) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_column_labels()) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(weight = "bold")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(color = "white", font_style = "italic")),
             locations = cells_body(columns = vars(household, industry, market))) %>%
   tab_style(style = list(cell_text(color = "white", font_style = "italic")),
             locations = cells_body(columns = vars(household, industry, market)))

 # Print the modified gt table
 df


  # -------------------------------------------------------------------------

#comparison percentage chart
  #make a ggplot showing the percentage of each waste category by origin
  characterisation %>%
    group_by( waste_category,origin) %>%
    summarise(total = sum(kg_weight_net)) %>%
    mutate(percentage = total/sum(total)*100) %>%
    ggplot(aes(x = waste_category, y = percentage, fill = origin)) +
    geom_col(position = "fill") +
    labs(title = "Waste composition by origin",
         subtitle = "Percentage of waste in kg",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 13),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 13),
          axis.text.y = element_text(size = 13,
          ) ) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = paste0(round(percentage), "%")), position = position_fill(vjust = 0.5)) +

    theme(legend.position = "bottom")



# -------------------------------------------------------------------------
  #Overall Composition of Households Waste pie chart
  characterisation %>%
    filter(origin == "Household") %>%
    group_by(waste_category) %>%
    summarise(total = sum(kg_weight_net)) %>%
    ggplot(aes(x = "", y = total, fill = waste_category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = "Overall Composition of Households Waste",
         subtitle = "Sorted waste in kg",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = paste0(round(total/sum(total)*100), "%")), position = position_stack(vjust = 0.5))

# -------------------------------------------------------------------------

  #Percentage Comparison Chart for Waste Composition from Low, Medium and High-Income Locations

  characterisation %>%
    filter(hh_economics != "NA") %>%
    group_by( waste_category,hh_economics) %>%
    summarise(total = sum(kg_weight_net)) %>%
    mutate(percentage = total/sum(total)*100) %>%
    ggplot(aes(x = waste_category, y = percentage, fill = hh_economics)) +
    geom_col(position = "fill") +
    labs(title = "Waste composition by origin",
         subtitle = "Percentage of waste in kg",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 13),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 13),
          axis.text.y = element_text(size = 13,
          ) ) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = paste0(round(percentage), "%")), position = position_fill(vjust = 0.5)) +

    theme(legend.position = "bottom")

# -------------------------------------------------------------------------

#Overall Composition of market Waste pie chart

  characterisation %>%
    filter(origin == "Market") %>%
    group_by(waste_category) %>%
    summarise(total = sum(kg_weight_net)) %>%
    ggplot(aes(x = "", y = total, fill = waste_category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = "Overall Composition of market Waste",
         subtitle = "Sorted waste in kg",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = paste0(round(total/sum(total)*100), "%")), position = position_stack(vjust = 0.5))


# -------------------------------------------------------------------------

 # Overall Composition of industrial Waste pie chart

  characterisation %>%
    filter(origin == "Industries") %>%
    group_by(waste_category) %>%
    summarise(total = sum(kg_weight_net)) %>%
    ggplot(aes(x = "", y = total, fill = waste_category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = "Overall Composition of industrial Waste",
         subtitle = "Sorted waste in kg",
         caption = "Source: WCS 2020",
         fill = "Waste categories") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = paste0(round(total/sum(total)*100), "%")), position = position_stack(vjust = 0.5))

# -------------------------------------------------------------------------
 #use here here to write data to the processed folder
 write_csv(characterisation, here::here("data/processed/characterisation.csv"))

