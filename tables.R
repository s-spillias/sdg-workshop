## Create Tables

# Load data
summaries <- read_excel('pub_table_summaries.xlsx')

statements_example <- mysheets[["Theme_Sum"]] %>% select("Themes","No. of statements",                                   
                                   "subThemes"         ,                                  
                                   "Example statements** (statement #, stakeholder type)") %>% 
  filter(!is.na(subThemes)) %>% 
   mutate(Themes = case_when(Themes == "Moving forward" ~ paste0(.$Themes, ": n = 65"),
                             Themes == "Current context" ~ paste0(.$Themes, ": n = 38"),
                             Themes == "Environment" ~ paste0(.$Themes, ": n = 46"),
                             Themes == "Communities & employment" ~ paste0(.$Themes, ": n = 36"),
                             Themes == "Human health & food" ~ paste0(.$Themes, ": n = 9"))
                             ) %>% 
  gt(groupname_col = "Themes") %>% 
  # Add title
  tab_header(title = md("**Themes and subThemes**")) %>% 
  # Bold column names
  cols_label(#Themes = md("**Themes**"),
             'No. of statements' = md("**No. of statements**"),
             subThemes = md("**subThemes**"),
             "Example statements** (statement #, stakeholder type)" = md("**Example statements** (statement #, stakeholder type)"),
             # Summary = md("**Summary**")
  ) %>% 
  # Add captions
  tab_source_note(source_note = "Data: This Study") %>% 
  # tab_source_note(source_note = "Table by @dm_ferraro for #TidyTuesday") %>% # For some reason a newline wasn't working %>% 
  ## Alter cell width in summary
     cols_width(c( "Example statements** (statement #, stakeholder type)") ~ px(700),
    #               "subThemes" ~ pct(10)
    ) %>% 
  # Style the title
  tab_style(
    style = list(
      cell_fill(color = "#95AFBA"),
      cell_text(weight = "bold")
    ),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = list(
      cell_text(size = 2,
                style = "italic")
    ), locations = cells_body(columns = "Example statements** (statement #, stakeholder type)")
  ) %>% 
  tab_style(
    style = list(
      cell_text(align = "left")
    ),
    locations = cells_column_labels("No. of statements")
  ) %>% 
  # Background color
  tab_options(
    table.background.color = "#F3F6F7"
  ) %>% 
  tab_style(
    style = list(
      cell_text(align = "center")
    ), locations = cells_body(columns = 'No. of statements')
  ) 

# Save
# Had to run webshot::install_phantomjs() first
# gtsave(employed_gt, filename = here("2021-02-23", "employment.png")) # Doesn't save with all of the markdown
gtsave(statements_example,filename = here("Figures/statement_examples.png"))


# Make table
(summaries_gt <- summaries %>% 
    subset(!is.na(subThemes)) %>% 
    replace_na(list(Themes = "")) %>% 
    select(Themes, subThemes, Positive, Uncertain, Negative
           #, Summary
    ) %>% 
    group_by(Themes) %>% 
    dplyr::summarise(subThemes =paste(subThemes, collapse="; "),
                     Positive = sum(Positive),
                     Uncertain = sum(Uncertain),
                     Negative = sum(Negative)) %>% 
    
    gt() %>% 
    # Add title
    tab_header(title = md("**Themes and subThemes**")) %>% 
    # Bold column names
    cols_label(Themes = md("**Themes**"),
               subThemes = md("**subThemes**"),
               Positive = md("**Positive**"),
               Uncertain = md("**Uncertain**"),
               Negative = md("**Negative**"),
               # Summary = md("**Summary**")
    ) %>% 
    # Add banner column
    tab_spanner(label = "Polarity",
                columns = vars(Positive, Uncertain, Negative)) %>% 
    # Add captions
    tab_source_note(source_note = "Data: This Study") %>% 
    # tab_source_note(source_note = "Table by @dm_ferraro for #TidyTuesday") %>% # For some reason a newline wasn't working %>% 
    # Style the totals row
    tab_style(
      style = list(
        cell_fill(color = "#95AFBA"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = vars(Themes, subThemes, Positive, Uncertain, Negative
                       #, Summary
        ),
        rows = Themes == "Total")
    ) %>% 
    ## Alter cell width in summary
    #   cols_width(vars(Summary) ~ pct(50)) %>% 
    # Style the title
    tab_style(
      style = list(
        cell_fill(color = "#95AFBA"),
        cell_text(weight = "bold")
      ),
      locations = cells_title(groups = "title")
    ) %>% 
    # Background color
    tab_options(
      table.background.color = "#F3F6F7"
    ))


# Save
# Had to run webshot::install_phantomjs() first
# gtsave(employed_gt, filename = here("2021-02-23", "employment.png")) # Doesn't save with all of the markdown
gtsave(summaries_gt, filename = here("Figures/by_theme.png"))


summaries_det <- summaries %>% 
  select(Themes, subThemes_, Summary_) %>% 
  subset(!is.na(Summary_)) %>% 
  gt(groupname_col = "Themes") %>% 
  # Add title
  tab_header(title = md("**Summarised Results by Theme and subTheme**")) %>% 
  # Bold column names
  cols_label(#Themes = md("**Themes**"),
    subThemes_ = md("**subThemes**"),
    Summary_ = md("**Summaries**")
  ) %>% 
  # Add captions
  #  tab_source_note(source_note = "Data: This Study") %>% 
  # tab_source_note(source_note = "Table by @dm_ferraro for #TidyTuesday") %>% # For some reason a newline wasn't working %>% 
  # Style the totals row
  tab_style(
    style = list(
      cell_fill(color = "#95AFBA"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(#Themes, 
        subThemes_, Summary_
      ),
      rows = Themes == "Total")
  ) %>% 
  ## Alter cell width in summary
  cols_width(vars(Summary_) ~ pct(80)) %>% 
  # Style the title
  tab_style(
    style = list(
      cell_fill(color = "#95AFBA"),
      cell_text(weight = "bold")
    ),
    locations = cells_title(groups = "title")
  ) %>% 
  # Background color
  tab_options(
    table.background.color = "#F3F6F7"
  ) %>% 
  tab_options(table.width = px(900)) 

# Save
# Had to run webshot::install_phantomjs() first
# gtsave(employed_gt, filename = here("2021-02-23", "employment.png")) # Doesn't save with all of the markdown
gtsave(summaries_det, filename = here("Figures/by_theme_det.png"))

# Load data
by_SDG <- mysheets$By_SDG %>% 
  mutate(SDG_ = SDG) %>% 
  unite(titles, c(SDG,SDG_Text), sep = " - ") %>% 
  mutate(Polarity = factor(Polarity, levels = c("+", "+ / -","-"))) %>% 
  arrange(Polarity)

SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][1]
# Let's make a table with {gt} shall we
(by_SDG_gt <- by_SDG %>% 
    select(-SDG_) %>% 
    gt(groupname_col = "titles") %>% 
    # Add title
    tab_header(title = md("**Pathways to the SDGs**")) %>% 
    # Bold column names
    cols_label(Description = md("**Pathway**"),
               Polarity = md("**Polarity**")) %>% 
    # Add banner column
    # tab_spanner(label = "Polarity",
    #              columns = vars(Positive, Uncertain, Negative)) %>% 
    # Add captions
    tab_source_note(source_note = "Data: This Study") %>% 
    # tab_source_note(source_note = "Table by @dm_ferraro for #TidyTuesday") %>% # For some reason a newline wasn't working %>% 
    # Style the totals row
    tab_style(
      style = list(
        cell_fill(color = "#95AFBA"),
        cell_text(weight = "bolder")
      ),
      locations = cells_body(
        columns = vars(Description, Polarity),
        rows = Description == "Total")
    ) %>% 
    ## Style Row Names
    
    # Style the title
    tab_style(
      style = list(
        cell_fill(color = "#95AFBA"),
        cell_text(weight = "bold", align = "left"                 )
      ),
      locations = cells_title(groups = "title")
    ) %>% 
    ## Center Polarity Column
    tab_style(
      style = cell_text(weight = "bold",
                        align = "center"),
      locations = list(cells_body(columns = vars(Polarity)),
                       cells_body(columns = vars(Polarity),
                                  rows = Description == "Total"))
      
    ) %>% 
    # Background color
    tab_options(
      table.background.color = "#F3F6F7"
    ) %>% 
    ## Alter cell width in summary
    cols_width(vars(Description) ~ pct(85)) %>% 
    tab_options(table.width = px(500)) %>% 
    
    
    ####GROUP HEADINGS##########
  tab_style(
    style = list(
      cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][1]),
      cell_text(style = "italic", weight = "bold",color = "black")
    ),
    locations = cells_row_groups(groups = unique(by_SDG$titles)[1])
  )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][2]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[2])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][3]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[3])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][4]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[4])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][5]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[5])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][6]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[6])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][7]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[7])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][8]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[8])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][9]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[9])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][10]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[10])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][11]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[11])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][12]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[12])
    )%>% 
    tab_style(
      style = list(
        cell_fill(color = SDGpalette[c(2,3,5,6,8,9,10,11,12,13,14,15,17)][13]),
        cell_text(style = "italic", weight = "bold",color = "black")
      ),
      locations = cells_row_groups(groups = unique(by_SDG$titles)[13])
    ))



#gt_preview(by_SDG_gt)
# Save
# Had to run webshot::install_phantomjs() first
# gtsave(employed_gt, filename = here("2021-02-23", "employment.png")) # Doesn't save with all of the markdown
gtsave(by_SDG_gt, filename = here("Figures/by_SDG_raw.png"))


