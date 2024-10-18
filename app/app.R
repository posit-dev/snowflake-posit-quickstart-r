library(shiny)
library(bslib)
library(ggplot2)
library(gt)
library(stringr)
library(dplyr)
library(thematic)
library(scales)

source("connect.R")
thematic_shiny()


metric_choices <-
  c(
    "Age" = "age",
    "Serum Sodium" = "serum_sodium",
    "Serum Creatinine" = "serum_creatinine"
  )


# UI ----
ui <- page_sidebar(
  title =
    div(
      img(src = "heart.png", height = "30px"),
      "Heart Failure Data Dashboard"
    ),
  sidebar =
    sidebar(
      selectizeInput(
        inputId = "metric",
        label = "Select a clinical metric:",
        choices = metric_choices,
        selected = "age"
      )
    ),
  layout_column_wrap(
    card(
      card_header("Clinical Metric Distribution by Survival"),
      plotOutput("metric_plot")
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Summary Statistics"),
        gt_output("summary_gt")
      ),
      card(
        card_header("Key Values"),
        layout_column_wrap(
          value_box(
            "Total Patients",
            value = textOutput("total_patients"),
            theme = "primary",
            showcase = bsicons::bs_icon("person-fill")
          ),
          value_box(
            "Median Age",
            value = textOutput("median_age"),
            theme = "info",
            showcase = bsicons::bs_icon("calendar3-event")
          ),
          value_box(
            "Survival Rate",
            value = textOutput("survival_rate"),
            theme = "warning",
            showcase = bsicons::bs_icon("heart-pulse-fill")
          )
        )
      )
    )
  ),
  theme = bslib::bs_theme(preset = "sandstone")
)

# Server logic ----
server <- function(input, output) {

  # # Reactive summary statistics
  summary_stats <-
    heart_failure |>
    group_by(death_event, diabetes) |>
    summarize(
      across(
        c(age, serum_creatinine, serum_sodium),
        \(x) median(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )


  # Plot output
  output$metric_plot <- renderPlot({

    metric <- sym(input$metric)
    metric_label <- names(metric_choices)[metric_choices == input$metric]

    heart_failure |>
      mutate(
        death_event = as.character(death_event),
        diabetes = as.character(diabetes)
      ) |>
      ggplot(aes(x = death_event, y = !!metric, fill = diabetes)) +
      geom_boxplot() +
      scale_fill_manual(values = c("0" = "#6baed6", "1" = "#f57b3b")) +
      labs(
        title = paste("Distribution of", metric_label, "by Survival Outcome"),
        x = "Survival Outcome (0 = Survived, 1 = Died)",
        y = metric_label,
        fill = "Diabetes"
      )
  })

  # GT table output
  output$summary_gt <- render_gt({
    summary_stats |>
      mutate(
        death_event = case_when(
          death_event == 1 ~ "Died",
          death_event == 0 ~ "Survived"
        ),
        diabetes = case_when(
          diabetes == 1 ~ "Yes",
          diabetes == 0 ~ "No"
        )
      ) |>
      gt(rowname_col = "death_event") |>
      cols_label(
        diabetes = "Diabetes Status",
        age = "Median Age",
        serum_creatinine = "Median Serum Creatinine (mg/dL)",
        serum_sodium = "Median Serum Sodium (mEq/L)"
      ) |>
      tab_header(
        title = "Clinical Metrics by Survival Outcome and Diabetes Status"
      ) |>
      data_color(
        columns = c(serum_creatinine, serum_sodium),
        palette = "Blues"
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups(groups = everything())
      )
  })

  # Value box outputs
  output$total_patients <- renderText({
    tally(heart_failure) |> pull(n)
  })

  output$median_age <- renderText({
    median(heart_failure |> pull(age), na.rm = TRUE)
  })

  output$survival_rate <- renderText({
    death_events <- heart_failure |> pull(death_event)
    rate <- round((1 - mean(death_events, na.rm = TRUE)) * 100, 0)
    glue::glue("{rate}%")
  })
}

# Run the application
shinyApp(ui = ui, server = server)