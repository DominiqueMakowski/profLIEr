#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bayestestR)
library(parameters)
library(lavaan)
library(effectsize)
library(see)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)

load("data.rda")
load("model_dimensions.rda")
load("model_clusters.rda")

names(data) <- stringr::str_remove(names(data), "LIE_")


# Convenience functions
get_scores <- function(input){
  subject <- as.data.frame(input)
  subject <- effectsize::change_scale(subject, to = c(-5, 5), range = c(0, 10))

  cfa <- attributes(cfa_parameters)$model
  scores <- lavPredict(cfa, newdata = rbind(lavInspect(cfa, what = "data"),
                                            subject))
  as.data.frame(t(scores[nrow(scores),]))
}







shinyServer(function(input, output) {


# Distribution plot -------------------------------------------------------


  output$distributions <- renderPlot({
    scores <- get_scores(reactiveValuesToList(input))

    density_data <- data.frame()
    for(i in c("Ability", "Frequency", "Negativity", "Contextuality")){
      dat <- estimate_density(data[[i]])
      val <- scores[[i]]
      dat$fill <- ifelse(dat$x < val, i, "Above")
      dat$Dimension <- i
      density_data <- rbind(density_data, dat)
    }

    density_data %>%
      ggplot(aes(x = x, y = y)) +
      geom_line(data = density_data[density_data$fill == "Above", ], linetype = "dotted", color = "#607D8B") +
      geom_area(aes(fill = fill)) +
      geom_vline(data = data.frame(score = t(scores)[,1], Dimension = names(scores)), aes(xintercept = score)) +
      scale_fill_manual(values = c("Ability" = "#E91E63",
                                   "Frequency" = "#9C27B0",
                                   "Negativity" = "#2196F3",
                                   "Contextuality" = "#FF9800",
                                   "Above" = "white"), guide = FALSE) +
      theme_modern() +
      ylab("Population") +
      theme(axis.title.x = element_blank(),
            strip.placement = "outside") +
      facet_wrap(~Dimension, nrow = 2, scales = "free", strip.position = "bottom")
  })




  output$scores <- renderTable({
    scores <- get_scores(reactiveValuesToList(input))
    scores$Profile <- predict(k, newdata = scores, names = c("Average", "Trickster", "Virtuous"))
    scores
  })

# Radar plot --------------------------------------------------------------




  output$radar <- renderPlot({

    scores <- get_scores(reactiveValuesToList(input))

    profile_data <- data %>%
      pivot_longer(-Profile, names_to = "Dimension", values_to = "Score") %>%
      mutate(Profile = fct_relevel(Profile, "Trickster", "Average", "Virtuous"),
             Dimension = fct_relevel(Dimension, "Ability", "Frequency", "Contextuality", "Negativity")) %>%
      group_by(Profile, Dimension) %>%
      summarise_all(mean, na.rm=TRUE) %>%
      ungroup()

    profile_data <- rbind(profile_data,
                          data.frame(Profile = "Results", Dimension = names(scores), Score = t(scores)[,1])) %>%
      as.data.frame() %>%
      mutate(Profile = fct_relevel(Profile, "Trickster", "Average", "Virtuous", "Results"),
             Dimension = fct_relevel(Dimension, "Ability", "Frequency", "Negativity", "Contextuality")) %>%
      arrange(Dimension)

    profile_data %>%
      ggplot(aes(x = Dimension, y = Score, color = Profile, group = Profile)) +
      geom_line() +
      geom_polygon(fill = NA, size = 2, show.legend = FALSE) +
      scale_color_manual(values = c("Average" = "#D500F9", "Trickster" = "#F50057", "Virtuous" = "#3D5AFE", "Results" = "black")) +
      theme_minimal() +
      xlab("") + ylab("") +
      # theme(axis.ticks.y = element_blank(),
      #       axis.text.y = element_blank()) +
      scale_y_continuous(breaks = c(-2, 0, 2), expand = expand_scale(c(.10, 0))) +
      # scale_y_continuous(limits = c(-5, 5)) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_text(face="bold"),
            panel.grid.major.y = element_line(color="#E0E0E0", linetype="longdash"),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(
              vjust = -0.5,
              # face="bold",
              color="black")) +
      coord_radar(start = -pi/4, clip="off")
  })


})

