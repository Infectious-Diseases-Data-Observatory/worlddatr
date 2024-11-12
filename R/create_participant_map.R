#' Create a cloropleth map of the number of participants in a dataset
#'
#' @param DATA_DM Data frame with includes the column "COUNTRY", listing the 3
#'   letter ISO country code where the participant is located in. The
#'   Demographics (DM) domain in SDTM and IDDO-SDTM for example.
#' @param include_ATA Boolean. Should Antarctica be included on the map? Default
#'   is `FALSE`
#'
#' @return A cloropleth map displaying the number of participants in each
#'   country
#'
#' @export
#'
#' @author Rhys Peploe
#'
#' @examples
#' library(worlddatr)
#' set.seed(123) # for reproducibility
#'
#' countries <- sample(world_income$alpha_3_code, 100, replace = FALSE)
#'
#' probabilities <- runif(length(countries))
#' probabilities <- probabilities / sum(probabilities)
#'
#' country_data <- data.frame(COUNTRY = sample(countries, 10000, replace = TRUE, prob = probabilities))
#'
#' create_participant_map(DATA_DM     = country_data,
#'                        include_ATA = FALSE)
#'
create_participant_map <- function(DATA_DM, include_ATA = FALSE){
  GROUP_DM = DATA_DM %>%
    group_by(COUNTRY) %>%
    summarise(n_participants = n()) %>%
    ungroup()

  sum_participants = sum(GROUP_DM$n_participants)

  world_map_DM = GROUP_DM %>%
    right_join(world_map,
               by = c("COUNTRY" = "alpha_3_code"))

  if(include_ATA == FALSE){
    world_map_DM = world_map_DM %>%
      filter(COUNTRY != "ATA")
  }

  ggplot(world_map_DM, aes(x = long, y = lat, group = group)) +
    geom_polygon(colour = "black", fill = "#fdf3f4", linewidth = 0.01) +
    geom_polygon(data = world_map_DM %>%
                   filter(!is.na(n_participants)),
                 mapping = aes(x = long, y = lat, group = group, fill = n_participants),
                 alpha = 0.85, colour = "black", linewidth = 0.01) +
    scale_fill_gradient(high ="#14B1E7", low = "#ccecf9",
                        breaks = pretty_breaks()) +
    theme(panel.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid = element_blank(),
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.key.height = unit(1, 'cm')) +
    labs(fill = "Number of Participants") +
    annotate("text", x = 165, y = -60, label = str_c("n = ", sum_participants))
}
