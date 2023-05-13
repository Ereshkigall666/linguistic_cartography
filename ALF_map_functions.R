# Timothée Premat | 10 octobre 2020, based on original script by:
# Mathieu Avanzi | 22 avril 2020

# packages that need to be loaded before running the code
library("maps")
library("dplyr")
library("ggplot2")
library("ggsn")
library("grid")
library("plotly")

create_map <- function(source_file_path, dest_file_name, title = "", author = "", alf_ref = "", subtitle = "", web = T, caption = list()) {
    # load data
    dataALF <- read.csv2(source_file_path, header = T, quote = "", dec = ".")

    # get map
    world <- map_data("world")

    # plot it
    g <- ggplot() +
        geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#3c3b3b", colour = "lightgrey", linewidth = 0.4) +
        coord_quickmap(xlim = c(-4.5, 9), ylim = c(41.8, 51)) +
        geom_point(data = dataALF, mapping = aes(x = Long, y = Lat, colour = Value, shape = Value, text = sprintf(
            "Type : %s\nN° pt : %s\nCommune : %s\nDpt/Région : %s", Value, name, description, Nom_dept
        )), size = 3) +
        scale_shape_manual(values = c(17, 15, 43), "", limits = c("var1", "var2", "Other")) +
        scale_colour_manual(values = c("darkgreen", "red", "#b46cee"), limits = c("var1", "var2", "Other")) +
        theme(
            panel.background = element_rect(fill = "#8eb2c8", colour = "transparent"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.justification = c(0, 0),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = c(0.02, 0.27),
            legend.spacing = unit(1, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1.2, "lines"),
            legend.text.align = 0,
            legend.title.align = 0,
            legend.text = element_text(size = 12, color = "black"),
            legend.key = element_rect(fill = "transparent", color = "transparent"),
            legend.title = element_text(size = 1, color = "transparent", face = "bold"),
            legend.background = element_rect(fill = "transparent", colour = "transparent")
        ) +
        scalebar(
            dist = 100, dist_unit = "km", transform = TRUE, model = "WGS84",
            x.min = 3.95, x.max = 6.65, y.min = 41.8, y.max = 43.3,
            st.size = 3, height = 0.05,
            st.dist = 0.1, border.size = 0.1,
            box.fill = c("white", "gray20"),
            st.color = "black"
        )
    if (web) {
        g
        gweb <- ggplotly(g, width = 750, height = 750, tooltip = "text")
        # Add fix coordinates
        gweb <- gweb %>% layout(
            xaxis = list(range = c(-4.5, 9)),
            yaxis = list(range = c(41.74, 51.15)),
            legend = list(x = 0.02, y = 0.2)
        )

        # Add annotations
        gweb <- gweb %>% add_annotations(
            x = -4.5,
            y = 51,
            text = paste("<i>", title, "<i>"),
            showarrow = F,
            font = list(size = 20, color = "white"),
            xanchor = "left"
        )

        gweb <- gweb %>% add_annotations(
            x = -4.5,
            y = 50.725,
            text = paste("<i>", subtitle, "<i>"),
            showarrow = F,
            font = list(size = 15, color = "white"),
            xanchor = "left"
        )

        gweb <- gweb %>% add_annotations(
            x = -4.5,
            y = 50.475,
            text = paste("<i>", paste("ALF c.", alf_ref, title, sep = " "), "<i>"),
            showarrow = F,
            font = list(size = 12, color = "white"),
            xanchor = "left"
        )

        gweb <- gweb %>% add_annotations(
            x = -4.5,
            y = 50.28,
            text = paste("© ", author),
            showarrow = F,
            font = list(size = 10, color = "white"),
            xanchor = "left"
        )

        gweb <- gweb %>% add_annotations(
            x = 5.5,
            y = 42.5,
            text = "based on:TutoMap (4)\nhttps://phonodiachro.hypotheses.org/410",
            showarrow = F,
            font = list(size = 10, color = "white"),
            xanchor = "center"
        )

        # print the plot
        gweb

        # Save the plot
        htmlwidgets::saveWidget(as_widget(gweb), paste(title, "-", subtitle, "-interactive-map", ".html", sep = ""))

        return(gweb)
    }
    g <- g + annotate("text", x = -4, y = 51, label = title, size = 6, color = "blue", hjust = 0) +
        annotate("text", x = -4, y = 50.75, label = subtitle, size = 5, color = "blue", hjust = 0) +
        annotate("text", x = -4, y = 50.515, label = paste("ALF c.", alf_ref, title, sep = " "), size = 4, color = "blue", hjust = 0) +
        annotate("text", x = -4, y = 50.35, label = paste("© ", author), size = 3, color = "blue", hjust = 0) +
        annotate("text", x = 4, y = 42.5, label = "based on: TutoMap (2),\nUrl : https://phonodiachro.hypotheses.org/?p=145", size = 3, color = "black", hjust = 0)
    g
    ggsave(g, filename = dest_file_name, width = 25, height = 25, units = "cm", scale = 1, dpi = "retina")
    return(g)
}

generate_ALF_map_from_terminal <- function() {}
