library(aqp)
library(soilDB)
library(magick)
library(cropcircles)
library(ggplot2)
library(ggforce)
# library(extrafont)
#
# loadfonts()

# Define function to add images to ggplot

add_image <- function(img, x, y, height){

  if(missing(height)){
    height <- img |>
      image_info() |>
      pull(height)
  }

  aspect_ratio <-
    img |>
    image_info() |>
    with({
      width/height
    })

  width <- height*aspect_ratio

  annotation_raster(img,
                    xmin = x - width/2,
                    xmax = x + width/2,
                    ymin = y - height/2,
                    ymax = y + height/2)

}

# fetch soil profile for Port Silt Loam

port <- fetchOSD("port")

# Create soil profile plot

{

sol_width <- 1000
sol_height <- 1000

soil_plot <-
  image_graph(width = sol_width,
              height = sol_height,
              res = 300,
              bg = "transparent")

plotSPC(port, width = 0.1,
        depth.axis = FALSE,
        print.id = FALSE,
        cex.names = 0.5)

dev.off()

soil <-
  soil_plot |>
  image_trim()

}

{
mod_plot <-
  image_graph(width = sol_width,
              height = sol_height,
              res = 300,
              bg = "transparent")

sol_prm <-
  # expression(theta[s]*","*theta[fc]*","*theta[r]*"...")
  expression(theta[s]*","*theta[fc]*"...")

print(
  soil |>
  image_info() |>
  with({
    soil |>
      image_crop(geometry_area(width = width*.75,
                               height = height))
  }) |>
  image_canny() |>
  image_negate() |>
  image_charcoal() |>
  image_blur() |>
  image_threshold(type = "black", threshold = "50%") |>
  image_threshold(type = "white", threshold = "50%") |>
  image_fill("#e0cdb5") |>
  image_trim() |>
  image_ggplot()+
  annotate("text", x = 48, y = 360, size = 4.5, label = sol_prm)+
  annotate("text", x = 48, y = 290, size = 4.5, label = sol_prm)+
  annotate("text", x = 48, y = 200, size = 4.5, label = sol_prm)+
  annotate("text", x = 48, y = 90, size = 4.5, label = sol_prm)
)

dev.off()

mod_soil <-
  mod_plot |>

  image_trim()
}

{
soil_db <-
    image_graph(width = 352*2,
                height = 512*2,
                bg = "transparent")

el_df <-
  data.frame(x0 = 352/2,
             y0 = seq(352/5, 512 - 352/5, length.out = 4),
             a = 352/2,
             b = 352/5,
             angle = 0)
print(
el_df |>
  ggplot(aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle))+
  geom_ellipse(data = el_df[1,], linewidth = 10, fill = "gray")+
  with(el_df, annotate("rect", xmin = 0, xmax = 352,
                       ymin = y0[1], ymax = y0[2],
                       fill = "gray")) +
  geom_ellipse(data = el_df[2,], linewidth = 10, fill = "gray")+
  with(el_df, annotate("rect", xmin = 0, xmax = 352,
                       ymin = y0[2], ymax = y0[3],
                       fill = "gray")) +
  geom_ellipse(data = el_df[3,], linewidth = 10, fill = "gray")+
  with(el_df, annotate("rect", xmin = 0, xmax = 352,
                       ymin = y0[3], ymax = y0[4],
                       fill = "gray")) +
  geom_ellipse(data = el_df[4,], linewidth = 10, fill = "gray")+
  annotate("segment",
           x = 0, y = 512 - 352/5,
           xend = 0, yend = 352/5,
           linewidth = 10)+
  annotate("segment",
           x = 352, y = 512 - 352/5,
           xend = 352, yend = 352/5,
           linewidth = 10)+
  xlim(c(0, 352))+
  ylim(c(0, 512))+
  coord_fixed()+
  theme_void()
)

dev.off()

soil_db_icon |>
  image_ggplot()
# soil_db_icon <-
#   image_read_svg("sticker/Database.svg") |>
#   image_background("gray") |>
#   image_trim()
#
# soil_db <-
  # soil_db_icon |>
  # image_info()
#   with({
#     soil_db_icon |>
#     image_fill("transparent",
#                point = geometry_point(1, 1)) |>
#     image_fill("transparent",
#                point = geometry_point(1, height-1)) |>
#     image_fill("transparent",
#                point = geometry_point(width-1, 1)) |>
#     image_fill("transparent",
#                point = geometry_point(width-1, height-1))
#   })
}

{
csmsoil_sticker <-
  image_graph(width = sol_width,
              height = sol_height,
              res = 300,
              bg = "#e0cdb5")

print(
  ggplot()+
    xlim(c(-1, 1))+
    ylim(c(-1, 1))+
    annotate("curve",
             x = -0.59,
             y = 0.15,
             xend = -0.15,
             yend = 0.05,
             linewidth = 1,
             curvature = 0.2,
             angle = 45,
             arrow = arrow(length = unit(0.05, "npc")))+
    annotate("curve",
             x = -0.59,
             y = -0.20,
             xend = -0.15,
             yend = -0.10,
             linewidth = 1,
             curvature = -0.2,
             angle = 45,
             arrow = arrow(length = unit(0.05, "npc")))+
    add_image(mod_soil, x = 0.75, y = -0.05, height = 1.1) +
    annotate("curve",
             x = 0.05,
             y = 0.24,
             xend = 0.64,
             yend = 0.4,
             linewidth = 1,
             curvature = 0.4,
             arrow = arrow(length = unit(0.05, "npc")))+
    annotate("curve",
             x = 0.05,
             y = 0.17,
             xend = 0.64,
             yend = 0.22,
             linewidth = 1,
             curvature = 0.4,
             arrow = arrow(length = unit(0.05, "npc")))+
    annotate("curve",
             x = 0.05,
             y = -0.11,
             xend = 0.64,
             yend = -0.06,
             linewidth = 1,
             curvature = 0.4,
             arrow = arrow(length = unit(0.05, "npc")))+
    annotate("curve",
             x = 0.05,
             y = -0.4,
             xend = 0.64,
             yend = -0.35,
             linewidth = 1,
             curvature = 0.4,
             arrow = arrow(length = unit(0.05, "npc")))+
    annotate("label", x = -0.4, y = -0.15,
             angle = -90,
             label = "Data transfer functions",
             fontface = "bold",
             size = 3.5,
             alpha = 0.95,
             fill = "white") +
    annotate("text", x = 0, y = 0.65,
             label = "csmsoil",
             size = 13,
             family = "Courier",
             fontface = "bold") +
    annotate("label", x = 0.4, y = -0.15,
             angle = -90,
             label = "Pedotransfer functions",
             fontface = "bold",
             size = 3.5,
             alpha = 0.95,
             fill = "white") +
    add_image(soil_db, x = -0.75, y = 0.25, height = 0.5) +
    add_image(soil_db, x = -0.75, y = -0.35, height = 0.5) +
    add_image(soil, x = 0.05, y = -0.25, height = 1.2) +
    theme_void()
)

dev.off()

tmp_stickr <- tempfile(fileext = ".png")


csmsoil_sticker |>
  image_write(tmp_stickr)

crop_hex(tmp_stickr,
         to = "sticker/csmsoil_sticker.png",
         border_size = 12,
         border_colour = "#ad8b60")

}

