# required packages
suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(png))

# set project working directory (EDIT THIS)
setwd("D:/Box Sync/cboyer/service_requests/request_201512020824_extscorecard/infographic/R")

# load slide and text data into R
slide_data <- read.csv(file = "../csv/infographic_data.csv", 
	            stringsAsFactors = FALSE)
text_data <- read.csv(file = "../csv/text_data.csv",
	            stringsAsFactors = FALSE)

# function definitions	
create_slide <- function(data, str1, str2, img1, img2, title) {
	# define paramaters
	str2 <- gsub("\\[v\\]", data$v, str2)
	str2 <- gsub("\\[X\\]", data$X, str2)
	str2 <- gsub("\\[Y\\]", data$Y, str2)
	str2 <- gsub("\\[commune name\\]", data$commune, str2)
	commune <- data$commune

	# convert image to plottable grobs
	grob22 <- rasterGrob(img1, interpolate = FALSE)
	grob31 <- rasterGrob(img2, interpolate = FALSE)

	# convert strings to text grobs
	grob21 <- textGrob(str1, gp = gpar(cex = 2))
	grob32 <- textGrob(str2, gp = gpar(cex = 2))
  tgrob <- textGrob(title, just = "left", gp = gpar(cex = 3, fontface = 2))
  
	# set up layout for the grid
	lo <- grid.layout(3, 2)

	# create indicator graph using ggplot
	plt22 <- create_progress_bar(data, grob22, commune)

	# show the layout
	grid.show.layout(lo)
	grid.newpage()
	pushViewport(viewport(layout = lo))

	# assign grobs to grid layout
	# title
	pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
	print(grid.draw(tgrob), newpage=FALSE)
	popViewport()

	# left upper quad
	pushViewport(viewport(layout.pos.row=2, layout.pos.col = 1))
	print(grid.draw(grob21), newpage=FALSE)
	popViewport()

  # left lower quad
	pushViewport(viewport(layout.pos.row=3, layout.pos.col = 1))
	print(grid.draw(grob31), newpage=FALSE)
	popViewport()

	# right upper quad
	pushViewport(viewport(layout.pos.row=2, layout.pos.col = 2))
	print(plt22, newpage=FALSE)
	popViewport()	

	# right lower quad
	pushViewport(viewport(layout.pos.row=3, layout.pos.col = 2))
	print(grid.draw(grob32), newpage=FALSE)
	popViewport()
}

create_progress_bar <- function(data, grob, title) {
  data$v <- ifelse(is.na(data$v), 0, data$v)
  data$v <- ifelse(data$indicator == "summary", floor(data$v/140*100), data$v)
  data$v <- ifelse(data$indicator == "school_supplies", 
                   ifelse(data$v > 50, 100, floor((data$v/50)*100)), data$v)
  
  # define parameters
  full_colored <- floor(data$v/10)
  p_colored <- data$v/10 - floor(data$v/10)
  p_colored_y <- ifelse(full_colored >= 5, -0.25, 0.25)
  p_colored_x <- ifelse(full_colored >= 5, full_colored - 4, full_colored + 1)
  title <- paste0(title, "\n")
  odd <- c("CEP_admission",
            "school_water_source",
            "assisted_delivery",
            "CSPS_gas",
            "birth_certificate")
    
  cgrob <- grob
  cgrob[["raster"]][cgrob[["raster"]] == "#BFBFBF"] <- 
    ifelse(data$indicator %in% odd,"#2c7fb8", "#e34a33")

  pgrob <- cgrob
  pgrob$name <- 11 + full_colored
  full_size <- ncol(pgrob$raster)
  clip <- floor(full_size*p_colored)
  pgrob$raster <- pgrob$raster[, 1:clip]
  
	# create plot using ggplot
	p <- ggplot(data, aes(x = rep(1:5, 2), y = rep(c(0.25, -0.25), 5))) +
	       geom_blank() +
	       scale_x_continuous(limits = c(0.5, 5.5)) +
	       scale_y_continuous(limits = c(-0.45, 0.45)) +
	       theme_bw() + 
	       ggtitle(title) +
	       theme(axis.title.x = element_blank(),
	             axis.title.y = element_blank(),
	             axis.text.x = element_blank(),
	             axis.text.y = element_blank(),
	             axis.ticks.x = element_blank(),
	             axis.ticks.y = element_blank(),
	             panel.grid.major = element_blank(),
	             panel.grid.minor = element_blank(),
	             plot.title = element_text(face="bold", size = 20)) + 
	       mapply(function(xx, yy, ii) {
	         grob$name <- ii
	         annotation_custom(grob, 
	                    xmin = xx - 0.4, 
	                    xmax = xx + 0.4,
	                    ymin = yy - 0.4,
	                    ymax = yy + 0.4)
	       }, rep(1:5, 2), rep(c(0.25, -0.25), each = 5), 1:10) +
	       mapply(function(xx, yy, ii) {
	         cgrob$name <- ii
	         annotation_custom(cgrob, 
	                      xmin = xx - 0.4, 
	                      xmax = xx + 0.4,
	                      ymin = yy - 0.4,
	                      ymax = yy + 0.4)
	       }, rep(1:5, 2, length.out = full_colored), 
	          rep(c(0.25, -0.25), each = 5, length.out = full_colored), 
	          seq(11, 10 + full_colored, length.out = full_colored)) + 
	       annotation_custom(pgrob, 	                      
	                         xmin = p_colored_x - 0.4, 
	                         xmax = p_colored_x + (0.8*(clip/full_size) - 0.4),
	                         ymin = p_colored_y - 0.4,
	                         ymax = p_colored_y + 0.4)
	          
	return(p)
}

reshape_data <- function(data) {
	# reshape the data to long format
	data_long <- data %>% 
	               gather(ind, value, -id, -commune, -region) %>%
	               separate(ind, c("indicator", "type"), sep = "\\.") %>%
	               spread(type, value)
	return(data_long)
}


get_string <- function(data, fstub, fsfx) {
	# read string
	str <- data[which(data$indicator == fstub), fsfx]
	return(str)
}

get_image <- function(fstub, fsfx) {
	# image string
	fstr <- paste0("../img/", fstub, "_", fsfx, ".png")

	# read images 
	png <- readPNG(fstr)
	return(png)
}


fstub <- c("CEP_admission",
		   "school_supplies",
		   "school_water_source",
		   "school_latrines",
		   "assisted_delivery",
		   "vaccinated_infants",
		   "CSPS_gas",
		   "pop_water_source",
		   "birth_certificate",
		   "summary")



n_ind <- ncol(slide_data)
n_communes <- nrow(slide_data)

# reshape data
data_long <- reshape_data(slide_data)

for (i in 1:140) {
	for (j in 1:10) {

		# get string
		str1 <- get_string(text_data, fstub[j], "left")
		str2 <- get_string(text_data, fstub[j], "right")
		title <- get_string(text_data, fstub[j], "title")

		# get images
		img1 <- get_image(fstub[j], "right_thumb2")
		img2 <- get_image(fstub[j], "left")

		# filter data
		plot_data <- data_long %>% 
		               filter(indicator == fstub[j] & id == i)

		# update stdout
		msg <- paste0("Creating slide of indicator ", j, " for ", plot_data$commune)
		print(msg)

		# prep output png
		fout <- paste0("../slides/", plot_data$commune, "_", sprintf("%02d", j), ".png")
		png(filename = fout, width = 1366, height = 768)
		
		# create slide
		create_slide(plot_data, str1, str2, img1, img2, title)
		
		# save png
    dev.off()
	}
}

