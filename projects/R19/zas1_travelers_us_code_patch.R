# Map of NIE-ZAS-1 - # 3 Panels 

# First Expotations from Nigeria to surrounding countries (Lake Chad Basin Era)
# Second Phase into AFRO (Explosive AFRO)
# Third Phase Into Europe (Boarder exportations into EURO )


# Load in Packages 
source("./reference/obx_packages_01.R", local = T)
library(ggarrow)
# Load in Data 
map_ref<- sirfunctions::edav_io(io = "read", file_loc = "Data/orpg/mapref.table.rds")

# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}
date_end <- floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)
e_w <- paste0("W",as.character(epiweek(date_end)),",", sub="")
e_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")

# Set up Output 
# Output 
datt_task_id <- "R19"

sub_folder <- "3. Figures"


# Figure Type 
# "2b1, "3p_w"
f_type <- "3p_w"
# Output options: 

# 2b1 option 
if (f_type == "2b1"){
fig_name <- paste("zas1_traverlers_2b1_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path <- file.path(tempdir(), fig_name)

print(fig_name)

temp_path <- file.path(tempdir(), fig_name)
}else if (f_type == "3p_w"){
  fig_name <- paste("zas1_traverlers_3p_w_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
  temp_path <- file.path(tempdir(), fig_name)
  print(fig_name)
  
}else{
  print("no file name created")
}

sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")

# Do you want to upload to sharepoint 
uploadtosp <- "yes"

## Load Sharepoint Site & Writing Functions 

get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)




zas1 <- raw.data$pos %>% 
           filter(measurement == "cVDPV 2" & 
                  whoregion == "EURO" &
                  dateonset >= d3 & 
                  dateonset <= d4)

zas1 <- zas1 |> dplyr::filter(!place.admin.0 == "ISRAEL")

# #Time Frames
# 
# 
# d1 <- as_date("2020-01-01")
# d2 <- as_date("2021-12-31") 
# d3 <- as_date("2024-12-31") 
# d4 <- as_date("2025-12-31")


# AFRO only map
all<- raw.data$global.ctry %>% filter(WHO_REGION %in% c("AFRO" , "EMRO" , "EURO", "AMRO"))

afro2<- raw.data$global.ctry %>% filter(WHO_REGION %in% c("AFRO" , "EMRO" , "EURO"))
# 
# afro <- raw.data$global.ctry %>% filter(WHO_REGION == "AFRO" |
#                                         ADM0_NAME %in% c("EGYPT", "SOMALIA", "ETHIOPIA",
#                                                          "LIBYA", "SUDAN", "DJIBOUTI",
#                                                          "TUNISIA", "ERITREA", "MOROCCO"))
prov <- raw.data$global.prov %>% filter(ADM0_NAME %in% c("UNITED STATES OF AMERICA", "CANADA"))

prov2 <-raw.data$global.prov %>% filter(ADM0_NAME %in% c("UNITED STATES OF AMERICA")) 

# # write_xlsx(prov2, path="USprovs.xlsx")
# 
# zas2021 <- zas1 %>% 
#              filter(dateonset >= d1 & 
#                     dateonset < d2)
# 
# zas2021_ctry <- zas2021 %>% 
#                     group_by(place.admin.0) %>%
#                     count() %>%
#                     mutate(n=1)
# 
# # Merge with country file 
# zas_shape <- left_join(afro, zas2021_ctry, by = c("ADM0_NAME"="place.admin.0")) %>%
#              filter(is.na(n)==F) 
# 
# zas2021_cases <- zas2021 %>% 
#   filter(source == "AFP")
# 
# # Create maps 
# nh_data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/traveler_outbound.rds")
# 
# nh1 <- nh_data %>%
#   filter(year == 2023) %>%
#   mutate(ctry = str_to_upper(ctry),
#          ctry2 = case_when(
#            ctry == "IVORY COAST" ~  "COTE D IVOIRE",
#            ctry == "UNITED KINGDOM" ~ "THE UNITED KINGDOM",
#            TRUE ~ ctry))
# 
# zas_shape2 <- left_join(zas_shape,nh1, by = c("ADM0_NAME"="ctry2"))  
# zas_shape2 <- zas_shape2 %>% 
#   mutate(
#     count_cat = case_when(
#       count < 2000 ~ "<2k",
#       count >= 2000 & count < 10000 ~ "2-10k",
#       count >= 10000 & count < 100000 ~ "10-100k",
#       count >= 100000 & count < 1000000 ~ "100k-1m",
#       count >= 1000000 & count < 3000000 ~ "1-3m",
#       count >= 3000000  ~ ">3m"),
#     count_cat = factor(count_cat, levels = c("<2k", "2-10k","10-100k","100k-1m", "1-3m", ">3m")))
# 
# 
# map_20 <-
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = afro2,  fill =  "#FFFFFF") +
#   # ggplot2::geom_sf(data = afro2, color = "grey20", lwd = 0.6, fill = "#FFFFFF") +
#   ggplot2::geom_sf(data = zas_shape2, aes(fill=count_cat), color = "grey20", lwd = 0.4) +
#   # ggplot2::coord_sf(
#   #   xlim = country_bbox[c("xmin", "xmax")],
#   #   ylim = country_bbox[c("ymin", "ymax")]
#   # ) +
#   ggplot2::theme_bw() +
#   scale_fill_brewer(name= "Travelers\n to US:\n(2023)" , palette = "Reds", drop = FALSE) +
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "#E3EBFF"),
#         legend.title = element_text(face="bold", size = 12),
#         axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
#   ggtitle("2020-2022")  +
#   coord_sf(xlim = c(-30, 55), ylim = c(-10, 40), expand = FALSE) 
# 
# print(map_20)
# # top <- plot_grid(figure.list[[1]], figure.list[[2]], ncol = 2)
# # bottom <- plot_grid(figure.list[[3]], ncol = 1)
# # plot_grid(top, bottom,
# #           ncol=1, rel_heights=c(1,1))
# 
# 
# # 2022-2024
# zas2023 <- zas1 %>% 
#   filter(dateonset > d2 & 
#            dateonset <= d3)
# 
# zas2023_ctry <- zas2023 %>% 
#   group_by(place.admin.0) %>%
#   count() %>%
#   mutate(n_23=1)
# 
# # Merge with country file 
# zas_shape23 <- left_join(afro2, zas2023_ctry, by = c("ADM0_NAME"="place.admin.0")) %>%
#   filter(is.na(n_23)==F) 
# 
# zas2023_cases <- zas2023 %>% 
#   filter(source == "AFP")
# 
# zas_shape23 <- left_join(zas_shape23,nh1, by = c("ADM0_NAME"="ctry2"))  
# zas_shape23 <- zas_shape23 %>% 
#   mutate(
#     count_cat = case_when(
#       count < 2000 ~ "<2k",
#       count >= 2000 & count < 10000 ~ "2-10k",
#       count >= 10000 & count < 100000 ~ "10-100k",
#       count >= 100000 & count < 1000000 ~ "100k-1m",
#       count >= 1000000 & count < 3000000 ~ "1-3m",
#       count >= 3000000  ~ ">3m"),
#     count_cat = factor(count_cat, levels = c("<2k", "2-10k","10-100k","100k-1m", "1-3m", ">3m")))
# 
# map_23 <-
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = afro2, fill =  "#FFFFFF") +
#   ggplot2::geom_sf(data = zas_shape23, aes(fill=count_cat), color = "grey20", lwd = 0.4) +
#   ggplot2::theme_bw() +
#   scale_fill_brewer(name= "Travelers\n to US:\n(2023)" , palette = "Reds", drop = FALSE) +
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "#E3EBFF"),
#         legend.title = element_text(face="bold", size = 12),
#         axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
#   ggtitle("2022-2024")  +
#   coord_sf(xlim = c(-30, 55), ylim = c(-10, 40), expand = FALSE) 
# 
#  print(map_23)
# 
# # Now big map with Europe 
# # 2022-2024
# zas2025 <- zas1 %>% 
#   filter(dateonset > d3 & 
#            dateonset <= d4)
# 
# zas2025_ctry <- zas2025 %>% 
#   group_by(place.admin.0) %>%
#   count() %>%
#   mutate(n_25=1)
# 
# # Merge with country file 
# zas_shape25 <- left_join(afro2, zas2025_ctry, by = c("ADM0_NAME"="place.admin.0")) %>%
#   filter(is.na(n_25)==F) 
# 
# zas2025_cases <- zas2025 %>% 
#   filter(source == "AFP")
# 
# # Create maps 
# nh_data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/traveler_outbound.rds")
# 
# # Sub Code <- 
# zas25_sub <-  zas_shape25 %>% 
#   select(ADM0_NAME, Shape, CENTER_LON, CENTER_LAT)
# 
# nh1 <- nh_data %>%
#   filter(year == 2023) %>%
#   mutate(ctry = str_to_upper(ctry),
#          ctry2 = case_when(
#            ctry == "IVORY COAST" ~  "COTE D IVOIRE",
#            ctry == "UNITED KINGDOM" ~ "THE UNITED KINGDOM",
#            TRUE ~ ctry))
# 
# zas2_sub <- left_join(zas25_sub, nh1, by = c("ADM0_NAME"= "ctry2"))
# 
# # sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/zas_points.rds", obj = zasdata_points)
# 
# zasdata_points <- sirfunctions::edav_io(io = "read",  default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/zas_points.rds")
# # zasdata_test1 <- read_excel("zasdata_test1.xlsx", 
# #                             col_types = c("text", "text", "numeric", 
# #                                           "numeric", "text", "numeric", "numeric"))
# zas3 <- right_join(zas25_sub, zasdata_points, by = c("ADM0_NAME"))
# zas3 <- left_join(zas3, nh1, by = c("ADM0_NAME"="ctry2")) 
# 
# zas3_a <- zas3 %>% mutate(
#   count_cat = case_when(
#     count < 2000 ~ "<2k",
#     count >= 2000 & count < 10000 ~ "2-10k",
#     count >= 10000 & count < 100000 ~ "10-100k",
#     count >= 100000 & count < 1000000 ~ "100k-1m",
#     count >= 1000000 & count < 3000000 ~ "1-3m",
#     count >= 3000000  ~ ">3m"),
#   count_cat = factor(count_cat, levels = c("<2k", "2-10k","10-100k","100k-1m", "1-3m", ">3m")))



zasdata_points <- sirfunctions::edav_io(io = "read",  default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/zas_points.rds")


zas2025_ctry <- zas1 %>%
  group_by(place.admin.0) %>%
  count() %>%
  mutate(n_25=1)

# Merge with country file
zas_shape25 <- left_join(afro2, zas2025_ctry, by = c("ADM0_NAME"="place.admin.0")) %>%
  filter(is.na(n_25)==F & 
           ENDDATE == "9999-12-31")


zas3_a <- zasdata_points |> dplyr::filter(ADM0_NAME %in% c("THE UNITED KINGDOM", "POLAND", "GERMANY")) 

zas3_a <- dplyr::left_join(zas_shape25, zas3_a, by = "ADM0_NAME")

map_25_n <-
  ggplot2::ggplot() +
  # ggplot2::geom_sf(data = all) +
  ggplot2::geom_sf(data = all,  fill = "#FFFFFF") +
  # ggplot2::geom_sf(data = prov2,  lwd = 0.1, fill = NA, show.legend = FALSE) +
  ggplot2::geom_sf(data = zas_shape25, fill = "#FE0100", color = "grey10", lwd = 0.8) +

  geom_arrow_curve(data=zas3_a, aes(x = CENTER_LON, y = CENTER_LAT,
                                                            xend = US_CITY_LON, yend = US_CITY_LAT),
                   mid_place = 1, col = "#909090", size = 1, curvature = .3,
                   lineend = "square", linetype = "dashed") +
  ggplot2::theme_bw() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.title = element_text(face="bold", size = 12),
        legend.text = element_text(size = 12),
        
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
  guides(fill = guide_legend(nrow = 1))+
  coord_sf(xlim = c(-127, 38), ylim = c(25, 75), expand = FALSE)  

print(map_25_n)

# map_25_l <-
#   map_25_n <-
#   ggplot2::ggplot() +
#   # ggplot2::geom_sf(data = all) +
#   ggplot2::geom_sf(data = all,  fill = "#FFFFFF") +
#   # ggplot2::geom_sf(data = prov2,  lwd = 0.1, fill = NA, show.legend = FALSE) +
#   ggplot2::geom_sf(data = zas3_a , aes(fill = count_cat), color = "grey20", lwd = 0.4) +
#   geom_arrow_curve(data=zas3_a %>% filter(count>10000), aes(x = CENTER_LON, y = CENTER_LAT, 
#                                                             xend = US_CITY_LON, yend = US_CITY_LAT), 
#                    mid_place = 1, col = "black", size = 1, curvature = .3,
#                    lineend = "square") + 
#   ggplot2::theme_bw() +
#   scale_fill_brewer(name= "Travelers\n to US:\n(2023)" , palette = "Reds") +
#   theme(legend.position = "bottom",
#         panel.background = element_rect(fill = "#E3EBFF"),
#         legend.title = element_text(face="bold", size = 12),
#         legend.text = element_text(size = 12),
#         
#         axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
#   ggtitle("2024-Present")  +
#   guides(fill = guide_legend(nrow = 1))+
#   coord_sf(xlim = c(-127, 38), ylim = c(-20, 75), expand = FALSE)  


ggsave(filename = paste("./output/tiger/gid_allhands_240626.png"), height = 4, width = 6, unit = "in", dpi = 300)

# opt1 <- map_20 | map_23
# # print(opt1)
# opt_a <- opt1 / map_25_l +
#   plot_layout(heights = unit(c(0.9, 3.5), c("in")))
# 
# print(opt_a)

# ggsave(filename = paste("./output/tiger/zas1_draft_v3.png"), height = 8, width = 8, unit = "in", dpi = 300)


# Make new figure 
titnew = "Countries with NIE-ZAS-1 Detections"

if (f_type == "2b1"){
top <- plot_grid(map_20, map_23, ncol = 2)
bottom <- plot_grid(map_25_l, ncol = 1)
all_2 <- plot_grid(top, bottom,
           ncol=1, rel_heights=c(1,2))

print(all_2)
}else if (f_type == "3p_w"){
  legend <- get_legend(map_25_l)
  
  # Wide Panel
  panel_wide <- (map_20 + map_23 + map_25_n)/ legend +
    plot_layout(heights = unit(c(2, 1), c("in"))) 
  
  print(panel_wide)
}else{
  print("No image output")
}
# # long_p <- plot_grid(map_20, map_23, map_25_n, nrow = 3)
# legend <- get_legend(map_25_l)
# 
# # Add the legend to the combined plot
# lp_all <- plot_grid(all_2, legend, ncol = 1,rel_heights = c(1, 0.1))

# # Display the final plot
# print(lp_all)
# ggsave(filename = paste("./output/tiger/zas1_draft_2by1.png"), height = 8, width = 8, unit = "in", dpi = 300)
# 

# # Wide Panel 
# 
# long_w <- plot_grid(map_20, map_23, map_25_n, labels = titnew, ncol = 3)
# lp_wide <- plot_grid(long_w, legend, ncol = 1, align = "h", rel_heights = c(1,.01))
# print(lp_wide)


# 
# ggsave(filename = paste("./output/tiger/zas1_draft_wide.png"), height = 8, width = 8, unit = "in", dpi = 300)


# Save Locally
if(uploadtosp == "yes"){

  if(f_type == "2b1"){
    
    ggsave(plot = all_2, filename = temp_path, height = 8, width = 8, unit = "in", dpi = 300)
    cli::cli_alert("Beep Beep - Image Saved to Temp Folder")
    
  }else if(f_type == "3p_w"){
    
    ggsave(plot = panel_wide, filename = temp_path, height = 4, width = 12, unit = "in", dpi = 300)
    cli::cli_alert("Beep Beep - Image Saved to Temp Folder")
    
  }
  upload_to_sharepoint(
    file_to_upload = temp_path,  # Writes to temp directory 
    sharepoint_file_loc = sp_path,
    site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
    drive = "Documents")
  
  cli::cli_alert("Beep Beep - Image uploaded to Sharepoint")
  
}else{
  cli::cli_alert("No output saved")
}
