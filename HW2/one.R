
library(readr)
library(dplyr)
library(ggplot2) 

mobile <- read_csv("mobile_data.csv")
mobile %>% group_by(company) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) -> sorted
sorted[1,1]$company
countries <- sorted[1:20 ,]
drawable <- mobile[mobile$company %in% countries$company, ]
p = ggplot(data = drawable , aes(x = company))
p + geom_bar(fill = "purple", color = "black" , alpha = 0.4) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Company Name') + ylab('Count')


plot1 <- ggplot(data = mobile , aes(x = year , y = dim_length))
plot1 + geom_point(color = "blue", size = 2 , alpha = 0.4) + geom_smooth( method="lm", color = "black") + xlab('Year') + ylab('Length')


plot2 <- ggplot(data = mobile , aes(x = year, y = dim_breadth))
plot2 + geom_point(color = "red", size = 2 , alpha = 0.4) + geom_smooth(method = "lm") + xlab('Year') + ylab('Width')


plot3 <- ggplot(data = mobile , aes(x = year, y = dim_thickness))
plot3 + geom_point(color = "red", size = 2 , alpha = 0.4) + geom_smooth(method = "lm") + xlab('Year') + ylab('Thickness')


plot4 <- ggplot(data = mobile , aes(x = year, y = cam_px))
plot4 + geom_point(color = "purple", size = 2 , alpha = 0.4) + geom_smooth(method = "lm") + xlab('Year') + ylab('Camera Quality') + xlim(2000 , 2018)


mobile %>% group_by(sim_no, LTE) %>% 
  summarise(price = mean(price , na.rm = T)) -> stat2
p <- ggplot(data = stat2 , aes(x = sim_no , y = price))
p + geom_bar(aes(fill = LTE),stat =  "identity", alpha = 0.5, position = "fill")

mobile %>% group_by(LTE) %>% 
  summarise(price = mean(price , na.rm = T)) -> stat3
p <- ggplot(data = stat3 , aes(x = LTE, y = price))
p + geom_bar( size = 10 , alpha = 0.5, stat = "identity", fill = "violet")



new_phones <- mobile[which(mobile$year == 2017),]
p <- ggplot(data = new_phones , aes(x = audio_jack , y = dim_thickness))
p + geom_boxplot(fill = "yellow" , alpha = 0.5)

  ppi <- (mobile$px_col * mobile$px_row) / mobile$display_size
  mobile_info <- mobile
  mobile_info$ppi <- ppi
  ppi_plot <- ggplot(data = mobile_info , aes(x = ppi))
  ppi_plot + geom_histogram(fill = "pink" , color = "black") + xlab('PPI') + ylab('Count') + ggtitle("PPI histogram")
  sort_ppi <- mobile_info[order(mobile_info$ppi , decreasing = T) , ]
  max_ppi <- sort_ppi[1,]
  mobile_info %>% group_by(year) %>% 
    summarise(ppi = mean(ppi , na.rm = T)) -> ppi_stat
  ppi_plot <- ggplot(data = ppi_stat , aes(x = year,  y = ppi))
  ppi_plot + geom_point(color = "blue" , size = 4 , alpha = 0.5) + xlab('Year') + ylab('PPI') + ggtitle("PPI per year") + geom_line(color = "red")+ xlim(1999 , 2017)


    mobile_info$gooshkoob = mobile$weight / (mobile$dim_breadth * mobile$dim_length * mobile$dim_thickness) * 1000 / mobile$price
    sorted_gooshkoob = mobile_info[order(mobile_info$gooshkoob, decreasing = TRUE), ] 
    sorted_gooshkoob = sorted_gooshkoob[c(1:10),]
    gooshkoob_plot <- ggplot(data = sorted_gooshkoob , aes(x = device , y = gooshkoob))
    gooshkoob_plot + geom_point(aes(color = company) , size = 4 , alpha = 0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  water_density <- 1
  mobile_info$density <- mobile_info$weight / (mobile_info$dim_length * mobile_info$dim_breadth * mobile_info$dim_thickness) * 1000
  mobile_info$less_dense <- mobile_info$density < 1
  amount = data.frame(is_less = mobile_info$less_dense[!is.na(mobile_info$less_dense)])
  density_plot <- ggplot(data = amount , aes(x = is_less))
  density_plot + geom_bar(aes(fill = is_less)) + xlab('Density comparisson to water') + ylab('Count')
  denstiy_plot2 <- ggplot(data = mobile_info , aes(x = density))
  denstiy_plot2 + geom_histogram(fill = "orange" ,alpha = 0.7) + xlim(0 , 4)
  print(mobile_info[which(mobile_info$density < 1),]$device[1:10])
  
  battery_weight <- ggplot(data = mobile , aes(x = battery_mah, y = weight))
  battery_weight + geom_point(color = "violet") + geom_smooth(method = "lm")
  coef = coef(lm(weight ~ battery_mah , data = mobile)) 
  print(coef[1])
  print(coef[2])

samsung <- mobile[which(mobile$price > 500),]
samsung <- samsung[which(samsung$company == "Samsung"),]
samsung_plot <- ggplot(data = samsung , aes(x = year, y = price))
samsung_plot + geom_point(aes(color = device))+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_smooth(method = "lm")

    hand_width = 70
    hand_length = 150
    TH_width <- 20
    TH_length <- 30
    mobile_info$hand_friendly <- (mobile_info$dim_breadth < hand_width + TH_width & mobile_info$dim_breadth > hand_width - TH_width & mobile_info$dim_length < hand_length + TH_length & mobile_info$dim_length > hand_length - TH_length)
    hand_friendly_plot <- ggplot(data = mobile_info , aes(x = dim_breadth , y = dim_length))
  
    new_phones <- mobile[which(mobile$year == 2017),]
    new_phones <- new_phones[which(new_phones$price <= 3000) ,]
    new_phones <- new_phones[which(new_phones$weight <= 250),]
    new_phones %>% group_by(company) %>% 
      summarise(weigth = mean(weight , na.rm = T), price = mean(price, na.rm = T)) -> new_phones_weigth
    company_plot <- ggplot(data = new_phones_weigth , aes(x = company , y = weigth))
    

      
 
    company_plot2 <- ggplot(data = new_phones_weigth , aes(x = company  , y = price))
    company_plot2 + geom_histogram(aes(fill = weigth), stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + coord_flip()

    mobile_info$LTE = mobile$LTE
    
    mobile_info$LTE_count <- ifelse(mobile$LTE=="Yes", 1, 0)
    mobile_info$audio_jack_count <- ifelse(mobile$audio_jack == "Yes", 1, 0)
    mobile_info$wlan_count <- ifelse(mobile$wlan == "Yes", 1, 0)
    mobile_info$gps_count <- ifelse(mobile$gps == "Yes" , 1,0)
    mobile_info$features = mobile_info$LTE_count+mobile_info$audio_jack_count+mobile_info$wlan_count+mobile_info$gps_count
    
    mobile_info %>% group_by(company) %>% 
      summarise(battery = mean(battery_mah , na.rm = TRUE), point =  mean(features , na.rm = T), price = mean(price , na.rm = T)) -> company_battery
    
    
    
    company_battery_plot <- ggplot(data = company_battery , aes(x = point , y = battery))
    company_battery_plot + geom_point(color = "orange" , alpha = 0.5) + geom_smooth(method = "lm")
 
    
    