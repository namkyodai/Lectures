# Vẽ Đồ Thị!
# Cơ Bản
ggplot(thedata_BC_Can, aes(x=ref_date, y=value, group=geo)) + 
  geom_line()
# các bạn đã thấy chưa, R cung cấp cho ta 1 dạng đồ thị cơ bản và cũng khá là bắt mắt. Và chúng ta có thể thêm thắt và làm đẹp cho nó vì khi nhìn vào đồ thị này, chúng ta để ý là có 2 đường thể hiện, 1 đường cho Canada và 1 cho B.C, nhưng chúng ta không biết đường nào thuộc về vùng nào.

#
# trang điểm
dataplot <- ggplot(thedata_BC_Can, aes(x=ref_date, y=value, colour=geo)) + 
  geom_line(size=1.5) 

plot(dataplot) 


dataplot2 <- dataplot +
  scale_x_date(date_breaks = "2 years", labels = year) +
  scale_y_continuous(labels = comma, limits = c(80, 125)) +
  scale_colour_manual(name=NULL,
                      breaks=c("Canada", "British Columbia"),
                      labels=c("Canada", "British Columbia"), 
                      values=c("#325A80", "#CCB550")) +
  bida_chart_theme
#  theme_bw() +
#  theme(
#    panel.border = element_rect(colour="white"),
#    plot.title = element_text(face="bold"),
#    legend.position=c(1,0), 
#    legend.justification=c(1,0),
#    legend.title = element_text(size=12),
#    legend.text = element_text(size=11),
#    axis.line = element_line(colour="black"),
#    axis.title = element_text(size=12),
#    axis.text = element_text(size=12)
#  )
#
plot(dataplot2)


# experiments with ggplot2's new subtitle and caption options
NHPI_title <- as.character("New Housing Price Index, Canada & B.C.")
NHPI_subtitle <- as.character("December 2016 = 100")
NHPI_caption <- as.character("Source: Statistics Canada, CANSIM table 18-10-0205-01")
# add titles / X-Y axis labels
NHPI_plot <- dataplot2 +
  labs(title = NHPI_title,
       subtitle = NHPI_subtitle,
       caption = NHPI_caption, 
       x = NULL, y = "NHPI (Dec. 2016 = 100)")

plot(NHPI_plot)

ggsave(filename = "NHPI_plot.png", plot = NHPI_plot,
       width = 8, height = 6)


#nếu muốn biểu đồ động, thì có thể dùng thư viện plotly()

library(plotly)

ggplotly(NHPI_plot)


