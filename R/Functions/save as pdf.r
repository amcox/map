save_plot_as_pdf <- function(plot, file_name, wide=T){
  if(wide){
    pdf(paste0("./../Output/", file_name, ".pdf"),
        width=10.5, height=8
    )
  }else{
    pdf(paste0("./../Output/", file_name, ".pdf"),
        width=8, height=10.5
    )
  }
  print(plot)
  dev.off()
}

save_plot_as_pdf_adjustable <- function(plot, file_name, w=10.5, h=8){
  pdf(paste0("./../Output/", file_name, ".pdf"),
      width=w, height=h
  )
  print(plot)
  dev.off()
}