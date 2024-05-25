
library(MixSIAR)
                         
#Establecer el directorio de de trabajo: session>set working directory>choose directory y escoger la carpeta donde est?n los archivos

#despu?s de cargados agregar los datos de la mezcla: ojo, revisar que los archivos csv definitivamente esten separados por coma y no por punto y coma

mix <- load_mix_data(file="consumersXY17.csv",
                     iso_names=c("d13C","d15N"),
                     factors="Group",
                     fac_random=FALSE,
                     fac_nested=FALSE,
                     cont_effects=NULL)
#cargar datos de las fuentes
source <- load_source_data(filename="sourcesXY17.csv",
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means", mix)
#cargar el archivo de discriminaci?n
discr <- load_discr_data(filename="discriminationXY17.csv", mix)

#Crear la gr?fica 
plot_data(filename="isospace_plot", plot_save_pdf=FALSE,
          plot_save_png=FALSE, mix,source,discr)

#calcular ?rea
calc_area(source = source, mix = mix, discr = discr)


#plot prior (gr?fico de barras de proporciones, en rojo que es propio y en gris generalista)
# plot_prior( 
  # alpha.prior = 1,
  # source,
  # plot_save_pdf = TRUE,
  # plot_save_png = FALSE,
  # filename = "prior_plot XY17")

###############################################
# Crea los parametros de la grafica 
n.sources <- source$n.sources # 17 graficas
if (is.numeric(alpha.prior) == F) 
  alpha.prior = 1
if (length(alpha.prior) == 1) 
  alpha = rep(alpha.prior, n.sources)
if (length(alpha.prior) > 1 & length(alpha.prior) != n.sources) 
  alpha = rep(1, n.sources)
if (length(alpha.prior) > 1 & length(alpha.prior) == n.sources) 
  alpha = alpha.prior
alpha.unif <- rep(1, n.sources)
alpha.jeff <- rep(1/n.sources, n.sources)
p = MCMCpack::rdirichlet(10000, alpha)
p.unif = MCMCpack::rdirichlet(10000, alpha.unif)
alpha_lab <- paste0("(", paste0(round(alpha, 2), collapse = ","), 
                    ")", sep = "")
alpha.unif_lab <- paste0("(", paste0(round(alpha.unif, 
                                           2), collapse = ","), ")", sep = "")


for (i in 1:n.sources) {
  # 1. Open jpeg file y nombra
  jpeg(paste(source$source_names[i], ".jpg", sep = ""), 
       width = 600, height = 150)
  # 2. Create the species plot
  par(mfrow=c(1,2)) # dos columnas
  par(mai = rep(0.5, 4)) #recorta margen
  # primer histograma
  hist(p[, i], breaks = seq(0, 1, length.out = 40), 
       col = "red", main = source$source_names[i], 
       xlab = expression(p[i]))
  # Segundo histograma
  hist(p.unif[, i], breaks = seq(0, 1, length.out = 40), 
       col = "darkgrey", main = source$source_names[i], 
       xlab = expression(p[i]), xlim = c(0, 1))
  # 3. Close the file
  dev.off()
}

# crea la leyenda en otro archivo  
par(mai = c(0, 0, 0, 0))
plot.new()
# 1. Open jpeg file
jpeg("leyenda.jpg", 
     width = 600, height = 150)
# plotea leyenda
legend(x = "center", ncol = 1, 
       legend = c(paste0("Your prior: ", 
                         alpha_lab), paste0("\"Uninformative\" prior", 
                                            alpha.unif_lab)), fill = c("red", "darkgrey"), 
       bty = "n", cex = 0.7)
# 3. Close the file
dev.off()
###############################################






# crear el modelo
write_JAGS_model(
  filename = "MixSIAR_model_XY17.txt",
  resid_err = TRUE,
  process_err = TRUE,
  mix,
  source)

#correr el modelo
jags.1 <- run_model(
  "test",
  mix,
  source,
  discr,
  "MixSIAR_model_XY17.txt",
  alpha.prior = 1,
  resid_err = NULL,
  process_err = NULL)

#output del modelo
output_JAGS(
  jags.1,
  mix,
  source,
  output_options = list(summary_save = TRUE, summary_name = "summary_statistics",
                        sup_post = FALSE, plot_post_save_pdf = TRUE, plot_post_name = "posterior_density",
                        sup_pairs = FALSE, plot_pairs_save_pdf = TRUE, plot_pairs_name = "pairs_plot", sup_xy
                        = TRUE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", gelman = TRUE, heidel =
                          FALSE, geweke = TRUE, diag_save = TRUE, diag_name = "diagnostics", indiv_effect =
                          FALSE, plot_post_save_png = FALSE, plot_pairs_save_png = FALSE, plot_xy_save_png =
                          FALSE, diag_save_ggmcmc = TRUE)
)
