#############################################################################
#### Workshop: Evaluación del Impacto del Clima en los Cultivos          ####
#############################################################################

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)    # Eliminar la notación científica
options(digits = 4)      # Número de decimales

# Apertura de paquetes

library(pacman)
p_load(agricolae,ExpDes,lmtest,nortest,dplyr,car,stringr,gtools, readxl, tidyr,
       olsrr, broom, phia, writexl, VIM, tidyverse, ppsr, recipes, timetk,
       forecast)

### Importando datos de produccion anual ----

data_prod_anual <- readxl::read_xlsx("data/data.xlsx", sheet = "Anual",
                          col_types = c(rep("numeric",4)))

data_clima_dia <- readxl::read_xlsx("data/data.xlsx", sheet = "Clima",
                                     col_types = c("date",
                                                   rep("numeric",4))) %>%
  select(-`HUMEDAD RELATIVA`) %>%
  rename("Fecha" = "AÑO / MES / DÍA") %>%
  mutate(MAX = ifelse(MAX <= 0, NA, MAX),
         MIN = ifelse(MIN <= 0, NA, MIN),
         PRECIPITACIÓN = ifelse(MIN < 0, NA, PRECIPITACIÓN))

data_clima_dia2 <- readxl::read_xlsx("data/data.xlsx", sheet = "Clima2",
                                    col_types = c(rep("numeric",6))) %>%
  mutate(MAX = ifelse(MAX <= 0, NA, MAX),
         MIN = ifelse(MIN <= 0, NA, MIN),
         PRECIPITACION = ifelse(MIN < 0, NA, PRECIPITACION))

# Paso 1: Combina Año, Mes y Día y conviértelos a formato POSIXct
data_clima_dia2 <- data_clima_dia2 %>%
  mutate(`Fecha` = as.POSIXct(paste(Año, Mes, Dia, sep = "-"), format = "%Y-%m-%d"))

# Paso 2: Selecciona las columnas deseadas
data_clima_dia2 <- data_clima_dia2 %>%
  select(`Fecha`, MAX, MIN, PRECIPITACION) %>%
  rename("PRECIPITACIÓN" = "PRECIPITACION")

# Unión de data clima

data_clima <- data_clima_dia %>%
  bind_rows(data_clima_dia2) %>%
  mutate(MAX = ifelse(MAX <= 0, NA, MAX),
         MIN = ifelse(MIN <= 0, NA, MIN),
         PRECIPITACIÓN = ifelse(MIN < 0, NA, PRECIPITACIÓN)) %>%
  rename("PP" = "PRECIPITACIÓN") %>%
  mutate(Fecha = as.Date(format(Fecha, format = "%Y/%m/%d"))) %>%
  arrange(Fecha) %>%
  pad_by_time(.by = "day")

data_clima %>% 
  filter(is.na(MAX))

## Imputación de datos ----

# Apply Imputation
# recipe_box_cox <- recipe(~ ., data = data_clima) %>%
#   step_ts_impute(MAX, MIN, PP, period = 12, lambda = "auto") %>%
#   prep()
# 
# data_clima <- recipe_box_cox %>% bake(data_clima)


# # Agrupa los datos por año y mes, y calcula las estadísticas deseadas
# data_resumida <- data_clima %>%
#   mutate(Año = year(Fecha), Mes = month(Fecha)) %>%
#   group_by(Año, Mes) %>%
#   summarize(MAX = max(MAX, na.rm = T),
#             MIN = min(MIN, na.rm = T),
#             PP = sum(PP, na.rm = T))
# 
# # Convierte Año y Mes en un formato legible
# data_resumida$Fecha <- as.Date(paste(data_resumida$Año, data_resumida$Mes, "01", sep = "-"))

# Elimina las columnas Año y Mes si no son necesarias
# data_resumida <- data_resumida %>% select(Fecha, MAX, MIN, PRECIPITACIÓN)

# Muestra el DataFrame resultante
# print(data_resumida)

# Agregar columnas de mes y año
data_clima <- data_clima %>%
  mutate(Año = year(Fecha), Mes = month(Fecha)) %>%
  group_by(Mes) %>%
  mutate_at(vars(MAX, MIN, PP), ~ zoo::na.approx(.))

## Resumir los datos ----

data_resumida <- data_clima %>%
  group_by(Año, Mes) %>%
  summarize(
    T.MAX = max(MAX, na.rm = T),
    T.MIN = min(MIN, na.rm = T),
    PP = sum(PP, na.rm = T)
  ) %>%
  ungroup()

# Obtener los nombres completos de los meses en español
nombres_meses <- month.name

# Cambiar el nombre de las columnas a los nombres completos de los meses
data_resumida <- data_resumida %>%
  mutate(DIF = T.MAX - T.MIN,
         Mes = factor(Mes, levels = 1:12, labels = nombres_meses))

# Convierte la variable Mes a un factor con orden
meses_ordenados <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

data_resumida$Mes <- factor(data_resumida$Mes, levels = meses_ordenados)

##  Crea el gráfico de series temporales ----

ggplot(data_resumida %>%
         filter(Año >= 1960), aes(x = Mes, y = T.MAX, group = Año, color = Año)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Maximum Temperature Time Series",
       x = "Month",
       y = "Maximum Temperature",
       colour = "Year") +
  theme_minimal() +
  viridis::scale_color_viridis() -> pp#+
#facet_wrap(Año~.)

años_interes <- c(1982, 1983, 1997, 1998, 2009, 2010, 2017)
data_filtrada <- data_resumida %>% filter(Año %in% años_interes)

# Graficar con colores específicos para cada año
ggplot(data_resumida %>% filter(Año >= 1960),
       aes(x = Mes, y = T.MAX,
           group = Año,
           color = factor(Año))) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_line(data = data_filtrada,
            aes(x = Mes,
                y = T.MAX,
                group = Año,
                color = factor(Año)),
            size = 1,
            alpha = 0.5) +
  geom_point(data = data_filtrada,
             aes(x = Mes,
                 y = T.MAX,
                 color = factor(Año)),
             size = 3,
             alpha = 0.5) +
  labs(title = "Maximum Temperature Time Series",
       x = "Month",
       y = "Maximum Temperature",
       color = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("1982" = "red",
                                "1983" = "blue",
                                "1997" = "green",
                                "1998" = "purple",
                                "2009" = "brown",
                                "2010" = "navy",
                                "2017" = "orange"))


ggsave("plot/ts2.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

ggplot(data_resumida %>%
         filter(Año >= 1960), aes(x = Mes, y = T.MIN, group = Año, color = Año)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Minimum Temperature Time Series",
       x = "Month",
       y = "Minimum Temperature",
       colour = "Year") +
  theme_minimal() +
  viridis::scale_color_viridis() -> pp

años_interes <- c(1982, 1983, 1997, 1998, 2009, 2010, 2017)
data_filtrada <- data_resumida %>% filter(Año %in% años_interes)

# Graficar con colores específicos para cada año
ggplot(data_resumida %>% filter(Año >= 1960),
       aes(x = Mes, y = T.MIN, group = Año, color = factor(Año))) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_line(data = data_filtrada,
            aes(x = Mes, y = T.MIN,
                group = Año,
                color = factor(Año)),
            size = 1,
            alpha = 0.5) +
  geom_point(data = data_filtrada,
             aes(x = Mes,
                 y = T.MIN,
                 color = factor(Año)),
             size = 3,
             alpha = 0.5) +
  labs(title = "Minimum Temperature Time Series",
       x = "Month",
       y = "Minimum Temperature",
       color = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("1982" = "red",
                                "1983" = "blue",
                                "1997" = "green",
                                "1998" = "purple",
                                "2009" = "brown",
                                "2010" = "navy",
                                "2017" = "orange"))

ggsave("plot/ts3.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

ggplot(data_resumida %>%
         filter(Año >= 1960), aes(x = Mes, y = DIF, group = Año, color = Año)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Thermal Differential Time Series",
       x = "Month",
       y = "Thermal Differential",
       colour = "Year") +
  theme_minimal() +
  viridis::scale_color_viridis() -> pp

ggsave("plot/ts4.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

ggplot(data_resumida %>%
         filter(Año >= 1960), aes(x = Mes, y = PP, group = Año, color = Año)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Rainfall Time Series",
       x = "Month",
       y = "Rainfall",
       colour = "Year") +
  theme_minimal() +
  viridis::scale_color_viridis() -> pp

años_interes <- c(1982, 1983, 1997, 1998, 2009, 2010, 2017)
data_filtrada <- data_resumida %>% filter(Año %in% años_interes)

# Graficar con colores específicos para cada año
ggplot(data_resumida %>% filter(Año >= 1960),
       aes(x = Mes, y = PP,
           group = Año, color = factor(Año))) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_line(data = data_filtrada,
            aes(x = Mes, y = PP,
                group = Año,
                color = factor(Año)),
            size = 1,
            alpha = 0.5) +
  geom_point(data = data_filtrada,
             aes(x = Mes,
                 y = PP,
                 color = factor(Año)),
             size = 3,
             alpha = 0.5) +
  labs(title = "Rainfall Time Series",
       x = "Month",
       y = "Rainfall",
       color = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("1982" = "red",
                                "1983" = "blue",
                                "1997" = "green",
                                "1998" = "purple",
                                "2009" = "brown",
                                "2010" = "navy",
                                "2017" = "orange"))

ggsave("plot/ts5.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

data_largo <- data_resumida %>%
  pivot_longer(cols = c(T.MAX, T.MIN, PP, DIF),
               names_to = "Variable",
               values_to = "Valor") %>%
  dplyr::mutate(Variable = factor(Variable,
                           levels = c("T.MAX",
                                      "T.MIN",
                                      "DIF",
                                      "PP"),
                           labels = c("Maximum Temperature (°C)",
                                      "Minimum Temperature (°C)",
                                      "Thermal Differential (°C)",
                                      "Rainfall (mm)")))

ggplot(data_largo %>%
         filter(Año >= 1960), aes(x = Mes, y = Valor, group = Año, color = Año)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Weather Time Series",
       x = "Month",
       y = "Value",
       colour = "Year") +
  theme_minimal() +
  facet_wrap(Variable~., scales = "free_y", ncol = 2) +
  viridis::scale_color_viridis() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) -> pp

ggsave("plot/ts6.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

## Pivotear los datos al formato largo ----

data_resumida_largo <- data_resumida %>%
  pivot_wider(
    id_cols = Año,
    names_from = Mes,
    values_from = c(T.MAX, T.MIN, DIF, PP)#,
    # names_glue = "{.value}_{.name}"
  )

# Mostrar el DataFrame resultante
print(data_resumida_largo)

## Ordenar los nombres de las columnas----

column_order <- c("Año", "T.MAX_January", "T.MAX_February", "T.MAX_March", "T.MAX_April", "T.MAX_May", "T.MAX_June", "T.MAX_July", "T.MAX_August", "T.MAX_September", "T.MAX_October", "T.MAX_November", "T.MAX_December",
                  "T.MIN_January", "T.MIN_February", "T.MIN_March", "T.MIN_April", "T.MIN_May", "T.MIN_June", "T.MIN_July", "T.MIN_August", "T.MIN_September", "T.MIN_October", "T.MIN_November", "T.MIN_December",
                  "DIF_January", "DIF_February", "DIF_March", "DIF_April", "DIF_May", "DIF_June", "DIF_July", "DIF_August", "DIF_September", "DIF_October", "DIF_November", "DIF_December",
                  "PP_January", "PP_February", "PP_March", "PP_April", "PP_May", "PP_June", "PP_July", "PP_August", "PP_September", "PP_October", "PP_November", "PP_December")

## Reorganizar las columnas----

data_clima_ordenado <- data_resumida_largo %>%
  select(all_of(column_order)) %>%
  filter(!is.na(Año))

## Mostrar el DataFrame resultante ----
print(data_clima_ordenado)

datos_filtrados <- data_clima_ordenado %>% 
  dplyr::filter(Año >= 1960)

# Especifica el nombre del archivo XLSX
nombre_archivo <- "datos_clima.xlsx"

## Guarda los datos en un archivo XLSX ----
write_xlsx(datos_filtrados, path = nombre_archivo)

## Generar data final ----
data_final <- data_prod_anual %>%
  left_join(data_clima_ordenado %>%
              select(!ends_with(c("July","August",
                                "September","October",
                                "November","December"))), by = "Año") #%>%
  # filter(!Año < 1990)

data_final %>% psych::describe()

## Función para crear gráficos de dispersión con facet_wrap y etiquetas de rho con significancia----
crear_grafico_dispersión_facet <- function(df, variable_y, term, xlab, ylab) {
  df_filtered <- df %>%
    select(Año, starts_with(term), all_of(variable_y))

  # Extraer el nombre del mes de las variables
  df_filtered <- df_filtered %>%
    rename_with(~gsub(paste0(term, "_"), "", .), starts_with(term))

  # Crear una columna de mes para facet_wrap
  df_melted <- df_filtered %>%
    pivot_longer(cols = -c(Año, all_of(variable_y)), names_to = "Variable_x", values_to = "Valor_x") %>%
    na.omit()

  # Definir el orden de los meses
  df_melted$Variable_x <- factor(df_melted$Variable_x, levels = month.name)

  # Calcular el coeficiente de correlación de Spearman en cada faceta
  correlations <- df_melted %>%
    group_by(Variable_x) %>%
    summarize(rho = cor.test(Valor_x, .data[[variable_y]], method = "spearman")$estimate,
              p_value = cor.test(Valor_x, .data[[variable_y]], method = "spearman")$p.value,
              stars = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", ""))))

  # Agregar etiquetas de rho con significancia a cada faceta
  correlations <- correlations %>%
    mutate(label = paste0("rho = ", round(rho,2), stars),
           rho_stars = paste0(round(rho,2), stars))

  grafico <- ggplot(df_melted, aes(x = Valor_x, y = .data[[variable_y]])) +
    geom_point(aes(colour = Año)) +
    labs(x = xlab, y = ylab, colour = "Year") +
    facet_wrap(~ Variable_x, scales = "free_x") +
    ggtitle(paste("Scatter plot:", ylab, "vs", xlab)) +
    geom_smooth(method = "gam") +
    theme_minimal() +
    viridis::scale_color_viridis()

  grafico <- grafico +
    geom_text(data = correlations, aes(label = label, x = Inf, y = -Inf, hjust = 1, vjust = 0))

  return(list(grafico = grafico, correlaciones = correlations))
}

## Ejemplo de uso ----
grafico <- crear_grafico_dispersión_facet(data_final, "Productividad_kg.ha", "T.MAX",
                                          xlab = "Maximum Temperature (°C)",
                                          ylab = "Productivity (kg/ha)")
grafico[[1]]
grafico[[2]]

# ggsave("plot/plottest.jpeg",
#        # width = 144.2,
#        # height = 100,
#        width = 246.2,
#        height = 144.2,
#        units = "mm",
#        dpi = 600,
#        # scale = 2,
#        plot = grafico[[1]])

### DIF

grafico <- crear_grafico_dispersión_facet(data_final,
                                          "Superficie_ha", "DIF",
                                          xlab = "Thermal Differential (°C)",
                                          ylab = "Harvested Area (ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot1.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])


grafico <- crear_grafico_dispersión_facet(data_final,
                                          "Producción_Tn", "DIF",
                                          xlab = "Thermal Differential (°C)",
                                          ylab = "Production (tons)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot2.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

grafico <- crear_grafico_dispersión_facet(data_final,
                                          "Productividad_kg.ha", "DIF",
                                          xlab = "Thermal Differential (°C)",
                                          ylab = "Productivity (kg/ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot3.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

### TMAX

grafico <- crear_grafico_dispersión_facet(data_final,
                                          "Superficie_ha", "T.MAX",
                                          xlab = "Maximum Temperature (°C)",
                                          ylab = "Harvested Area (ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot4.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

grafico <- crear_grafico_dispersión_facet(data_final, "Producción_Tn", "T.MAX",
                                          xlab = "Maximum Temperature (°C)",
                                          ylab = "Production (tons)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot5.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

grafico <- crear_grafico_dispersión_facet(data_final, "Productividad_kg.ha", "T.MAX",
                                          xlab = "Maximum Temperature (°C)",
                                          ylab = "Productivity (kg/ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot6.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

### TMIN

grafico <- crear_grafico_dispersión_facet(data_final, "Superficie_ha", "T.MIN",
                                          xlab = "Minimum temperature (°C)",
                                          ylab = "Harvested Area (ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot7.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

grafico <- crear_grafico_dispersión_facet(data_final, "Producción_Tn", "T.MIN",
                                          xlab = "Minimum temperature (°C)",
                                          ylab = "Production (tons)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot8.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

grafico <- crear_grafico_dispersión_facet(data_final, "Productividad_kg.ha", "T.MIN",
                                          xlab = "Minimum temperature (°C)",
                                          ylab = "Productivity (kg/ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot9.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

### PP

grafico <- crear_grafico_dispersión_facet(data_final, "Superficie_ha", "PP",
                                          xlab = "Rainfall (mm)",
                                          ylab = "Harvested Area (ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot10.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

grafico <- crear_grafico_dispersión_facet(data_final, "Producción_Tn", "PP",
                                          xlab = "Rainfall (mm)",
                                          ylab = "Production (tons)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot11.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])

grafico <- crear_grafico_dispersión_facet(data_final, "Productividad_kg.ha", "PP",
                                          xlab = "Rainfall (mm)",
                                          ylab = "Productivity (kg/ha)")
print(grafico[[1]])
grafico[[2]]

ggsave("plot/plot12.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = grafico[[1]])


# Reorganizar los datos en formato largo (tidy)
data_final_long <- data_final %>%
  mutate(PP = rowSums(select(., starts_with("PP")))) %>%
  select(#T.MAX_March, T.MAX_April, T.MAX_May, DIF_May, PP, 
         Productividad_kg.ha, Producción_Tn, Superficie_ha, Año) %>%
  pivot_longer(cols = c(#T.MAX_March, T.MAX_April, T.MAX_May, DIF_May, PP,
                        Productividad_kg.ha, Producción_Tn, Superficie_ha),
               names_to = "Variable",
               values_to = "Valor")  %>%
  mutate(Variable = gsub("PP", "Rainfall (mm)", Variable),
         Variable = gsub("Productividad_kg.ha", "Productivity (kg/ha)", Variable),
         Variable = gsub("Producción_Tn", "Production (tons)", Variable),
         Variable = gsub("Superficie_ha", "Harvest Area (ha)", Variable),
         Variable = gsub("_", " ", Variable),
         Variable = gsub("T.MAX", "Maximum Temperature (°C)", Variable),
         Variable = gsub("DIF", "Thermal Differential (°C)", Variable))

# Crear el gráfico de serie de tiempo con facet_wrap
ggplot(data_final_long, aes(x = Año, y = Valor)) +
  geom_line() +
  labs(x = "Year", y = "Value") +
  facet_wrap(~ Variable, scales = "free_y", nrow = 8) +
  scale_color_manual(values = c("blue", "red", "green")) -> pp# +
  # theme_minimal()

ggsave("plot/ts1.jpeg",
       # width = 144.2, 
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

## Modelos de regresión ----

## Importancia de variables ----

v1 <- ppsr::visualize_pps(df = data_final %>%
                            dplyr::select(-contains(c("Productividad",
                                                      "Superficie",
                                                      "Año"))),
                          y = c("Producción_Tn"),
                          do_parallel = TRUE, n_cores = 7,
                          include_target = F)
v1

ggsave("plot/ppsr_produccion.jpeg",
       width = 144.2,
       height = 100,
       # width = 246.2,
       # height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = v1)

## Ajuste de un modelo de regresión con errores ARIMA ----

data_final_y <- ts(data_final %>% 
                     select(Producción_Tn,
                            Productividad_kg.ha,
                            Superficie_ha),start = 1960, end = 2022)

data_final_x <- ts(data_final %>% 
                     select(!c(Año, Producción_Tn,
                            Productividad_kg.ha,
                            Superficie_ha)),start = 1960, end = 2022)

fit1 <- auto.arima(data_final_y[,"Producción_Tn"], 
                   xreg=data_final_x[,c("T.MAX_May",
                                        "PP_February",
                                        "T.MAX_March")],
                   stepwise = F,
                   trace = T#,
              # c(1,1,2)
              )
summary(fit1)

windows()

# Abrir dispositivo de gráficos en formato PNG
jpeg("plot/check_produccion.jpeg",
    width = 246.2,
    height = 144.2,
    units = "mm", 
    res = 300)

# set.seed(2021)
checkresiduals(fit1, plot = T)
# dev.copy2pdf(file = "plot/check_produccion.pdf")
dev.off()

cbind("Production (tons)" = data_final_y[,"Producción_Tn"],
      "Fitted" = fitted(fit1),
      "Regression Errors" = residuals(fit1, type="regression"),
      "ARIMA errors" = residuals(fit1, type="innovation")) %>%
  autoplot(facets=TRUE) -> pp

ggsave("plot/errors_produccion.jpeg",
       # width = 144.2,
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

v2 <- ppsr::visualize_pps(df = data_final %>%
                            dplyr::select(-contains(c("Producción",
                                                      "Superficie",
                                                      "Año"))),
                          y = c("Productividad_kg.ha"),
                          do_parallel = TRUE, n_cores = 7,
                          include_target = F)
v2

ggsave("plot/ppsr_productividad.jpeg",
       width = 144.2,
       height = 100,
       # width = 246.2,
       # height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = v2)

fit2 <- auto.arima(data_final_y[,"Productividad_kg.ha"], 
                   xreg=data_final_x[,c("T.MAX_May")],
                   stepwise = F,
                   trace = T#,
                   # c(1,1,2)
)
summary(fit2)

windows()

# Abrir dispositivo de gráficos en formato PNG
jpeg("check_productividad.jpeg",
     width = 246.2,
     height = 144.2,
     units = "mm", 
     res = 300)

# set.seed(2021)
checkresiduals(fit2, plot = T)
# dev.copy2pdf(file = "plot/check_produccion.pdf")
dev.off()


cbind("Productivity (kg/ha)" = data_final_y[,"Productividad_kg.ha"],
      "Fitted" = fitted(fit2),
      "Regression Errors" = residuals(fit2, type="regression"),
      "ARIMA errors" = residuals(fit2, type="innovation")) %>%
  autoplot(facets=TRUE) +
  labs(y = "value") -> pp

ggsave("errors_productividad.jpeg",
       # width = 144.2,
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)

v3 <- ppsr::visualize_pps(df = data_final %>%
                            dplyr::select(-contains(c("Producción",
                                                      "Productividad",
                                                      "Año"))),
                          y = c("Superficie_ha"),
                          do_parallel = TRUE, n_cores = 7,
                          include_target = F)
v3

ggsave("ppsr_superficie.jpeg",
       width = 144.2,
       height = 100,
       # width = 246.2,
       # height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = v3)

fit3 <- auto.arima(data_final_y[,"Superficie_ha"], 
                   xreg=data_final_x[,c("DIF_May",
                                        "T.MAX_April",
                                        "T.MAX_May")],
                   stepwise = F,
                   trace = T#,
                   # c(1,1,2)
)
summary(fit3)


windows()

# Abrir dispositivo de gráficos en formato PNG
jpeg("check_superficie.jpeg",
     width = 246.2,
     height = 144.2,
     units = "mm", 
     res = 300)

# set.seed(2021)
checkresiduals(fit3)
# dev.copy2pdf(file = "plot/check_produccion.pdf")
dev.off()

cbind("Harvest Area (ha)" = data_final_y[,"Superficie_ha"],
      "Fitted" = fitted(fit3),
      "Regression Errors" = residuals(fit3, type="regression"),
      "ARIMA errors" = residuals(fit3, type="innovation")) %>%
  autoplot(facets=TRUE) +
  labs(y = "value") -> pp

ggsave("errors_superficie.jpeg",
       # width = 144.2,
       # height = 100,
       width = 246.2,
       height = 144.2,
       units = "mm", 
       dpi = 600,
       # scale = 2,
       plot = pp)
