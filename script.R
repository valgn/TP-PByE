# Instalar si no los tenés
#install.packages("googledrive")
#install.packages("readxl")


# Cargar las librerías
library(googledrive)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr) 
library(stringr)



drive_download(as_id("10DMEGyZUcNUpHCZN_tLg5GgnoafbmxDZKGtu6CzEDbk"),
               path = "Datos_LP.xlsx",
               overwrite = TRUE)


# Leer la hoja "DATOS"
datos <- read_excel("Datos_LP.xlsx", sheet = "DATOS", skip = 2)
#names(datos)
# Verificamos nombres de columnas disponibles (opcional)
# Ver las primeras filas sin encabezado, para ubicar la fila correcta
#datos_raw <- read_excel("Datos_LP.xlsx", sheet = "DATOS", col_names = FALSE)
#head(datos_raw, 15)
#names(datos)
# Creamos el boxplot para edad_jefe
#`Edad jefe/a del hogar`
# Calcular la media para la anotación
# Calcular la media para la anotación

# Calculamos la media primero
media_edad <- mean(datos$edad_jefe, na.rm = TRUE)

ggplot(datos, aes(y = edad_jefe)) +
  geom_boxplot(
    fill = "#4E79A7",
    color = "#2E4A6E",
    width = 0.4
  ) +
  geom_hline(
    yintercept = media_edad,
    color = "#F28E2B",
    linetype = "dashed",
    size = 0.8
  ) +
  annotate(
    "text",
    x = 0.7,
    y = media_edad,
    label = paste("Media:", round(media_edad, 1)),
    color = "#F28E2B",
    hjust = 0,
    size = 4
  ) +
  labs(title = "EDAD DE JEFES DE HOGAR", y = "Años") +
  theme_minimal()

ggplot(datos, aes(x = tiempo_residencia)) +
  geom_histogram(bins = 20, fill = "#52854C", color = "white", alpha = 0.8) +
  labs(title = "Tiempo de Residencia en la Vivienda",
       subtitle = "Distribución de años de residencia del jefe de hogar",
       x = "Años de residencia", 
       y = "Frecuencia",
       caption = "Fuente: Datos_LP.xlsx") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Primero verificamos los valores únicos para asegurarnos
# Primero verificamos los valores únicos para asegurarnos
unique(datos$discapacidades_vivienda)

# Convertimos a factor si es necesario
datos$discapacidades_vivienda <- factor(datos$discapacidades_vivienda)

ggplot(datos, aes(x = discapacidades_vivienda)) +
  geom_bar(fill = c("#D16103", "#4E84C4"), width = 0.3) +
  labs(title = "Presencia de Personas con Discapacidad en la Vivienda",
       subtitle = "Análisis de la variable 'discapacidades_vivienda'",
       x = "¿Hay personas con discapacidad?", 
       y = "Cantidad de viviendas",
       caption = "Fuente: Datos_LP.xlsx") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Primero creamos una tabla de frecuencias
frecuencias <- datos %>%
  count(genero_disidente_vivienda) %>%
  arrange(genero_disidente_vivienda)

ggplot(frecuencias, aes(x = genero_disidente_vivienda, y = n)) +
  geom_step(direction = "hv", color = "#C4961A", size = 1.2) +
  geom_point(color = "#C4961A", size = 3) +
  labs(title = "Personas de Género Disidente por Vivienda",
       subtitle = "Distribución de la variable 'genero_disidente_vivienda'",
       x = "Número de personas", 
       y = "Cantidad de viviendas",
       caption = "Fuente: Datos_LP.xlsx") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Definimos el orden deseado manualmente

orden_deseado <- c("Al menos 5 veces a la semana",
                   "Entre 2 y 4 veces a la semana",
                   "Una vez a la semana",
                   "No hay servicio de recolección municipal")

# 2. Preparar los datos
datos_frecuencia <- datos %>%
  count(frecuencia_recoleccion) %>%
  mutate(
    porcentaje = n / sum(n) * 100,
    frecuencia_recoleccion = factor(frecuencia_recoleccion, levels = orden_deseado, ordered = TRUE)
  ) %>%
  arrange(frecuencia_recoleccion)

# 3. Crear el gráfico (versión corregida)
ggplot(datos_frecuencia, aes(y = frecuencia_recoleccion, x = n)) +
  geom_col(fill = "#1F77B4", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste(n, "(", round(porcentaje, 1), "%)")), 
            hjust = -0.1, size = 3.5, color = "black") +
  labs(title = "FRECUENCIA DE RECOLECCIÓN DE RESIDUOS",
       x = "Número de viviendas",
       y = "") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)))

datos_plagas <- datos %>%
  mutate(
    # Identificar casos sin plagas (espacios en blanco o NA)
    sin_plagas = is.na(tipo_plagas_consolidado) | tipo_plagas_consolidado == "",
    
    # Limpieza básica para casos con plagas
    tipo_plagas = ifelse(sin_plagas, "No tiene plagas", str_squish(tolower(tipo_plagas_consolidado)))
  ) %>%
  # Separamos las respuestas múltiples (solo para casos con plagas)
  mutate(
    plagas = ifelse(sin_plagas, 
                    list("No tiene plagas"), 
                    str_split(tipo_plagas, " "))
  ) %>% 
  # Creamos una fila por cada reporte
  unnest(plagas) %>%
  # Estandarizamos los nombres
  mutate(
    plagas = case_when(
      str_detect(plagas, "cucaracha") ~ "Cucarachas",
      str_detect(plagas, "rata") ~ "Ratas",
      str_detect(plagas, "mosquito") ~ "Mosquitos",
      TRUE ~ plagas  # Mantenemos otros valores como están (incluye "No tiene plagas")
    )
  ) %>%
  # Filtramos y categorizamos
  mutate(
    plagas = factor(plagas, 
                    levels = c("Cucarachas", "Ratas", "Mosquitos", "No tiene plagas"))
  ) %>%
  # Contamos frecuencia
  count(plagas) %>%
  # Calculamos porcentaje
  mutate(porcentaje = n / sum(n) * 100)

# Verificamos los datos
print(datos_plagas)

# Gráfico de torta mejorado
ggplot(datos_plagas, aes(x = "", y = n, fill = plagas)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(porcentaje), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, fontface = "bold") +
  labs(
    title = "DISTRIBUCIÓN DE PLAGAS EN VIVIENDAS",
    subtitle = "Incluyendo viviendas sin plagas reportadas",
    fill = "Estado de plagas",
    caption = paste("Total de viviendas analizadas:", sum(datos_plagas$n))
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  scale_fill_manual(
    values = c("#E63946", "#1D3557", "#2A9D8F", "#999999"),  # Rojo, azul oscuro, verde, gris
    labels = c("Cucarachas", "Ratas", "Mosquitos", "No tiene plagas")
  )

datos <- datos %>%
  mutate(
    presion_agua = trimws(tolower(presion_agua)),  # Minúsculas y sin espacios
    presion_agua = case_when(
      presion_agua %in% c("muy debil", "muy débil", "muy baja") ~ "Muy débil",
      presion_agua %in% c("debil", "débil", "baja") ~ "Débil",
      presion_agua %in% c("buena", "normal", "adecuada") ~ "Buena",
      TRUE ~ presion_agua  # Mantener otros valores como están
    ),
    # Convertir a factor con niveles ordenados
    presion_agua = factor(presion_agua,
                          levels = c("Muy débil", "Débil", "Buena"),
                          ordered = TRUE)
  )

# 3. Gráfico mejorado
ggplot(datos, aes(x = presion_agua)) +
  geom_bar(fill = "#4E84C4", width = 0.6, alpha = 0.8) +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(
    title = "PRESIÓN DEL AGUA EN LAS VIVIENDAS",
    subtitle = "Distribución según nivel de presión reportado",
    x = "Nivel de presión",
    y = "Cantidad de viviendas",
    caption = paste("Total de viviendas:", sum(!is.na(datos$presion_agua)))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#Transforma espacios en blanco en "no responde"
datos <- datos %>%
  mutate(
    instalacion_agua_caliente = ifelse(
      is.na(instalacion_agua_caliente) | instalacion_agua_caliente == "",
      "No responde",
      instalacion_agua_caliente
    )
  )


datos %>%
  count(instalacion_agua_caliente) %>%
  ggplot(aes(x = "", y = n, fill = instalacion_agua_caliente)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Instalación de Agua Caliente en las Viviendas",
       fill = "¿Tiene instalación?") +
  theme_void()

# Procesamiento de datos incluyendo "No responde"
tabla_humedad <- datos %>%
  mutate(
    problemas_humedad_consolidado = ifelse(
      is.na(problemas_humedad_consolidado) | problemas_humedad_consolidado == "",
      "No responde",
      problemas_humedad_consolidado
    )
  ) %>%
  count(problemas_humedad_consolidado) %>%
  mutate(
    porcentaje = n / sum(n) * 100,
    # Ordenamos por frecuencia (opcional)
    problemas_humedad_consolidado = factor(
      problemas_humedad_consolidado,
      levels = problemas_humedad_consolidado[order(n)]
    )
  )


humedad_cols <- grep("humedad", names(datos), ignore.case = TRUE, value = TRUE)


# Gráfico horizontal mejorado
ggplot(tabla_humedad, aes(y = reorder(problemas_humedad_consolidado, n), x = n)) +
  geom_bar(stat = "identity", fill = "#8B4513", alpha = 0.8, width = 0.7) +  # Color café mejorado
  geom_text(
    aes(label = paste0(n, " (", round(porcentaje, 1), "%)")), 
    hjust = -0.1, 
    size = 3.5,
    color = "black"
  ) +
  labs(
    title = "PROBLEMAS DE HUMEDAD REPORTADOS",
    subtitle = "Distribución de tipos de humedad en las viviendas",
    x = "Número de viviendas", 
    y = "",
    caption = paste("Total de registros:", sum(tabla_humedad$n))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "#4A4A4A"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F7F7F"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(color = "#7F7F7F", size = 10)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15)),  # 15% más de espacio a la derecha
    breaks = scales::pretty_breaks()
  ) +
  scale_fill_manual(values = c("#8B4513", "#D2B48C", "#A0522D"))  # Escala de colores tierra

ggplot(datos, aes(x = intentaron_desalojar_su_residencia)) +
  geom_bar(fill = c("red", "green")) +
  labs(title = "Intento de Desalojo en Viviendas",
       x = "¿Intentaron desalojarlos?", y = "Cantidad de viviendas") +
  theme_minimal()

datos %>%
  filter(!is.na(Cuantas_veces_intentaron_desalojar)) %>%
  count(Cuantas_veces_intentaron_desalojar) %>%
  ggplot(aes(x = Cuantas_veces_intentaron_desalojar, y = n)) +
  geom_step(direction = "hv", color = "#E63946", size = 1.2) +
  geom_point(color = "#E63946", size = 3) +
  labs(
    title = "INTENTOS DE DESALOJO (CASOS REPORTADOS)",
    subtitle = paste("Se omiten", sum(is.na(datos$Cuantas_veces_intentaron_desalojar)), "casos sin datos"),
    x = "Número de intentos de desalojo",
    y = "Cantidad de viviendas",
    caption = paste("Total viviendas analizadas:", nrow(datos))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50")
  )

datos %>%
  count(menores_en_la_vivienda) %>%
  ggplot(aes(x = menores_en_la_vivienda, y = n)) +
  geom_step(direction = "hv", color = "darkblue", size = 1.5) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "Cantidad de Menores en la Vivienda",
       x = "Número de menores", y = "Frecuencia") +
  theme_minimal()



