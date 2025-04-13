# Instalo los paquetes necesarios (si aún no los tengo instalados)
#install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

######################
# Renombrar columnas #
######################
# Lee los datos
column_names <- c(
  "edad_jefe",                 # cuantitativa discreta (años cumplidos)
  "tiempo_residencia",         # cuantitativa continua (años de residencia)
  "integrantes_vivienda",      # cuantitativa discreta (n° personas)
  "familias_convivientes",     # cuantitativa discreta (n° familias)
  "varones_vivienda",          # cuantitativa discreta (n° varones)
  "mujeres_vivienda",          # cuantitativa discreta (n° mujeres)
  "genero_disidente_vivienda", # cuantitativa discreta (n° personas)
  "menores18_vivienda",        # cuantitativa discreta (n° menores)
  "discapacidades_vivienda",   # cualitativa nominal (sí/no + tipos)
  "ambientes_dormitorio",      # cuantitativa discreta (n° ambientes)
  "max_personas_dormitorio",   # cuantitativa discreta (n° personas)
 
   "certificado_renabap",       # cualitativa nominal (sí/no)
  "intento_desalojo",          # cualitativa nominal (sí/no)
  "veces_intento_desalojo",    # cuantitativa discreta (n° intentos)
  "tiempo_ultimo_desalojo",    # cuantitativa continua (años desde último)
  "tipo_vivienda",             # cualitativa nominal (propia/alquilada/etc.)
  "contrato_alquiler",         # cualitativa nominal (sí/no)
  "costo_alquiler",            # cuantitativa continua (monto $)
  "aumento_alquiler_anual",    # cuantitativa continua (monto $)
  "porcentaje_aumento",        # cuantitativa continua (%)
  
  "fuente_agua",               # cualitativa nominal (red/pozo/etc.)
  "compra_agua_embotellada",   # cualitativa nominal (sí/no)
  "presion_agua",              # cualitativa ordinal (buena/regular/mala)
  "almacenamiento_agua_altura",# cualitativa nominal (sí/no)
  "litros_almacenamiento",     # cuantitativa continua (litros)
  "posee_baño",                # cualitativa nominal (sí/no)
  "lugar_higiene",             # cualitativa nominal (descripción lugar)
  "baños_compartidos",         # cualitativa nominal (sí/no)
  "baño_con_descarga",         # cualitativa nominal (sí/no)
  "tipo_desague",              # cualitativa nominal (red/cloaca/etc.)
  "agua_cocina",               # cualitativa nominal (sí/no)
  "agua_caliente_cocina",      # cualitativa nominal (sí/no + tipo)
  "agua_baño",                 # cualitativa nominal (sí/no)
  "agua_caliente_baño",        # cualitativa nominal (sí/no + tipo)
  
  "fuente_energia_cocinar",    # cualitativa nominal (gas/leña/electricidad)
  "fuente_calefaccion",        # cualitativa nominal (gas/leña/etc.)
  "ventilacion_calefaccion",   # cualitativa nominal (sí/no)
  
  "conexion_electrica",        # cualitativa nominal (formal/precaria/etc.)
  "tendido_electrico",         # cualitativa nominal (descripción)
  "perdida_electrodomesticos", # cualitativa nominal (sí/no)
  "incendios_electricos",      # cualitativa nominal (sí/no)
  "cortes_electricos_verano",  # cualitativa ordinal (frecuencia)
  "cortes_electricos_invierno",# cualitativa ordinal (frecuencia)
  
  "internet_banda_ancha",      # cualitativa nominal (sí/no)
  "celular_con_datos",         # cualitativa nominal (sí/no)
  "abonos_datos_moviles",      # cuantitativa discreta (n° abonos)
  "computadoras",              # cuantitativa discreta (n° dispositivos)
  "dispositivos_moviles",      # cuantitativa discreta (n° dispositivos)
  
  "contrapiso",                # cualitativa nominal (sí/no)
  "material_piso",             # cualitativa nominal (cerámico/cemento/etc.)
  "material_techo",            # cualitativa nominal (chapa/losa/etc.)
  "aislamiento_techo",         # cualitativa nominal (sí/no)
  "material_puertas_exterior", # cualitativa nominal (madera/metal/etc.)
  "material_paredes_exteriores",# cualitativa nominal (ladrillo/chapa/etc.)
  "terminacion_exterior",      # cualitativa nominal (sí/no)
  "tipo_terminacion_exterior", # cualitativa nominal (revoque/pintura/etc.)
  "pintura_exterior",          # cualitativa nominal (sí/no)
  "problemas_humedad",         # cualitativa nominal (sí/no)
  "riesgo_derrumbe",           # cualitativa nominal (sí/no)
  "trabajo_en_vivienda",       # cualitativa nominal (sí/no)
  "tipo_trabajo",              # cualitativa nominal (descripción)
  
  "calle_asfaltada",           # cualitativa nominal (sí/no)
  "salida_calle",              # cualitativa nominal (sí/no)
  "veredas",                   # cualitativa nominal (sí/no)
  "alumbrado_publico",         # cualitativa nominal (sí/no)
  "arbolado_manzana",          # cualitativa ordinal (abundante/escaso/etc.)
  "plagas",                    # cualitativa nominal (sí/no)
  "tipo_plagas",               # cualitativa nominal (cucarachas/ratas/etc.)
  "cucarachas",
  "ratas",
  "mosquitos",
  
  "espacios_esparcimiento",    # cualitativa nominal (lista de espacios)
  "frecuencia_uso_espacios",   # cualitativa ordinal (diario/semanal/etc.)
  "espacios_verdes",           # cualitativa nominal (parques/plazas/etc.)
  "frecuencia_uso_verdes",     # cualitativa ordinal (diario/semanal/etc.)
  "frecuencia_transito",       # cualitativa ordinal (alta/media/baja)
  "diferencia_transito_dia_noche", # cualitativa nominal (sí/no)
  "acceso_bicicletas_publicas",# cualitativa nominal (sí/no)
  "basurales_cercanos",        # cualitativa nominal (sí/no)
  "contenedor_basura",         # cualitativa nominal (sí/no)
  "eliminacion_residuos",      # cualitativa nominal (recolección/quema/etc.)
  "frecuencia_recoleccion",    # cualitativa ordinal (diario/semanal/etc.)
  "riesgo_inundacion"          # cualitativa ordinal (alto/medio/bajo)
)

###################
# Modificar datos #
###################
datos_limpios <- datos %>% # Los pipelines permiten encadenar acciones
	
	mutate(   # Para crear nuevas variables y editar las ya existentes
		
		# Veo valores min y max de la variable para elegir una
		# particion en intervalos apropiada
		# min(altura)
		# max(altura)
		# sqrt(nrow(datos))
		
		# Creo una variable nueva, con la partición en intervalos de altura
		edad_jefe = cut(edad_jefe,
										 breaks = seq(from=10, to=90, by = 5),
										 right = F),
		
		# Modifico las columnas de la variable de respuesta múltiple
		# para dejarlas como indicadoras con valores 1 (en caso de presentar
		# el atributo) y 0 (en caso de no presentarlo)
		
		certificado_renabap = ifelse(certificado_renabap == "Si", 1, 0),
		celular_con_datos = ifelse(celular_con_datos == "Si", 1, 0 ),
		plagas = ifelse( plagas == "Si", 1, 0 ),
		cucarachas = ifelse( cucarachas == "cucarachas", 1, 0),
		ratas = ifelse( ratas == "ratas", 1, 0),
		mosquitos = ifelse( mosquitos == "mosquitos", 1, 0),
		
		# Notar que los NA no entran dentro de la categoría "no presentar 
		# el atributo", por lo que requieren un tratamiento particular:
		
		cucarachas = ifelse(is.na(cucarachas), 0, 1),
		ratas = ifelse(is.na(ratas), 0, 1),
		mosquitos = ifelse(is.na(mosquitos), 0, 1),

		# Esto solo es correcto porque teníamos dos valores posibles en estas
		# columnas: presencia de atributo (nombre de la plaga) y ausencia (NA).
		# En los casos en los que se presenten ambas categorías además del NA
		# correspondería trabajarlos como tres valores distintos (presencia,
		# ausencia y faltante) y su tratamiento dependerá de lo que se desee hacer
		
		# Para condiciones ifelse múltiples puedo usar la función case_when
	  edad_jefe_cate = case_when(edad_jefe < 18 ~ "Menor de Edad",
																 edad_jefe >= 18 ~ "Mayor de Edad",
	  ),
																 
																 
		
		# Recodifico las etiquetas de una variable categórica
		internet_banda_ancha = recode(internet_banda_ancha, 
		                 "Si inálambrico/satelital" = "Inalambrico o Satelital",
										 "No poseo internet de banda ancha" = "No",
										 "Si a través de cable (coaxial o ADSL)" = "Cable",
										 "Si a través de fibra óptica" = "Fibra Optica",
										 ),
		
		# Especifico ordinalidad a las categorías de una variable
		tiempo_residencia = factor(tiempo_residencia,
										levels = 1:5,
										labels = c("Menos de 2 años", "Entre 2 y 5 años",
																				 "Entre 5 y 10 años", "Entre 10 y 20 años",
																				 "20 años o más"))

	)

##########################################
# Seleccionar un subconjunto de columnas #
##########################################

# Opcion 1
datos_chico1 <- datos_limpios %>%
	select(   # Seleccionar las columnas que quiero conservar
		id, edad_jefe, certificado_renabap, celular_con_datos, plagas, cucarachas, ratas, mosquitos,
		internet_banda_ancha, tiempo_residencia
	)



###########################################
# Seleccionar un subconjunto de registros #
###########################################

# Opción 1: por criterio
#datos_reducido1 <-datos_orden %>%
#	filter((brotes > 4 & origen == "Nativo/Autóctono") | tiempo == "20 años o más")

# Opción 2: por indexación
#datos_reducido2 <-datos_orden %>%
#	slice(1:500)
