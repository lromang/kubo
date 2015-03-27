#------------------------------------------------------------
                                        # Luis Manuel Román García
                                        # Código para procesar datos y hacer análisis exploratorio. con respecto a la base de datos "tuberías"
#------------------------------------------------------------
                                        # Librerías utilizadas
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(Hmisc)
                                        #------------------------------------------------------------
                                        # Lectura de datos
input.data <- read.csv("../../Datos/tuberias.csv",
                       stringsAsFactors = FALSE)
                                        #------------------------------------------------------------
                                        # Procesamiento y visualización de datos
                                        # Procesamiento inicial, sólo encontrar características generales de la población.
                                        # Para mayor claridad, dividamos las edades en quintiles.
quint.edad <- cut2(extract_numeric(na.omit(input.data$edad)),g = 5)
                                        # Distribución sexos
plyr::count(input.data$sexo)
                                        # La distribución de sexos es práctimente la misma con una ligera mayor presencia de hombres.
                                        # Distribución nivel de estudios
plyr::count(input.data$nivel_estudios)
                                        # Es interesante notar el alto número de clientes con nivel de estudios universitarios
                                        # Distribución de clientes con respecto a estado civil.
plyr::count(input.data$estado_civil)

                                        # Histograma de clientes por edad, sexo, nivel de estudios. 
hist.pob.estudios <-
    ggplot(data = input.data,
           aes(x = quint.edad, fill = nivel_estudios)) +
        geom_histogram(position = "dodge") +
            facet_wrap(~sexo) + xlab("Quintiles edad") + ylab("Observaciones")
png("../presentation/images/hist_pob_estudios.png",width=850,height=500)
hist.pob.estudios
dev.off()
                                        # Histograma de clientes por edad, sexo, estado civil.
hist.pob.civil <-
    ggplot(data = input.data,
           aes(x = quint.edad, fill = estado_civil)) +
        geom_histogram(position ="dodge") +
            facet_wrap(~sexo) + xlab("Quintiles edad") + ylab("Observaciones")
hist.pob.civil
png("../presentation/images/hist_pob_civil.png",width=850,height=500)
hist.pob.civil
dev.off()
                                        # Distribución de clientes actividad laboral
plyr::count(input.data$es_comerciante)
plyr::count(input.data$es_empleado)
plyr::count(input.data$actividad_economica_emp)
                                        # Distribución de clientes activos
plyr::count(input.data$activo)
plyr::count(input.data$intentos)
                                        # Distribución de clientes score crediticio
plyr::count(input.data$bc_score)
plyr::count(input.data$kubo_score)
plyr::count(input.data$icc)
                                        # Distribución de clientes tipo de prestamo
plyr::count(input.data$estatus)
plyr::count(input.data$tipo_credito)
plyr::count(input.data$emproblemado)
                                        # Distribución tipos de cuenta
summary(extract_numeric(input.data$monto_actual))
plyr::count(input.data$producto)
                                        # Distribución monto credito
summary(extract_numeric(input.data$monto_credito))
                                        # Duración de crédito
#input.data$fecha_inicio
#input.data$fecha_vencimiento
                                        # tasa y frecuencia
summary(extract_numeric(input.data$tasa))
plyr::count(input.data$frecuencia)
                                        # Estudiar interacción entre tasa, duración de credito, número de cuotas estatus de crédito
plyr::count(input.data$dias_atraso > 0)
