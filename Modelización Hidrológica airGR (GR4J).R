# MODELIZACIÓN HIDROLÓGICA CON LA LIBRERÍAAIRGR============================================================================================

# 1. INGRESO DE DATOS====================
# No olvidemos configurar el directorio de trabajo!
setwd("C:/Users/monte/Documents/EVENTOS/DATA SCIENCE UNI FIC 2019/Workspace_UNI_2019") 

#Instalamos la librería airGR
install.packages("airGR")
library(airGR)
library(lubridate)


# Cargamos el archivo con datos de variables hidrometeorológicas
BasinObs<-read.csv("C:/Users/monte/Documents/WASSER WORLD/ACADÉMICO/WEBINARS/airGR-Agosto 2019/estación.csv")
data(L0123001)
BasinObs$DatesR<-as.POSIXlt(BasinObs$DatesR, format = "%YYY-%mm-%dd")

# Descripción de variables
# DatesR: Fechas en formato POSIXt
# P: Precipitación media diaria [mm/día]
# T: Temperatura media del aire [???]
# E: Evapotranspiración potencial media de la cuenca [mm/día]
# Qls: Caudal a la salida de la cuenca [l/s]
# Qmm: caudal a la salida de la cuenca (lámina en mm) [mm/day]

# 2. GENERACIÓN DE VECTORES DE VARIABLES HIDROMETEOROLÓGICAS====================
# No están permitidos los vacíos (NA)
# Creamos la variable para el input del modelo
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)
str(InputsModel)


# 3. CONDICIONES INICIALES DE SIMULACIÓN====================
# La función CreateRunOptions permite preparar las opciones requeridas para las funciones 
# "RunModel", que son las funciones de los modelos reales.
# FUN_MOD: nombre de la función a ejecutar
# InputsModel: Los datos de ingreso
# IndPeriod_Run: El período de simulación

# Ahora seleccionamos un período para el cual el usuario desea ejecutar el modelo, 
# También seleccionamos los índices correspondientes para diferentes períodos de tiempo 
# (no las fechas POSIXt)
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1999-01-01"), 
               which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1999-12-31"))
str(Ind_Run)

# La inicialización de los modelos hidrológicos es de suma importancia
# Una inicialización imprecisa provoca simulaciones de descarga de baja calidad durante 
# las primeras etapas del período de ejecución.
# En el caso de los modelos GR, por defecto los niveles de almacenamiento de producción 
# y de tránsito están configurados respectivamente en 30% y 50% de su capacidad, 
# lo cual podría estar lejos de su valor ideal. 
# Se ofrecen dos soluciones para inicializar con precisión  los modelos GR en airGR: 
# 1. predefinir manualmente los estados iniciales (por ejemplo, de una ejecución anterior) 
# 2. Ejecutar los modelos durante un período de calentamiento (warm up period) anterior al período de ejecución real. 
# Se recomienda configurar este período de calentamiento para que sea igual o superior a un año.

# Veamos los significados
# IniStates: Los estados iniciales de 2 hidrogramas unitarios (20 + 40 = 60 unidades)
# IniResLevels: Los niveles iniciales de los reservorios virtuales de producción y tránsito
# IndPeriod_WarmUp: Período de calentamiento para iniciar el modelo, con el mismo formato que "IndPeriod_Run"

RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
                               IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = NULL)

str(RunOptions)


# 4. FUNCIÓN PARA OBTENER CRITERIOS ESTADÍSTICOS====================
# La función CreateInputsCrit () permite preparar la entrada para calcular un criterio estadístico 
# Es posible definir los siguientes argumentos:
# FUN_CRIT: el nombre de la función de criterio estadístico de error (las funciones disponibles se presentan más adelante)
# InputsModel: las entradas del modelo hidrológico previamente preparado por la función "CreateInputsModel"
# RunOptions: las opciones del modelo hidrológico previamente preparado por la función "CreateRunOptions"
# VarObs: el nombre de la variable considerada (por defecto "Q" para la descarga)
# Obs: la serie de tiempo variable observada (por ejemplo, la descarga expresada en mm/paso de tiempo)
# Aquí si están permitidos los vacíos 

InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                               RunOptions = RunOptions, VarObs = "Q", Obs = BasinObs$Qmm[Ind_Run])
str(InputsCrit)

# La evaluación de la calidad de una simulación se estima mediante el cálculo de criterios 
# Como una medida para evaluar su desempeño en un período de control.
# ErrorCrit_RMSE (): error cuadrático medio (RMSE)
# ErrorCrit_NSE (): coeficiente de eficiencia del modelo Nash-Sutcliffe (NSE)
# ErrorCrit_KGE (): criterio de eficiencia King-Gupta (KGE)
# ErrorCrit_KGE2 (): criterio de eficiencia King-Gupta modificado (KGE ')
# También puedes crear tu propio criterio estadístico!, solo sigue la misma sintaxis de las funciones anteriores.

CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)
str(CalibOptions)

# 5. CALIBRACIÓN DEL MODELO====================
# El objetivo del algoritmo de calibración es identificar los parámetros del modelo 
# al comparar los resultadoscon los datos observados. El algoritmo determina 
# la combinación de parámetros que representa el mejor comportamiento de la cuenca.
# El algoritmo de calibración optimiza el criterio de error seleccionado como función objetivo. 
# Este algoritmo funciona en dos pasos:
# a) Se realiza una selección del espacio de parámetros utilizando una grilla predefinida aproximada 
# o una lista de conjuntos de parámetros definidos por el usuario.
# b) Se realiza un simple algoritmo de búsqueda local de descenso más pronunciado a partir del 
# mejor conjunto de parámetros encontrado en el primer paso

OutputsCalib <- Calibration_Michel(InputsModel = InputsModel, RunOptions = RunOptions,
                                   InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                   FUN_MOD = RunModel_GR4J)

# Obtenemos los 4 parámetros del modelo GR4J
Param <- OutputsCalib$ParamFinalR
Param

# 6. CONTROL DE SIMULACIÓN====================
# Ahora evaluaremos la capacidad predictiva del modelo. El control se define como la estimación 
# de la precisión del modelo en conjuntos de datos que no se utilizan en su construcción,
# y en particular su calibración.
# La forma clásica de realizar un control es mantener los datos de un período separados del período de calibración. 
# TENER EN CUENTA!: Si es posible, este período de control debe corresponder a situaciones climáticas 
# que difieren de las del período de calibración para señalar mejor las cualidades y debilidades del modelo. 
# Con todo ello es necesario para evaluar  la robustez del modelo, es decir, su capacidad para mantener rendimientos 
# estables fuera de las condiciones de calibración.


# 7. SIMULACIÓN====================
# Todos los datos necesarios ya se han preparado en los pasos anteriores definidos en esta guía.
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)
str(OutputsModel)

# 8. VISUALIZACIÓN DE RESULTADOS====================
# Con la ayuda de la función plot () generamos un panel de resultados que incluye 
# varios gráficos (según el modelo utilizado)

plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])


# 9. CRITERIO DE EFICIENCIA DEL MODELO====================
# Es posible usar el mismo criterio definido en el paso de calibración o usar otro criterio.

# Nash-Sutcliffe
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
str(OutputsCrit)

# King-Gupta (KGE)
OutputsCrit <- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
str(OutputsCrit)
