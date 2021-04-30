# *********************************************** *****************
# Paso 0. Descargar y descomprimir el conjunto de datos
# *********************************************** ******************
  
  if(!file.exists("./data")){dir.create("./data")}
#Here are the data for the project:
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Debe crear un script de R llamado run_analysis.R que haga lo siguiente.

# *********************************************** *****************
# Paso 1. Fusiona los conjuntos de entrenamiento y prueba para crear un conjunto de datos.
# *********************************************** *****************

# # 1.1 Leyendo archivos

# 1.1.1 Lectura de tablas de entrenamientos:

x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Lectura de tablas de prueba:
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Vector de características de lectura:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# 1.1.4 Leer etiquetas de actividades:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

# 1.2 Asignación de nombres de columna:

colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

# 1.3 Fusionando todos los datos en un conjunto:

mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
setAllInOne <- rbind(mrg_train, mrg_test)

#dim(setAllInOne)
#[1] 10299   563

#******************************************************************
# Paso 2.-Extrae solo las medidas sobre la media y la desviación estándar de cada medida.
#******************************************************************

# 2.1 Lectura de nombres de columnas:

colNames <- colnames(setAllInOne)

# 2.2 Crear vector para definir ID, media y desviación estándar:

mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

# 2.3 Haciendo un subconjunto necesario a partir de setAllInOne:

setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

#******************************************************************
# Paso 3. Utiliza nombres de actividades descriptivos para nombrar las actividades en el conjunto de datos
#******************************************************************

setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)

#******************************************************************
# Paso 4. Etiqueta apropiadamente el conjunto de datos con nombres de variables descriptivos.
#******************************************************************

#Done in previous steps, see 1.3,2.2 and 2.3!

#******************************************************************
# Paso 5. A partir del conjunto de datos del paso 4, crea un segundo conjunto de datos ordenado e independiente con el promedio de cada variable para cada actividad y cada tema.
# *********************************************** *****************

# 5.1 Hacer un segundo conjunto de datos ordenado

secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

# 5.2 Escribiendo un segundo conjunto de datos ordenado en un archivo txt

write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
