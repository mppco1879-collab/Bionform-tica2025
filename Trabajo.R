# NombreApellido_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
install.packages("readr") #para instalar el paquete 
library(readr)   #para cargar la libreria
install.packages("dplyr") #para lectura y manipulacion de datos
datos <- read_csv("datos_biomed.csv") #para leer el archivo CSV
head(datos) #para ver si podemos ver las primeras filas 

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
head(datos) #nos permite ver las filas 
summary(datos) #muestra estadisticas basicas de cada variable 
dim(datos) #filas y columnas 
str(datos) #muestra la estructura del dataframe 
#Encontramos con la función dim(), unas 100 filas y 5 columnas o variables. 
names(datos)
unique(datos$Tratamiento)  #nos permite ver los nombres de los tratamientos    
length(unique(datos$Tratamiento)) #nos permite ver cuantos tratamientos hay 
#Hay 3 tratamientos: FarmacoB, Placebo y FarmacoA 

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

library(tidyverse)
datos <- read_csv("datos_biomed.csv") #para importar el archivo y guardarlo en un objeto llamaddo datos 
datos_largo <- datos %>%
  pivot_longer(cols = c(Glucosa, Presion, Colesterol), #las columnas que queremos estirar hacia abajo
               names_to = "Variable",
               values_to = "Valor")
ggplot(datos_largo, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +#inicia gráfica con ggplot
  geom_boxplot() + #añade los boxplots a la gráfica 
  facet_wrap(~ Variable, scales = "free_y") +#creacion de varios paneles, cada uno para cada variables, como glucosa, presion y colesterol 
  theme_minimal() + #aplica estilo limpio 
  labs(
    title = "Boxplots por tratamiento",
    x = "Tratamiento",
    y = "Valor"
  ) 

# 4. Realiza un violin plot (investiga qué es). (1 pt)
install.packages("tidyverse") 
library(tidyverse)  #carga las librerias necesarias 
datos <- read_csv("datos_biomed.csv") #convierte las columnas en dos columnas nuevas
head(datos)
datos_largos <- datos %>%
  pivot_longer(
    cols = c(Glucosa, Presion, Colesterol),
    names_to = "Variable",
    values_to = "Valor"
  )
ggplot(datos_largos, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  #se encarga de dibujar la figura de los violines 
  geom_boxplot(width = 0.1, fill = "white") +  #agrega una caja dentro de cada violin
  facet_wrap(~ Variable, scales = "free_y") +  #crea 3 paneles para glucosa, presion y colesterol 
  labs(
    title = "Distribución de Glucosa, Presión y Colesterol según tratamiento",
    x = "Tratamiento",
    y = "Valor medido"
  ) +
  theme_minimal() + #estilo limpio y profesional 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  #rota las etiquetas del eje X y los nombres largos no se superponen 
    
  )


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

library(readr)
datos <- read_csv("datos_biomed.csv")
head(datos)

plot(datos$Glucosa, datos$Presion, #crea el grafico de dispersion 
     col = as.factor(datos$Tratamiento),   #asigna un color por cada tratamiento 
     pch = 19,                             #asigna puntos rellenos 
     xlab = "Glucosa (mg/dL)",            #xlab, ylab y main son etiquetas de los eje y titulo del grafico 
     ylab = "Presión (mmHg)",              
     main = "Relación Glucosa vs Presión según Tratamiento")  
legend("bottomright",                                # Posición: esquina inferior derecha
       legend = unique(datos$Tratamiento),            
       col = 1:length(unique(datos$Tratamiento)),     
       pch = 19,                                      
       title = "Tratamiento")   #encabezado a la leyenda 


# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
library(ggplot2)
library(readr)
datos <- read_csv("datos_biomed.csv")
ggplot(datos, aes(x = Colesterol, y = Presion, color = Tratamiento)) + #colorea los puntos según el tratamiento con color= tratamiento 
  geom_point(size = 2, alpha = 0.7) +   #puntos de dispersion 
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +   #dibuja los puntos del gráfico de dispersión 
  facet_grid(~ Tratamiento) +      #divide el gráfico en subgráficos (columna por tratamiento)       
  labs(
    title = "Relación entre Colesterol y Presión por Tratamiento",
    x = "Colesterol (mg/dL)",
    y = "Presión arterial (mmHg)"
  ) +
  theme_minimal() + #aplica un estilo limpio y moderno 
  theme(
    strip.text = element_text(size = 12, face = "bold"),  #cambia el tamaño y formato del texto de los nombres de los paneles 
    plot.title = element_text(hjust = 0.5)      #para centrar el titulo          
      )

# 7. Realiza un histogramas para cada variable. (0.5 pts)

library(tidyverse) #carga conjunto de paquetes diseñados para analisis de datos 
datos <- read_csv("datos_biomed.csv") #lee el archibo y lo guarda en un objeto llamado datos 
colores <- c(#crea un vector llamado colores y asocia cada variable a un color 
  "Glucosa" = "steelblue",
  "Presion" = "tomato",
  "Colesterol" = "seagreen"
)

datos_largo <- datos %>% #convertir el dataset a formato largo 
  pivot_longer(cols = c(Glucosa, Presion, Colesterol), #columnas que queremos convetir a largas
               names_to = "Variable", #nueva columna donde se guardará el nombre de la var
               values_to = "Valor") #nueva col donde se guardan los valores numericos 

ggplot(datos_largo, aes(x = Valor, fill = Variable)) + #incia una gráfica usando el dataset dato_largo
  geom_histogram(bins = 20, color = "black", alpha = 0.8) +#dibuja histograma que divide los datos en 20 barras, con borde negro en la barras y transparencia 80% 
  scale_fill_manual(values = colores) +#para los colores personalizados 
  facet_wrap(~ Variable, scales = "free") +#crea un panel para cada variable 
  theme_minimal() +#estilo minimalista
  labs(#agrega el titulo 
    title = "Histogramas por variable",
    x = "Valor",
    y = "Frecuencia"
  ) 
  
 
# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)

library(readr)
datos <- read_csv("datos_biomed.csv", col_names = FALSE) #extrae la primera fila que contiene los tratamientos de cada uno de los pacientes 
tratamientos <- as.character(datos[1, ])  # convierte los valores a caracteres 
tratamientos_factor <- factor(tratamientos, #crea un factor con los niveles especificados 
                              levels = c("Placebo", "Farmaco A", "Farmaco B"))
levels(tratamientos_factor) #muestra los niveles del factor 


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
library(readr)
datos <- read_csv("datos_biomed.csv") #para cargar el archivo CVS en dataframe datos 
#para calcular la media 
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean) #agrupar los datos por la columna tratamiento y se calcula la media de glucosa en cada grupo 
print(media_glucosa)
#para calcular la desviación estandar 
sd_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)#calcula la desviacion estandar 
print(sd_glucosa)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

library(dplyr)
library(readr) #para leer el archivo de datos_biomed
datos <- read_csv("datos_biomed.csv") #cargo los datos del archivo CSV en la variable datos 
#aqui se cargan las variables separadas por cada uno de los tratamientos 
placebo <- datos %>% filter(Tratamiento == "Placebo") #son las filas correspondientes a cada tratamiento 
farmacoA <- datos %>% filter(Tratamiento == "FarmacoA")
farmacoB <- datos %>% filter(Tratamiento == "FarmacoB")
#para verificar que todo esta bien, esto permite ver las primeras filas de cada dataset para comprobar que se extrjeron correctamente 
head(placebo)
head(farmacoA)
head(farmacoB)

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

library(tidyverse)
library(ggpubr)
#se hace para cargar los datos 
datos <- read_csv("datos_biomed.csv")
#las variables que necesitamos analizar 
variables <- c("Glucosa", "Presion", "Colesterol")
#funciones para evaluar la normalidad y comparativa de medias 
evaluar_variable <- function(var_name) {
  
  cat("\n==== Variable:", var_name, "====\n")
  
 #normalidad por cada uno de los tratamientos, shapiro-wilk test por grupo
  normalidad <- datos %>%
    group_by(Tratamiento) %>%
    summarise(shapiro_p = shapiro.test(get(var_name))$p.value) %>%
    ungroup()
  
  print(normalidad)
  
 #para ver si todos los grupos son normales o no 
  todos_normales <- all(normalidad$shapiro_p > 0.05)
  #comparamos medias entre los diferentes parametros 
  if(todos_normales) {
    cat("\nTodos los grupos normales → Realizando ANOVA\n")
    res <- aov(as.formula(paste(var_name, "~ Tratamiento")), data = datos)
    print(summary(res))
  } else {
    cat("\nAlgún grupo no normal → Realizando Kruskal-Wallis\n")
    res <- kruskal.test(as.formula(paste(var_name, "~ Tratamiento")), data = datos)
    print(res)
  }
  #para ver el histograma 
  ggplot(datos, aes_string(x = var_name)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
    geom_density(color = "red") +
    facet_wrap(~Tratamiento) +
    theme_minimal() +
    labs(title = paste("Distribución de", var_name, "por tratamiento"))
}
#aplicamos la funcion a todas las variables 
for (v in variables) {
  evaluar_variable(v)
}

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

library(tidyverse)
#para cargar los datos 
datos <- read_csv("datos_biomed.csv")
#para convertir tratamiento en factor si es que no lo es 
#as.factor asegura que R trate los tratamientos como categoria
datos$Tratamiento <- as.factor(datos$Tratamiento)
#se realiza ANOVA de glcuosa por cada tratamiento, comparando las medias de glucosa, placebo
y farmaco A y B 
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)
#muestra la tabla anova 
summary(anova_glucosa)
#se hace la prueba post-hov de Tukey para que pares de tratamientos tienen diferencias significativas en glucosa 
tukey_glucosa <- TukeyHSD(anova_glucosa)
tukey_glucosa