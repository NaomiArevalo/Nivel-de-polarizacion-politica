# Nivel de polarizacion politica

El objetivo de este trabajo es analizar el nivel de polarización política y las redes sociales, así como otros factores que podrían influir en el aumento de dicho fenómeno.

## Encuesta
https://docs.google.com/forms/d/e/1FAIpQLSfZObwaPmOcZU-9mE5oJXg7K7vU7cUr6A2FP39RPx5pS-4pxw/viewform?usp=dialog

## Resultados de la encuesta
[Polarizacion.xlsx](https://github.com/user-attachments/files/26759896/Polarizacion.xlsx)

## Código que usé para generar el trabajo

Primero cargué las librerías necesarias

library(dplyr)

library(ggplot2)

library(ggcorrplot)


# ----------------------------------------------------------
## Datos para la data.frame
# ----------------------------------------------------------

datos <- data.frame(
  Nivel_polarizacion = c(8,1,3,1,6,8,1,5,10,6,4,3,1,1,2,5,7,1,1,3,1,1,1,1,1,5,6,1,4,3,5,5,7,1,1),
  Tiempo_redes = c(6,3,5,3,6,8,9,6,3,3,2,10,4,6,3,2.5,6,6,2,2,1,4,4,4,5,3,1,2,0,6,2,5,1.5,2,10),
  Consumo_informacion = c(10,3,2,5,8,2,4,5,9,5,2,5,3,10,3,1,7,6,7,5,10,9,3,8,5,6,2,6,4,3,10,10,3,1,7),
  Confianza_medios = c(2,5,3,4,3,1,1,4,6,3,1,7,6,1,5,4,5,6,3,6,2,5,3,6,7,8,1,1,3,4,8,1,3,4,3),
  Ubic_ideologica = c(1,4,3,4,7,7,8,8,10,4,3,5,3,10,2,5,1,6,5,10,8,5,7,5,5,5,4,5,5,5,2,1,2,5,5),
  Genero = c(1,1,2,1,2,2,1,2,1,1,1,1,1,1,1,1,1,1,2,1,2,1,1,1,1,1,1,2,1,2,3,2,1,2,1), 
  Importancia_politica = c(10,5,1,9,5,4,4,7,5,8,3,5,4,10,6,4,10,8,7,8,10,7,5,9,5,6,9,3,5,8,10,10,10,7,9),
  Diversidad_informacion = c(9,4,1,6,1,5,8,3,10,5,1,5,5,10,3,5,6,6,4,5,1,8,7,4,5,7,6,5,1,5,4,1,6,4,5),
  Refuerzo_redes = c(10,3,2,3,6,7,1,5,8,5,1,3,9,10,7,8,7,10,1,5,1,8,6,2,10,7,9,4,4,7,10,5,6,1,6),
  Educacion = c(3,3,3,3,3,4,3,3,6,3,5,4,3,5,5,5,3,3,4,5,5,3,3,5,5,3,5,2,5,5,5,5,5,4,5)
)

# ----------------------------------------------------------
## Limpieza de datos
# ----------------------------------------------------------

### Excluí la variable del genero porque es categórico 

datos_clean <- datos %>%
  select(Nivel_polarizacion, Tiempo_redes, Consumo_informacion, 
         Confianza_medios, Ubic_ideologica, Importancia_politica, 
         Diversidad_informacion, Refuerzo_redes, Educacion) %>%
  filter(complete.cases(.))  

### Ver datos limpios

glimpse(datos_clean)

# ----------------------------------------------------------
## Matriz de correlación
# ----------------------------------------------------------

## Calculamos correlación de Pearson

corr_polarizacion <- cor(datos_clean, use = "pairwise.complete.obs") %>% round(2)
corr_polarizacion

## Visualización con ggcorrplot 
ggcorrplot(corr_polarizacion, type = "lower", lab = TRUE, show.legend = TRUE) +
  ggtitle("Matriz de correlación - Polarización política y hábitos de consumo de medios") +
  theme_minimal()

# ----------------------------------------------------------
## Gráfico de dispersión: 
# ----------------------------------------------------------

### Relación entre Nivel de polarización y Tiempo en redes sociales


### Correlación de Pearson entre estas dos variables 
cor(datos_clean$Nivel_polarizacion, datos_clean$Tiempo_redes, method = "pearson")

### Gráfico de dispersión
grafica <- ggplot(datos_clean, aes(x = Tiempo_redes, y = Nivel_polarizacion)) +
  geom_point(size = 3, alpha = 0.7, color = "darkblue") + 
  geom_smooth(method = lm, se = TRUE, color = "red") +
  labs(title = "Relación entre tiempo en redes sociales y polarización política",
       subtitle = paste("Correlación de Pearson =", 
                        round(cor(datos_clean$Nivel_polarizacion, datos_clean$Tiempo_redes), 2)),
       x = "Tiempo en redes sociales (horas/día)", 
       y = "Nivel de polarización (1-10)",
       caption = "Fuente: Encuesta propia") +
  theme_minimal()

# Ver la grafica
grafica

# Regresion lineal

modelo <- lm(Nivel_polarizacion ~ Tiempo_redes + Confianza_medios + 
               Consumo_informacion + Ubic_ideologica, data = datos_clean)
 
# Modelo

summary(modelo)

# Resultados del código

# Matriz de correlación

<img width="903" height="257" alt="Correlación" src="https://github.com/user-attachments/assets/49479d30-d1f7-4c04-af61-016bf5141996" />

<img width="864" height="453" alt="Correlación" src="https://github.com/user-attachments/assets/da96babe-2975-4d6d-9419-7ed066403e53" />

La gráfica nos permite ver que existe una correlación moderada positiva entre el nivel de polarización y el refuerzo en redes sociales, por lo que, las personas que reciben más refuerzo de sus ideas en redes sociales tienden a tener mayor nivel de polarización. Mientras que, la variable dependiente mantiene una correlación débil o casi nula con las demás variables.

# Gráfica del comportamiento de la variable dependiente "nivel de polarización" frente a una independiente "tiempo en redes"

<img width="864" height="453" alt="Relacion" src="https://github.com/user-attachments/assets/f9260d23-9d66-48b4-92b6-dc69768d0320" />

La gráfica nos muesta que existe una relación débil negativa (casi nula) de el nivel de polarización y el tiempo que las personas le dedican a las redes sociales.

# Regresión lineal

<img width="976" height="336" alt="regresion" src="https://github.com/user-attachments/assets/67afcb16-5617-406f-bef1-4addceef78e8" />

El modelo de regresión nos permite ver que ninguna de las variables alcanzó la significancia estadística (p < 0.05), incluyendo la variable de refuerzo de redes que mantenía una correlación moderada con la variable dependiente. Además, podemos observar un p-value de	0.671	lo cual nos remite a afirmar que el modelo no es significativo para predecir los niveles de polarización con esta muestra.

En conclusión en esta muestra, no se encontró evidencia estadística que respalde que las variables analizadas predigan significativamente el nivel de polarización política. 
