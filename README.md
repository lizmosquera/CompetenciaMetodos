# CompetenciaMetodos

Archivos de la competencia  
https://www.overleaf.com/9659178318kztvcxbpthcc Archivo de Overleaf

Integrantes:<br/>
 Javier Arturo Rozo Alzate - jarozoa@eafit.edu.co<br/>
 Alejandro Palacio Vásquez - apalac19@eafit.edu.co <br/>
 Liceth Cristina Mosquera Galvis - lcmosquerg@eafit.edu.co <br/>
 Cristian David Muñoz Mora - cdmunozm@eafit.edu.co <br/>
 Programa: Métodos Estadísticos Avanzados<br/>
 Docente:  Andrés Ramirez Hassan - aramir21@eafit.edu.co<br/>

## Resumen:
En este documento se presenta la metodología utilizada para abordar el problema de selección de variables y predicción planteado en la materia, para 3 bases de datos diferentes. Se inicia con un análisis exploratorio de la información, para posteriormente realizar con diferentes modelos estadísticos la selección de las variables explicativas más significativas y finalmente proceder a evaluar los resultados de predicción.

Palabras Clave: Selección de variables, Ridge, BMA, Lasso, Elastic Net.<br/>

## Introducción:
En estadística, los métodos de regularización son utilizados para la selección del modelo y para evitar el sobreajuste en las técnicas predictivas. Por ende, al abordar el trabajo, se quiere ofrecer una revisión general de la metodología y diferentes fases del proceso de selección de variables de una base de datos con alta dimensionalidad, así como de los criterios de selección y descripción de las diferentes técnicas que pueden utilizarse en la investigación de carácter aplicativo a la ciencias de los datos con diferentes metodologías y con el uso apropiado de las técnicas estadísticas, que ha de ser acorde con el tipo de información disponible.

Se usan diferentes metodologías robustas tanto bayesianas como frecuentistas para la correcta selección de variables, ya que es necesario seleccionar las mejores variables predictivas o registros auxiliares, también llamados regresores. Y, de esta forma, seleccionar la mejor ecuación de regresión de entre todas las posibles combinaciones. Donde al final, lo que se quiere es crear el modelo más simple e interpretable posible.
 
Uno de los objetivos es obtener un modelo parsimonioso, es decir, ajustar bien los datos a la variable de respuesta, pero usando la menor cantidad posible de variables explicativas o de regresores. Donde, la selección de variables y multicolinealidad son dos problemas que se pueden tratar de manera simultánea. Bajo este escenario se ubica el objetivo central de este trabajo.

## Descripción del Problema:
El objetivo de la investigación es estudiar y determinar las influencias significativas que ejercen las distintas variables, en correspondencia con el rendimiento final, determinando esencialmente la cantidad de variables que logren, de manera óptima, predecir el comportamiento de la variable respuesta. Donde, lo que se quiere al final, es analizar la capacidad predictiva general de los tres modelos con respecto a cada una de las bases de datos. En la base de datos continua y conteo, se medirá de acuerdo con el error cuadrático medio y para la base de datos binaria, se medirá de acuerdo de acuerdo con el Accuracy de los resultados (verdaderos positivos más verdaderos negativos dividido el tamaño de la muestra). La capacidad predictiva específica para la base de datos Continua será la correcta clasificación de los valores inferiores y superiores a -1, para la base de datos Binaria el área bajo la curva ROC y para la base de datos de Conteo, será la correcta clasificación de los valores iguales a 0 y mayores a 0. Para la correcta selección de variables, se medirá de acuerdo con el modelo ya establecido por el profesor y que tan similar es el modelo seleccionado con el establecido.
