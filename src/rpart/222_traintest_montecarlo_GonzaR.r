rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c(999931, 999953, 999959, 999961, 999979, 999983) #reemplazar por las propias semillas

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(70,30), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  que es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/usuario/Desktop/CURSOS_VARIOS/2_Maestría_Datos_UTN/2 AÑO/2-AMD-EcoFin")  #Establezco el Working Directory
#cargo los datos

dataset  <- fread("datasets/paquete_premium_202011.csv")

vector_maxdepth = c(4,5,6) #3
vector_cp = c(-0.0001) #-1
#vector_minsplit = c(10,20,50,100,200,300,500,1000,2000,5000,10000,15000,20000)
#vector_minsplit = c(500,600,700,800,900,1000,1200,1500,1800,2000)
#vector_minsplit = c(800,850,900,950,1000,1050,1100,1150,1200)
#vector_minsplit = c(800,830,850,870,900)
vector_minsplit = c(850)
for (m_split in vector_minsplit)
{
  vector_minbuck = seq(5, round(m_split/2), by = round(m_split/(2*10)))#round(m_split/(2*10))
  for (m_buck in vector_minbuck)
  {
    for (complejidad in vector_cp)
    {
      for (profundidad in vector_maxdepth)
      {
        param_basicos  <- list( "cp"=        complejidad,  #complejidad minima
                                "minsplit"=  m_split,  #minima cantidad de registros en un nodo para hacer el split
                                "minbucket"= m_buck,  #minima cantidad de registros en una hoja
                                "maxdepth"=  profundidad ) #profundidad mÃ¡xima del arbol
        #minbuck <= minsplit /2
        #la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
        ganancias  <- mcmapply( ArbolEstimarGanancia, 
                                ksemillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                                MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                                SIMPLIFY= FALSE,
                                mc.cores= 1 )  #se puede subir a 5 si posee Linux o Mac OS
        
        #muestro la lista de las ganancias en testing para la particion realizada con cada semilla
        ganancias
        
        #paso la lista a vector
        unlist(ganancias)
        
        #finalmente calculo la media (promedio)  de las ganancias
        ganancia_final <- mean(unlist(ganancias))
        
        cat(param_basicos$cp,
            param_basicos$minsplit,
            param_basicos$minbucket,
            param_basicos$maxdepth,
            ganancia_final,'\n', file ='texto850_cp.txt',append = TRUE)
      }  
    }
  }
}