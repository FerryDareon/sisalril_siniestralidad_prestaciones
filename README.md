# sisalril_siniestralidad_prestaciones

Un script para visualizar los datos de siniestralidad y prestaciones de la SISALRIL

Esto es script el cual crea un reporte en shiny con R de la información que se encuentra en la SISALRIL con respecto al apartado de Estadisticas/Series Históricas SFS, donde se selecciona la Siniestralidad y Prestaciones. Posterior a un proceso realizado de WebScrapping realizado en Python consultar los reportes, procesarlos y limpiarlos.
Para realizar la visualización de los reportes a través de R-Studio, deberán tener instalado el paquete correspondiente de shiny, y realizar la siguiente consulta con la función runGitHub(), para poder visualizar el Dashboard interactivo de shiny directamente desde R sin descargar nada.

```r
library(shiny)

runGitHub(repo = "sisalril_siniestralidad_prestaciones",username = "FerryDareon",ref = "main")
```
