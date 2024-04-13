# sisalril_siniestralidad_prestaciones

Esto es script el cual crea un reporte en `shiny` con R de la información que se encuentra en la SISALRIL con respecto al apartado de Estadisticas/Series Históricas SFS, donde se selecciona la Siniestralidad y Prestaciones. Posterior a un proceso realizado de WebScrapping realizado en Python consultar los reportes, procesarlos y limpiarlos.

Para realizar la visualización de los reportes a través de R-Studio, deberán tener instalado el paquete correspondiente de `shiny`, y realizar la siguiente consulta con la función `runGitHub()`, para poder visualizar el Dashboard interactivo de shiny directamente desde R sin descargar nada.

```r
library(shiny)

runGitHub(repo = "sisalril_siniestralidad_prestaciones",username = "FerryDareon",ref = "main")
```
Aquí tienes una versión mejorada del texto para el repositorio de GitHub:

---

El archivo de Jupyter Notebook detalla el proceso de extracción de datos en vivo utilizando Python y la biblioteca `selenium`. Este proceso permite realizar Web Scraping en la página de SISALRIL, específicamente en la sección de informaciones/estadísticas. El objetivo es obtener las URL de cada archivo ubicado en las carpetas de Siniestralidad, Prestaciones, Financiamiento y Afiliación, identificando el régimen de financiamiento a partir de las abreviaturas de cada informe, así como los códigos que varían según cómo se registra la información en el documento.

A modo de ejemplo, para el caso de la siniestralidad, una vez obtenidos los enlaces de cada archivo, se realizó lo siguiente:

```python
# Descarga y procesamiento de datos de siniestralidad
dataframe_siniestralidad = pd.DataFrame()

time.sleep(1)

for index, row in document_table[(document_table["periocidad_code"] == "01") & (document_table["file_name"] == "Siniestralidad")].iterrows():
    siniestralidad = pd.read_excel(requests.get(f'{row["document_link"]}').content, sheet_name=None)
    siniestralidad = siniestralidad[f"{list(siniestralidad.keys())[0]}"].replace("/2", "", regex=True)
    siniestralidad.columns = siniestralidad.iloc[7]
    siniestralidad = siniestralidad.rename(columns=lambda x: x.strip())

    siniestralidad = siniestralidad[~siniestralidad["Gasto en Salud"].isna()].drop(index=7).reset_index().drop(columns=['index'])

    siniestralidad["Ingresos en Salud"] = siniestralidad["Ingresos en Salud"].astype("float")
    siniestralidad["Gasto en Salud"] = siniestralidad["Gasto en Salud"].astype("float")

    siniestralidad["Regimen_abv"] = f"{row['regimen_abv']}"
    siniestralidad["Document_name"] = f"{row['document_name']}"

    dataframe_siniestralidad = pd.concat([dataframe_siniestralidad, siniestralidad])

dataframe_siniestralidad = dataframe_siniestralidad.reset_index().drop(columns="index")

dataframe_siniestralidad.head()
```

Este proceso implica:

1. Filtrar el `document_table` para seleccionar los informes de siniestralidad mensual, identificados con el código "01" en la columna `periocidad_code`, y con el nombre de archivo "Siniestralidad".
2. Utilizar un bucle `for` para leer cada enlace de los informes y realizar el procesamiento correspondiente.
3. Consultar el primer hoja de cada documento y reemplazar las instancias de "/2" por una cadena vacía para homogeneizar los encabezados de las columnas.
4. Seleccionar los datos relevantes, como los ingresos y gastos en salud, y eliminar las filas innecesarias.
5. Convertir los datos de ingresos y gastos en salud al tipo de dato float.
6. Agregar columnas adicionales al DataFrame resultante, como "Regimen_abv" (abreviatura del régimen de financiamiento) y "Document_name" (nombre del documento).
7. Concatenar los datos de cada informe y restablecer el índice del DataFrame resultante.

--- 

Este texto proporciona una descripción detallada del proceso de extracción de datos, incluyendo los pasos específicos y el razonamiento detrás de ellos.
