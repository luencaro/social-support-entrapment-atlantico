# Apoyo Social y Entrapment en Adolescentes Escolarizados del Atlántico

Proyecto de Seminario Investigativo: *"El género en la relación entre apoyo social y
entrapment como moderador en adolescentes escolarizados de municipios del
Atlántico"*, enmarcado en el modelo Motivacional-Volitivo Integrado (IMV).

Autores: Natalia Alvarado y Luis Cabarcas.

Este repositorio contiene el análisis reproducible del proyecto, escrito como un
libro [bookdown](https://bookdown.org/) que se publica en GitHub Pages.

## Estructura del repositorio

```
index.Rmd              # portada / setup del bookdown
01-introduccion.Rmd     # capítulo 1: contexto y descripción de los datos
02-eda.Rmd              # capítulo 2: Análisis Exploratorio de Datos (EDA)
R/_common.R             # funciones y setup compartido (carga de datos, gráficos, tablas)
data/                   # datos del proyecto (no versionados, ver más abajo)
docs/                   # salida renderizada del libro (lo que sirve GitHub Pages)
renv.lock               # versiones exactas de los paquetes de R usados
```

## Cómo empezar

### 1. Requisitos

- [R](https://cran.r-project.org/) 4.3+
- [Pandoc](https://pandoc.org/) (RStudio ya lo incluye; si usas R por fuera de RStudio,
  instálalo aparte)
- El paquete [`renv`](https://rstudio.github.io/renv/) (se activa solo al abrir el
  proyecto gracias al `.Rprofile`)

### 2. Clonar e instalar dependencias

```bash
git clone https://github.com/luencaro/social-support-entrapment-atlantico.git
cd social-support-entrapment-atlantico
```

Abre `social-support-entrapment-atlantico.Rproj` en RStudio (o inicia R en esa
carpeta). `renv` se activará automáticamente; restaura las dependencias con:

```r
renv::restore()
```

### 3. Obtener los datos

El archivo de datos **no está en el repositorio** (ni su nombre real aparece en el
código). Pide el archivo a los autores del proyecto, colócalo dentro de `data/`, y
luego indícale a R dónde está mediante una variable de entorno:

```bash
cp .Renviron.example .Renviron
```

Edita `.Renviron` y completa la ruta real, por ejemplo:

```
DATA_PATH=data/nombre_real_del_archivo.csv
```

`.Renviron` está en `.gitignore`, así que esta ruta se queda local a tu máquina y
nunca se sube al repositorio. Reinicia la sesión de R después de crearlo/editarlo
para que la variable quede disponible.

### 4. Renderizar el libro

```r
bookdown::render_book("index.Rmd")
```

Esto genera el HTML en `docs/`. Para previsualizar con recarga automática mientras
editas:

```r
bookdown::serve_book()
```

Cada capítulo (`01-introduccion.Rmd`, `02-eda.Rmd`, ...) también se puede correr o
"Knittear" por separado en RStudio sin renderizar todo el libro — el primer chunk de
cada uno carga `R/_common.R` automáticamente si hace falta. Para generar el HTML de
un solo capítulo dentro del sitio (en vez de `rmarkdown::render()`, que rompe la
estructura porque el proyecto usa `site: bookdown::bookdown_site`), usa:

```r
bookdown::preview_chapter("02-eda.Rmd")
```

También puedes servir la carpeta `docs/` ya generada con un servidor simple:

```bash
cd docs && python3 -m http.server 4321
```

y abrir `http://127.0.0.1:4321/index.html`.

## Flujo de trabajo con `renv`

Cada vez que instales o actualices un paquete nuevo, corre `renv::snapshot()` antes
de hacer commit para mantener `renv.lock` sincronizado. Así, sin importar si estás
en Linux o Windows, `renv::restore()` deja a cualquiera con las mismas versiones.

## Publicación (GitHub Pages)

El libro se publica desde la carpeta `docs/` de la rama `main`. Después de
renderizar y hacer commit/push de los cambios en `docs/`, GitHub Pages actualiza el
sitio automáticamente.
