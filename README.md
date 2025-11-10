# Laboratorio No. 12 - Aprendiendo Haskell y Cálculo Lambda

## 1. Visión General

Este proyecto documenta el proceso de aprendizaje de la programación funcional con Haskell y los fundamentos del cálculo lambda. A través de la resolución de cuatro problemas específicos de un laboratorio, se exploran conceptos clave de Haskell como tipos de datos algebraicos, funciones de orden superior, pattern matching, currying y manejo de I/O.

[Video Demostrativo](https://www.loom.com/share/60e90f3535294e948ec79874112aca4d)

## 2. Cómo fue codificado

La implementación inicial y la experimentación con el código Haskell se realizaron en un entorno de playground oficial de Haskell. Para los ajustes finales, la verificación de la salida y la interactividad con el usuario, se utilizó Replit, que proporcionó un entorno de terminal completo para ejecutar y probar los programas.

## 3. Cómo ejecutarlo

Para ejecutar el código de este proyecto, tienes dos opciones principales:

*   **Si tienes Haskell instalado localmente (GHC):**
    1.  Guarda el código de cada problema (por ejemplo, `problem1_solution.hs`) en un archivo `.hs`.
    2.  Abre tu terminal y navega hasta el directorio donde guardaste el archivo.
    3.  Compila el código con `ghc --make <nombre_del_archivo>.hs`.
    4.  Ejecuta el programa compilado con `./<nombre_del_archivo>`.
    5.  Alternativamente, puedes cargar el archivo en GHCi (el intérprete interactivo de Haskell) con `ghci <nombre_del_archivo>.hs` y luego llamar a la función `main`.

*   **Usando Replit (o un playground online interactivo):**
    1.  Copia el código de un problema específico (por ejemplo, el contenido de `problem1_solution.hs`).
    2.  Pega el código en un nuevo "Haskell Repl" en Replit.
    3.  Haz clic en el botón "Run". Si el problema requiere entrada del usuario, Replit te permitirá interactuar con el programa en su consola.

## 4. Uso de IA

Este proyecto fue desarrollado con la asistencia de **Gemini CLI**, un asistente de IA. Gemini CLI fue utilizado para:
*   Aclarar conceptos de programación funcional y Haskell.
*   Guiar en la resolución de los problemas, ofreciendo explicaciones paso a paso y sugerencias.
*   Proporcionar ejemplos de código y depuración.
*   Explicar convenciones de Haskell y el sistema de tipos.

El archivo `GEMINI.md` sirvió como un archivo de contexto para el asistente, detallando los objetivos del proyecto, las expectativas de respuesta y las reglas de codificación. Un resumen detallado de las explicaciones y el razonamiento detrás de cada solución se puede encontrar en los archivos `problemX_explanation.md` correspondientes a cada problema.

## 5. Estructura del Proyecto

El proyecto está organizado de la siguiente manera:

*   `GEMINI.md`: Archivo de contexto utilizado por el asistente de IA.
*   `Laboratorio No 12.pdf`: El documento original del laboratorio con los problemas a resolver.
*   `problemX_explanation.md`: Archivos que contienen explicaciones detalladas de los conceptos y la lógica utilizada para resolver cada `Problema X`.
*   `problemX_solution.hs`: Archivos que contienen las soluciones de código Haskell para cada `Problema X`.
*   `explanations.md`: Este archivo fue utilizado inicialmente para experimentar y comprender los conceptos básicos de Haskell antes de abordar los problemas específicos.

## 6. Agradecimientos

¡Muchas gracias por toda la ayuda y guía proporcionada! Ha sido una experiencia de aprendizaje increíble y un excelente cierre para el último trabajo de la clase. ¡Éxitos!
