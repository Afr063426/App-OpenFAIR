# Aplicación Shiny para Análisis Cuantitativo de Riesgos (TFM)

Esta aplicación permite ajustar distribuciones de probabilidad de pérdida a partir de datos históricos en Excel y correr simulaciones Monte Carlo mediante la metodología Open FAIR™ utilizando el paquete `evaluator`.

## 🚀 Instrucciones de Ejecución (Para Evaluadores / Profesores)

**No requiere tener R ni RStudio instalado.** Solo necesita tener instalado [Docker Desktop](https://www.docker.com/products/docker-desktop/).

### Pasos:

1. Descomprima este archivo `.zip` en cualquier carpeta de su equipo.
2. Inicie **Docker Desktop** en su equipo.
3. Abre una terminal o consola de comandos en la carpeta descomprimida.
4. Ejecute el siguiente comando:

   ```bash
   docker-compose up --build
   ```

5. Una vez que finalice la carga, abra su navegador web e ingrese a:
   
   **`http://localhost:3838`**

6. Para detener la aplicación, presione `Ctrl + C` en la terminal o ejecute:

   ```bash
   docker-compose down
   ```

---
*Nota: Compatible con Windows, macOS (Intel y Apple Silicon M1/M2/M3/M4) y Linux.*
