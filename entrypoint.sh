#!/usr/bin/env bash
# =============================================================================
# entrypoint.sh — Arranque del contenedor con pregunta interactiva de IA.
#
# Lógica de decisión para el módulo de IA local (Ollama):
#   1. Si ENABLE_OLLAMA ya viene definida (docker run -e / compose environment)
#      se usa ese valor SIN preguntar.
#   2. Si hay terminal interactiva real (docker run -it o compose con tty):
#      pregunta "¿Deseas activar el módulo de IA local con Ollama? (s/n)"
#      leyendo de /dev/tty con TIMEOUT de 20 s. Sin respuesta -> modo estándar.
#   3. Sin TTY (CI, daemon): arranca en modo estándar al instante.
# =============================================================================

# "set -e" aborta el script si cualquier comando falla: evita arrancar la app
# con un estado a medio configurar (p. ej. modelo de IA a medias).
set -e

# Paleta de colores ANSI para mensajes legibles en la terminal:
#   GREEN  -> confirmaciones / modo estándar
#   CYAN   -> banner de bienvenida
#   YELLOW -> avisos y preguntas
#   RED    -> errores no fatales (la app sigue arrancando)
#   NC     -> "no color": restablece el formato por defecto
GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# banner: dibuja el marco de bienvenida del contenedor (solo estética).
# Usa "echo -e" para interpretar los códigos de color ANSI (\033[...).
banner() {
  echo -e "${CYAN}"
  echo "┌──────────────────────────────────────────────────────────────────┐"
  echo "│   Evaluador Cuantitativo de Riesgos · OpenFAIR / Monte Carlo     │"
  echo "│   Módulo de IA local opcional: Ollama (llama3.2:1b)              │"
  echo "└──────────────────────────────────────────────────────────────────┘"
  echo -e "${NC}"
}

# enable_ollama: activa el módulo de IA local y deja Ollama listo.
# Se invoca cuando la respuesta del usuario (o ENABLE_OLLAMA) es afirmativa.
enable_ollama() {
  echo -e "${YELLOW}Iniciando servidor de Ollama en segundo plano...${NC}"
  # Variable de entorno que la app R consulta (ENABLE_OLLAMA=true) para
  # habilitar el botón "Generar Resumen con IA" y el chat contextual.
  export ENABLE_OLLAMA=true
  # Ollama NO permite ejecutarse como root; lo lanzamos como el usuario 'shiny'
  # (el mismo que ejecuta Shiny Server), para que la app pueda alcanzarlo.
  # El log se vuelca a /tmp/ollama.log para diagnóstico con `docker logs`.
  if id shiny >/dev/null 2>&1; then
    # su -s /bin/bash shiny -c '...': lanza el proceso con la identidad shiny
    # y sin terminal; "nohup ... &" lo desacopla del script para que siga
    # vivo cuando este termine.
    su -s /bin/bash shiny -c 'nohup ollama serve >/tmp/ollama.log 2>&1 &' \
      || nohup ollama serve >/tmp/ollama.log 2>&1 &
  else
    nohup ollama serve >/tmp/ollama.log 2>&1 &
  fi

  # Espera activa (polling) hasta 30 s a que el endpoint /api/tags responda.
  # Imprime un punto por segundo para que el usuario vea progreso.
  echo -n "Esperando a que Ollama responda"
  for _ in $(seq 1 30); do
    if curl -fsS http://localhost:11434/api/tags >/dev/null 2>&1; then
      echo " OK"
      break
    fi
    echo -n "."
    sleep 1
  done

  # Si la imagen ya pre-cargó el modelo en el build (Dockerfile paso 4b),
  # este pull es un no-op inmediato; si no, descarga los ~1.3 GB una vez.
  # El fallo NO es fatal: la app arranca igualmente en modo estándar.
  echo -e "${YELLOW}Verificando modelo llama3.2:1b (descarga solo la primera vez)...${NC}"
  if id shiny >/dev/null 2>&1; then
    su -s /bin/bash shiny -c 'ollama pull llama3.2:1b' || \
      echo -e "${RED}No se pudo descargar el modelo. La app arrancará igualmente (modo estándar).${NC}"
  else
    ollama pull llama3.2:1b || \
      echo -e "${RED}No se pudo descargar el modelo. La app arrancará igualmente (modo estándar).${NC}"
  fi
}

# disable_ollama: apaga el módulo de IA y arranca la app sin ella.
# La variable ENABLE_OLLAMA=false hace que la app no intente conectar.
disable_ollama() {
  echo -e "${GREEN}Iniciando la aplicación en modo estándar (sin módulo de IA)...${NC}"
  export ENABLE_OLLAMA=false
}

# ask_ollama: pregunta interactiva robusta.
# - El prompt va a STDERR (>&2) para que STDOUT quede limpio y solo contenga
#   la respuesta final ('s' o 'n'); el caller hace ans="$(ask_ollama)".
# - `read -t 20` es un builtin de bash que aporta el timeout nativo (20 s);
#   no hace falta `timeout`, que además rompía al tratar `read` como binario.
# - Se recorta cualquier CR/espacio que pueda dejar el TTY (terminales
#   Windows) y se pasa a minúsculas antes de comparar con s/n.
ask_ollama() {
  echo "" >&2
  echo -n "¿Deseas activar el módulo de IA local con Ollama en este contenedor? (s/n) [n]: " >&2
  # Valor por defecto "n": si el usuario pulsa Enter sin responder, NO se
  # activa la IA (decisión conservadora por defecto).
  local ans="n"
  # Intenta leer de /dev/tty (la terminal real). Si no hay TTY o expira el
  # timeout (20 s), read falla y se mantiene "n".
  if read -r -t 20 ans < /dev/tty 2>/dev/null; then
    # Normaliza la respuesta: elimina espacios/CR y pasa a minúsculas.
    ans="$(printf '%s' "$ans" | tr -d '[:space:]' | tr '[:upper:]' '[:lower:]')"
    echo -e "${YELLOW}Respuesta detectada: '${ans:-<vacía>}'${NC}" >&2
    # Acepta cualquier respuesta que EMPIECE por s o y (s, si, sí, yes...).
    case "$ans" in
      s*|y*) ans="s" ;;
      *)     ans="n" ;;
    esac
  else
    # Timeout agotado o sin TTY: se avisa y se continúa en modo estándar.
    echo ""
    echo -e "${YELLOW}Tiempo agotado sin respuesta; continuando en modo estándar.${NC}"
    ans="n"
  fi
  # STDOUT solo con la respuesta final (sin saltos de línea) para que
  # ans="$(ask_ollama)" capture exactamente 's' o 'n'.
  printf '%s' "$ans"
}

# ---------------------------------------------------------------------------
# Flujo principal del entrypoint
# ---------------------------------------------------------------------------
banner

# 1) Si la variable ENABLE_OLLAMA ya viene definida por el entorno (docker run
#    -e ENABLE_OLLAMA=true o `environment:` en docker-compose.yml), se usa ese
#    valor SIN hacer la pregunta interactiva. Esto permite automatizar el
#    arranque en CI o en despliegues sin terminal.
if [ -n "${ENABLE_OLLAMA:-}" ]; then
  # Normaliza el valor: true/1/yes/y/s activan; cualquier otra cosa no.
  case "$(echo "$ENABLE_OLLAMA" | tr '[:upper:]' '[:lower:]')" in
    true|1|yes|y|s) ans="s" ;;
    *)              ans="n" ;;
  esac
  echo -e "${YELLOW}ENABLE_OLLAMA=$ENABLE_OLLAMA detectado: no se pregunta.${NC}"

# 2) Terminal interactiva real -> pregunta al usuario:
#    - -t 0: STDIN es una terminal
#    - -r /dev/tty y -w /dev/tty: la terminal real es legible y escribible
#    (con docker run -it se cumplen las tres condiciones).
elif [ -t 0 ] && [ -r /dev/tty ] && [ -w /dev/tty ]; then
  ans="$(ask_ollama)"

# 3) Sin TTY (compose up, CI, daemon): arranca en modo estándar al instante,
#    sin bloquear el arranque esperando una respuesta que nunca llegará.
else
  echo -e "${YELLOW}Sin terminal interactiva; arrancando en modo estándar.${NC}"
  echo -e "${YELLOW}Para activar la IA local interactiva usa:${NC}"
  echo -e "${YELLOW}  docker compose run --rm -p 3838:3838 evaluator-app${NC}"
  echo -e "${YELLOW}  o: docker run -it --rm -p 3838:3838 tfm-evaluator${NC}"
  echo -e "${YELLOW}  o fija en el compose: environment: ENABLE_OLLAMA=true${NC}"
  ans="n"
fi

# Aplica la decisión: activa (enable_ollama) o desactiva (disable_ollama) el
# módulo de IA local según la respuesta normalizada ('s' o 'n').
case "$ans" in
  s*) enable_ollama ;;
  *)  disable_ollama ;;
esac

# Finalmente se arranca la aplicación real (la CMD del Dockerfile, por defecto
# /usr/bin/shiny-server). `exec` REEMPLAZA el proceso bash por shiny-server,
# de modo que el PID 1 del contenedor es el servidor Shiny: las señales
# (docker stop) llegan directamente y los logs fluyen a `docker logs`.
echo -e "${GREEN}Arrancando la aplicación de R...${NC}"
exec "$@"
