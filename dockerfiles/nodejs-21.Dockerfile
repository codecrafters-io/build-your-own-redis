FROM node:21.7-alpine3.19

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="package.json,package-lock.json"

WORKDIR /app

COPY package.json ./
COPY package-lock.json ./

RUN npm i 

RUN mkdir -p /app-cached
# If the node_modules directory exists, move it to /app-cached
RUN if [ -d "/app/node_modules" ]; then mv /app/node_modules /app-cached; fi

RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && npm i" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
