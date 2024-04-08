FROM golang:1.22-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="go.mod,go.sum"

WORKDIR /app

COPY go.mod go.sum ./

# Starting from Go 1.20, the go standard library is no loger compiled
# setting the GODEBUG environment to "installgoroot=all" restores the old behavior
RUN GODEBUG="installgoroot=all" go install std

RUN ash -c "set -exo pipefail; go mod graph | awk '{if (\$1 !~ \"@\") {print \$2}}' | xargs -r go get"
