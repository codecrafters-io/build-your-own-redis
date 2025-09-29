FROM golang:1.21-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="go.mod,go.sum"

WORKDIR /app

COPY go.mod go.sum ./

# Starting from Go 1.20, the go standard library is no loger compiled
# setting the GODEBUG environment to "installgoroot=all" restores the old behavior
# hadolint ignore=DL3062
RUN GODEBUG="installgoroot=all" go install std

RUN go mod download
