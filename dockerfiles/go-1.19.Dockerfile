FROM golang:1.19-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="go.mod,go.sum"

WORKDIR /app

COPY go.mod go.sum ./

RUN go mod download
