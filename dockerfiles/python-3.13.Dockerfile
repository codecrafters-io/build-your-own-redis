# syntax=docker/dockerfile:1.7-labs
FROM python:3.13-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Pipfile,Pipfile.lock"

# pipenv uses this to store virtualenvs
ENV WORKON_HOME=/venvs

RUN pip install --no-cache-dir "pipenv>=2025.0.2"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN pipenv install

# Force environment creation
RUN pipenv run python3 -c "1+1"