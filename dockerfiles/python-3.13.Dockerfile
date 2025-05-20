FROM python:3.13-alpine

RUN pip install --no-cache-dir pipenv==2025.0.2

COPY Pipfile /app/Pipfile
COPY Pipfile.lock /app/Pipfile.lock

WORKDIR /app

ENV WORKON_HOME=/venvs

RUN pipenv install

# Force environment creation
RUN pipenv run python3 -c "1+1"

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Pipfile,Pipfile.lock"