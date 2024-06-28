# syntax=docker/dockerfile:1.7-labs
FROM gcc:13.2.0-bookworm

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="vcpkg.json,vcpkg-configuration.json"

RUN apt-get update && \
    apt-get install --no-install-recommends -y zip=3.* && \ 
    apt-get install --no-install-recommends -y g++=4:* && \
    apt-get install --no-install-recommends -y build-essential=12.* && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# cmake 3.29.2 is required by vcpkg
RUN wget --progress=dot:giga https://github.com/Kitware/CMake/releases/download/v3.29.2/cmake-3.29.2-Linux-x86_64.tar.gz && \
    tar -xzvf cmake-3.29.2-Linux-x86_64.tar.gz && \
    mv cmake-3.29.2-linux-x86_64/ /cmake

ENV CMAKE_BIN="/cmake/bin"
ENV PATH="${CMAKE_BIN}:$PATH"

RUN git clone https://github.com/microsoft/vcpkg.git && \
    ./vcpkg/bootstrap-vcpkg.sh -disableMetrics

ENV VCPKG_ROOT="/vcpkg"
ENV PATH="${VCPKG_ROOT}:$PATH"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN vcpkg install --no-print-usage
RUN sed -i '1s/^/set(VCPKG_INSTALL_OPTIONS --no-print-usage)\n/' ${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake

RUN mkdir -p /app-cached/build
RUN if [ -d "/app/build" ]; then mv /app/build /app-cached; fi
RUN if [ -d "/app/vcpkg_installed" ]; then mv /app/vcpkg_installed /app-cached/build; fi

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE=${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake && cmake --build ./build && sed -i '/^cmake/ s/^/# /' ./spawn_redis_server.sh" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

# Once the heave steps are done, we can copy all files back
COPY . /app
