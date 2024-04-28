FROM gcc:13.2.0-bookworm

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="vcpkg.json,vcpkg-configuration.json"

RUN apt-get update && \
    apt-get install --no-install-recommends -y zip=3.* && \ 
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# cmake 3.29.2 is required by vcpkg
RUN wget --progress=dot:giga https://github.com/Kitware/CMake/releases/download/v3.29.2/cmake-3.29.2-Linux-x86_64.tar.gz && \
tar -xzvf cmake-3.29.2-Linux-x86_64.tar.gz && \
mv cmake-3.29.2-linux-x86_64/ /cmake

ENV PATH="/cmake/bin:$PATH"

RUN git clone https://github.com/microsoft/vcpkg.git && \
    ./vcpkg/bootstrap-vcpkg.sh -disableMetrics

ENV VCPKG_ROOT="/vcpkg"
ENV PATH="$VCPKG_ROOT:$PATH"

WORKDIR /app

COPY vcpkg.json ./
COPY vcpkg-configuration.json ./

RUN vcpkg install

RUN mkdir -p /app-cached
RUN if [ -d "/app/vcpkg_installed" ]; then mv /app/vcpkg_installed /app-cached; fi

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE=/vcpkg/scripts/buildsystems/vcpkg.cmake && cmake --build ./build && sed -i '/^cmake/ s/^/# /' ./spawn_redis_server.sh" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
