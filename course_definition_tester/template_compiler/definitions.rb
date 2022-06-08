FM = FileMapping
Repo = StarterRepoDefinition

redis = Course.new(slug: "redis", name: "Redis")
docker = Course.new(slug: "docker", name: "Docker")
git = Course.new(slug: "git", name: "Git")
sqlite = Course.new(slug: "sqlite", name: "SQLite")
react = Course.new(slug: "react", name: "React")

c = Language.new(slug: "c", name: "C", repo_suffix: "c")
clojure = Language.new(slug: "clojure", name: "Clojure", repo_suffix: "clojure")
crystal = Language.new(slug: "crystal", name: "Crystal", repo_suffix: "crystal")
csharp = Language.new(slug: "csharp", name: "C#", repo_suffix: "csharp")
elixir = Language.new(slug: "elixir", name: "Elixir", repo_suffix: "elixir")
go = Language.new(slug: "go", name: "Go", repo_suffix: "go")
haskell = Language.new(slug: "haskell", name: "Haskell", repo_suffix: "haskell")
java = Language.new(slug: "java", name:"Java", repo_suffix: "java")
javascript = Language.new(slug: "javascript", name: "JavaScript", repo_suffix: "javascript")
kotlin = Language.new(slug: "kotlin", name: "Kotlin", repo_suffix: "kotlin")
nim = Language.new(slug: "nim", name: "Nim", repo_suffix: "nim")
php = Language.new(slug: "php", name: "PHP", repo_suffix: "php")
python = Language.new(slug: "python", name: "Python", repo_suffix: "python")
ruby = Language.new(slug: "ruby", name: "Ruby", repo_suffix: "ruby")
rust = Language.new(slug: "rust", name: "Rust", repo_suffix: "rust")

DEFINITIONS = [
  # ------------------- REDIS ------------------------------
  Repo.new(
    course: redis,
    language: clojure,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new(".gitignore", "redis/clojure/.gitignore"),
      FM.new("project.clj", "redis/clojure/project.clj"),
      FM.new("src/redis/core.clj", "redis/clojure/src/redis/core.clj"),
      FM.new("spawn_redis_server.sh", "redis/clojure/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "lein",
      "user_editable_file": "src/redis/core.clj"
    }
  ),

  Repo.new(
    course: redis,
    language: crystal,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.cr", "redis/crystal/app/main.cr"),
      FM.new("spawn_redis_server.sh", "redis/crystal/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "crystal (1.1.0)",
      "user_editable_file": "app/main.cr"
    }
  ),

  Repo.new(
    course: redis,
    language: csharp,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new(".gitignore", "redis/csharp/.gitignore"),
      FM.new("codecrafters-redis.sln", "redis/csharp/codecrafters-redis.sln"),
      FM.new("codecrafters-redis.csproj", "redis/csharp/codecrafters-redis.csproj"),
      FM.new("src/Server.cs", "redis/csharp/src/Server.cs"),
      FM.new("spawn_redis_server.sh", "redis/csharp/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "dotnet (6.0)",
      "user_editable_file": "src/Server.cs"
    }
  ),

  Repo.new(
    course: redis,
    language: python,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.py", "redis/python/app/main.py"),
      FM.new("spawn_redis_server.sh", "redis/python/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "python (3.8)",
      "user_editable_file": "app/main.py"
    }
  ),
  Repo.new(
    course: redis,
    language: go,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/server.go", "redis/go/app/server.go"),
      FM.new("spawn_redis_server.sh", "redis/go/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "go (1.13)",
      "user_editable_file": "app/server.go"
    }
  ),
  Repo.new(
    course: redis,
    language: c,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/server.c", "redis/c/app/server.c"),
      FM.new("spawn_redis_server.sh", "redis/c/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "gcc",
      "user_editable_file": "app/server.c"
    }
  ),
  Repo.new(
    course: redis,
    language: ruby,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/server.rb", "redis/ruby/app/server.rb"),
      FM.new("spawn_redis_server.sh", "redis/ruby/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "ruby (2.7)",
      "user_editable_file": "app/server.rb"
    }
  ),
  Repo.new(
    course: redis,
    language: haskell,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new(".gitignore", "redis/haskell/.gitignore"),
      FM.new("package.yaml", "redis/haskell/package.yaml"),
      FM.new("stack.yaml", "redis/haskell/stack.yaml"),
      FM.new("stack.yaml.lock", "redis/haskell/stack.yaml.lock"),
      FM.new("app/Main.hs", "redis/haskell/app/Main.hs"),
      FM.new("spawn_redis_server.sh", "redis/haskell/spawn_redis_server.sh", is_executable=true),
    ],
      template_attrs: {
        "required_executable": "stack",
        "user_editable_file": "Main.hs"
      }
  ),
  Repo.new(
    course: redis,
    language: java,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new(".gitignore", "redis/java/.gitignore"),
      FM.new("src/main/java/Main.java", "redis/java/src/main/java/Main.java"),
      FM.new("spawn_redis_server.sh", "redis/java/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "java (1.8)",
      "user_editable_file": "Main.java"
    }
  ),
  Repo.new(
    course: redis,
    language: elixir,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("lib/server.ex", "redis/elixir/lib/server.ex"),
      FM.new("mix.exs", "redis/elixir/mix.exs"),
      FM.new("spawn_redis_server.sh", "redis/elixir/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "mix",
      "user_editable_file": "lib/server.ex"
    }
  ),
  Repo.new(
    course: redis,
    language: rust,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("src/main.rs", "redis/rust/src/main.rs"),
      FM.new(".gitignore", "redis/rust/.gitignore"),
      FM.new("Cargo.toml", "redis/rust/Cargo.toml"),
      FM.new("Cargo.lock", "redis/rust/Cargo.lock"),
      FM.new("spawn_redis_server.sh", "redis/rust/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "cargo (1.54)",
      "user_editable_file": "src/main.rs"
    }
  ),
  Repo.new(
    course: redis,
    language: php,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.php", "redis/php/app/main.php"),
      FM.new("spawn_redis_server.sh", "redis/php/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "php (7.4)",
      "user_editable_file": "app/main.php"
    }
  ),
  Repo.new(
    course: redis,
    language: javascript,
    file_mappings: [
      FM.new("README.md", "redis/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.js", "redis/javascript/app/main.js"),
      FM.new("spawn_redis_server.sh", "redis/javascript/spawn_redis_server.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "node (16)",
      "user_editable_file": "app/main.js"
    }
  ),

  # ------------------- DOCKER ------------------------------

  Repo.new(
    course: docker,
    language: go,
    file_mappings: [
      FM.new("README.md", "docker/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.go", "docker/go/app/main.go"),
      FM.new("Dockerfile", "docker/go/Dockerfile"),
      FM.new("your_docker.sh", "docker/go/your_docker.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "go",
      "user_editable_file": "app/main.go"
    }
  ),
  Repo.new(
    course: docker,
    language: c,
    file_mappings: [
      FM.new("README.md", "docker/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.c", "docker/c/app/main.c"),
      FM.new("Dockerfile", "docker/c/Dockerfile"),
      FM.new("your_docker.sh", "docker/c/your_docker.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "gcc",
      "user_editable_file": "app/main.c"
    }
  ),
  Repo.new(
    course: docker,
    language: php,
    file_mappings: [
      FM.new("README.md", "docker/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.php", "docker/php/app/main.php"),
      FM.new("Dockerfile", "docker/php/Dockerfile"),
      FM.new("your_docker.sh", "docker/php/your_docker.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "php (7.4)",
      "user_editable_file": "app/main.php"
    }
  ),
  Repo.new(
    course: docker,
    language: nim,
    file_mappings: [
      FM.new("README.md", "docker/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.nim", "docker/nim/app/main.nim"),
      FM.new("Dockerfile", "docker/nim/Dockerfile"),
      FM.new("your_docker.sh", "docker/nim/your_docker.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "nim (1.0)",
      "user_editable_file": "app/main.nim"
    }
  ),
  Repo.new(
    course: docker,
    language: rust,
    file_mappings: [
      FM.new("README.md", "docker/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("src/main.rs", "docker/rust/src/main.rs"),
      FM.new(".gitignore", "docker/rust/.gitignore"),
      FM.new("Cargo.toml", "docker/rust/Cargo.toml"),
      FM.new("Cargo.lock", "docker/rust/Cargo.lock"),
      FM.new("Dockerfile", "docker/rust/Dockerfile"),
      FM.new("your_docker.sh", "docker/rust/your_docker.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "cargo (1.54)",
      "user_editable_file": "src/main.rs"
    }
  ),

  # ------------------- GIT ------------------------------

  Repo.new(
    course: git,
    language: python,
    file_mappings: [
      FM.new("README.md", "git/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.py", "git/python/app/main.py"),
      FM.new("your_git.sh", "git/python/your_git.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "python",
      "user_editable_file": "app/main.py"
    }
  ),
  Repo.new(
    course: git,
    language: ruby,
    file_mappings: [
      FM.new("README.md", "git/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.rb", "git/ruby/app/main.rb"),
      FM.new("your_git.sh", "git/ruby/your_git.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "ruby",
      "user_editable_file": "app/main.rb"
    }
  ),
  Repo.new(
    course: git,
    language: go,
    file_mappings: [
      FM.new("README.md", "git/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.go", "git/go/app/main.go"),
      FM.new("your_git.sh", "git/go/your_git.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "go",
      "user_editable_file": "app/main.go"
    }
  ),
  Repo.new(
    course: git,
    language: rust,
    file_mappings: [
      FM.new("README.md", "git/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("src/main.rs", "git/rust/src/main.rs"),
      FM.new(".gitignore", "git/rust/.gitignore"),
      FM.new("Cargo.toml", "git/rust/Cargo.toml"),
      FM.new("Cargo.lock", "git/rust/Cargo.lock"),
      FM.new("your_git.sh", "git/rust/your_git.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "cargo (1.54)",
      "user_editable_file": "src/main.rs"
    }
  ),
  Repo.new(
    course: git,
    language: kotlin,
    file_mappings: [
      FM.new("README.md", "git/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.kt", "git/kotlin/app/main.kt"),
      FM.new("your_git.sh", "git/kotlin/your_git.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "kotlin",
      "user_editable_file": "app/main.kt"
    }
  ),

  # ------------------- SQLITE ------------------------------

  Repo.new(
    course: sqlite,
    language: python,
    file_mappings: [
      FM.new("README.md", "sqlite/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.py", "sqlite/python/app/main.py"),
      FM.new("app/record_parser.py", "sqlite/python/app/record_parser.py"),
      FM.new("app/varint_parser.py", "sqlite/python/app/varint_parser.py"),
      FM.new("sample.db", "sqlite/sample.db"),
      FM.new("Pipfile", "sqlite/python/Pipfile"),
      FM.new("Pipfile.lock", "sqlite/python/Pipfile.lock"),
      FM.new(".gitignore", "sqlite/.gitignore"),
      FM.new("your_sqlite3.sh", "sqlite/python/your_sqlite3.sh", is_executable=true),
      FM.new("download_sample_databases.sh", "sqlite/download_sample_databases.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "python (3.8)",
      "user_editable_file": "app/main.py"
    }
  ),

  Repo.new(
    course: sqlite,
    language: go,
    file_mappings: [
      FM.new("README.md", "sqlite/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.go", "sqlite/go/app/main.go"),
      FM.new("app/record_parser.go", "sqlite/go/app/record_parser.go"),
      FM.new("app/varint_parser.go", "sqlite/go/app/varint_parser.go"),
      FM.new("app/int_parser.go", "sqlite/go/app/int_parser.go"),
      FM.new("app/page_header.go", "sqlite/go/app/page_header.go"),
      FM.new("sample.db", "sqlite/sample.db"),
      FM.new("go.mod", "sqlite/go/go.mod"),
      FM.new("go.sum", "sqlite/go/go.sum"),
      FM.new(".gitignore", "sqlite/.gitignore"),
      FM.new("your_sqlite3.sh", "sqlite/go/your_sqlite3.sh", is_executable=true),
      FM.new("download_sample_databases.sh", "sqlite/download_sample_databases.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "go (1.16)",
      "user_editable_file": "app/main.go"
    }
  ),

  Repo.new(
    course: sqlite,
    language: javascript,
    file_mappings: [
      FM.new("README.md", "sqlite/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("app/main.js", "sqlite/javascript/app/main.js"),
      FM.new("app/database-file.js", "sqlite/javascript/app/database-file.js"),
      FM.new("app/read-int.js", "sqlite/javascript/app/read-int.js"),
      FM.new("app/read-page-header.js", "sqlite/javascript/app/read-page-header.js"),
      FM.new("app/read-record.js", "sqlite/javascript/app/read-record.js"),
      FM.new("app/read-varint.js", "sqlite/javascript/app/read-varint.js"),
      FM.new("package.json", "sqlite/javascript/package.json"),
      FM.new("sample.db", "sqlite/sample.db"),
      FM.new(".gitignore", "sqlite/.gitignore"),
      FM.new("your_sqlite3.sh", "sqlite/javascript/your_sqlite3.sh", is_executable=true),
      FM.new("download_sample_databases.sh", "sqlite/download_sample_databases.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "node (16)",
      "user_editable_file": "app/main.js"
    }
  ),
  Repo.new(
    course: sqlite,
    language: rust,
    file_mappings: [
      FM.new("README.md", "sqlite/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("src/main.rs", "sqlite/rust/src/main.rs"),
      FM.new("src/header.rs", "sqlite/rust/src/header.rs"),
      FM.new("src/lib.rs", "sqlite/rust/src/lib.rs"),
      FM.new("src/record.rs", "sqlite/rust/src/record.rs"),
      FM.new("src/schema.rs", "sqlite/rust/src/schema.rs"),
      FM.new("src/varint.rs", "sqlite/rust/src/varint.rs"),
      FM.new(".gitignore", "sqlite/rust/.gitignore"),
      FM.new("Cargo.toml", "sqlite/rust/Cargo.toml"),
      FM.new("Cargo.lock", "sqlite/rust/Cargo.lock"),
      FM.new("your_sqlite3.sh", "sqlite/rust/your_sqlite3.sh", is_executable=true),
      FM.new("sample.db", "sqlite/sample.db"),
      FM.new("download_sample_databases.sh", "sqlite/download_sample_databases.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "cargo (1.54)",
      "user_editable_file": "src/main.rs"
    }
  ),
  Repo.new(
    course: sqlite,
    language: csharp,
    file_mappings: [
      FM.new("README.md", "sqlite/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("src/Program.cs", "sqlite/csharp/src/Program.cs"),
      FM.new("src/PageHeader.cs", "sqlite/csharp/src/PageHeader.cs"),
      FM.new("src/Record.cs", "sqlite/csharp/src/Record.cs"),
      FM.new("src/Schema.cs", "sqlite/csharp/src/Schema.cs"),
      FM.new("src/Varint.cs", "sqlite/csharp/src/Varint.cs"),
      FM.new("src/MemoryExtensions.cs", "sqlite/csharp/src/MemoryExtensions.cs"),
      FM.new("codecrafters-sqlite.csproj", "sqlite/csharp/codecrafters-sqlite.csproj"),
      FM.new("codecrafters-sqlite.sln", "sqlite/csharp/codecrafters-sqlite.sln"),
      FM.new(".gitignore", "sqlite/csharp/.gitignore"),
      FM.new("your_sqlite3.sh", "sqlite/csharp/your_sqlite3.sh", is_executable=true),
      FM.new("sample.db", "sqlite/sample.db"),
      FM.new("download_sample_databases.sh", "sqlite/download_sample_databases.sh", is_executable=true),
    ],
    template_attrs: {
      "required_executable": "dotnet (6.0)",
      "user_editable_file": "src/Program.cs"
    }
  ),

  # ------------------- REACT ------------------------------
  Repo.new(
    course: react,
    language: javascript,
    file_mappings: [
      FM.new("README.md", "react/README.md"),
      FM.new("codecrafters.yml", "codecrafters.yml"),
      FM.new("react/index.js", "react/javascript/react/index.js"),
      FM.new("react/package.json", "react/javascript/react/package.json"),
      FM.new("react-dom/index.js", "react/javascript/react-dom/index.js"),
      FM.new("react-dom/package.json", "react/javascript/react-dom/package.json"),
    ],
    template_attrs: {
      "required_executable": "node (16)",
      "user_editable_file": "react/index.js"
    }
  ),
]
