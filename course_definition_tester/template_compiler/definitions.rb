FM = FileMapping
Repo = StarterRepoDefinition

redis = Course.new(slug: "redis", name: "Redis")

def load_definitions_from_file(filename)
  YAML.load_file(File.join(File.dirname(__FILE__), filename))
end

# TODO: Copy items from here!
# DEFINITIONS = [
#   Repo.new(
#     course: redis,
#     language: csharp,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new(".gitignore", "redis/csharp/.gitignore"),
#       FM.new("codecrafters-redis.sln", "redis/csharp/codecrafters-redis.sln"),
#       FM.new("codecrafters-redis.csproj", "redis/csharp/codecrafters-redis.csproj"),
#       FM.new("src/Server.cs", "redis/csharp/src/Server.cs"),
#       FM.new("spawn_redis_server.sh", "redis/csharp/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "dotnet (6.0)",
#       "user_editable_file": "src/Server.cs"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: python,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("app/main.py", "redis/python/app/main.py"),
#       FM.new("spawn_redis_server.sh", "redis/python/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "python (3.8)",
#       "user_editable_file": "app/main.py"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: go,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("app/server.go", "redis/go/app/server.go"),
#       FM.new("spawn_redis_server.sh", "redis/go/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "go (1.13)",
#       "user_editable_file": "app/server.go"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: c,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("app/server.c", "redis/c/app/server.c"),
#       FM.new("spawn_redis_server.sh", "redis/c/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "gcc",
#       "user_editable_file": "app/server.c"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: ruby,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("app/server.rb", "redis/ruby/app/server.rb"),
#       FM.new("spawn_redis_server.sh", "redis/ruby/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "ruby (2.7)",
#       "user_editable_file": "app/server.rb"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: haskell,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new(".gitignore", "redis/haskell/.gitignore"),
#       FM.new("package.yaml", "redis/haskell/package.yaml"),
#       FM.new("stack.yaml", "redis/haskell/stack.yaml"),
#       FM.new("stack.yaml.lock", "redis/haskell/stack.yaml.lock"),
#       FM.new("app/Main.hs", "redis/haskell/app/Main.hs"),
#       FM.new("spawn_redis_server.sh", "redis/haskell/spawn_redis_server.sh", is_executable=true),
#     ],
#       template_attrs: {
#         "required_executable": "stack",
#         "user_editable_file": "Main.hs"
#       }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: java,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new(".gitignore", "redis/java/.gitignore"),
#       FM.new("src/main/java/Main.java", "redis/java/src/main/java/Main.java"),
#       FM.new("spawn_redis_server.sh", "redis/java/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "java (1.8)",
#       "user_editable_file": "Main.java"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: elixir,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("lib/server.ex", "redis/elixir/lib/server.ex"),
#       FM.new("mix.exs", "redis/elixir/mix.exs"),
#       FM.new("spawn_redis_server.sh", "redis/elixir/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "mix",
#       "user_editable_file": "lib/server.ex"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: rust,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("src/main.rs", "redis/rust/src/main.rs"),
#       FM.new(".gitignore", "redis/rust/.gitignore"),
#       FM.new("Cargo.toml", "redis/rust/Cargo.toml"),
#       FM.new("Cargo.lock", "redis/rust/Cargo.lock"),
#       FM.new("spawn_redis_server.sh", "redis/rust/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "cargo (1.54)",
#       "user_editable_file": "src/main.rs"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: php,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("app/main.php", "redis/php/app/main.php"),
#       FM.new("spawn_redis_server.sh", "redis/php/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "php (7.4)",
#       "user_editable_file": "app/main.php"
#     }
#   ),
#
#   Repo.new(
#     course: redis,
#     language: javascript,
#     file_mappings: [
#       FM.new("README.md", "redis/README.md"),
#       FM.new("codecrafters.yml", "codecrafters.yml"),
#       FM.new("app/main.js", "redis/javascript/app/main.js"),
#       FM.new("spawn_redis_server.sh", "redis/javascript/spawn_redis_server.sh", is_executable=true),
#     ],
#     template_attrs: {
#       "required_executable": "node (16)",
#       "user_editable_file": "app/main.js"
#     }
#   ),
# ]
