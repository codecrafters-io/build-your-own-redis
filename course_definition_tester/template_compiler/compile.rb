require_relative "compiler"

language_filter = ARGV[0]

template_compiler = TemplateCompiler.new(
  output_directory: "../compiled_starters",
  templates_directory: "../",
  definitions: StarterRepoDefinition.load_from_files("../course-definition.yml", "../starter-repository-definitions.yml")
)

if language_filter
  template_compiler.compile_for_language(language_filter)
else
  template_compiler.compile_all
end
