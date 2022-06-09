require_relative "compiler"

language_filter = ARGV[0]

starter_template_compiler = StarterTemplateCompiler.new(
  output_directory: "../compiled_starters",
  templates_directory: "../",
  definitions: StarterRepoDefinition.load_from_files("../course-definition.yml", "../starter-repository-definitions.yml")
)

if language_filter
  starter_template_compiler.compile_for_language(language_filter)
else
  starter_template_compiler.compile_all
end
