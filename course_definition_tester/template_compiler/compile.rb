require_relative "compiler"

language_filter = ARGV[0]

if language_filter
  TemplateCompiler.new("../starter_templates", "../compiled_starters").compile_one(language_filter)
else
  TemplateCompiler.new("../starter_templates", "../compiled_starters").compile_all
end
