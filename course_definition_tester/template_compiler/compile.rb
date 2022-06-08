require_relative "compiler"

course_filter = ARGV[0]
language_filter = ARGV[1]

if course_filter && language_filter
  TemplateCompiler.new("starter_templates", "compiled_starters").compile_one(course_filter, language_filter)
else
  TemplateCompiler.new("starter_templates", "compiled_starters").compile_all
end
