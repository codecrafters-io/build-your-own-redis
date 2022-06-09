require_relative "../lib/first_stage_solutions_compiler"

solutions_compiler = FirstStageSolutionsCompiler.new(
  course: Course.load_from_file("../course-definition.yml"),
  starters_directory: "../compiled_starters",
  solutions_directory: "../solutions"
)

solutions_compiler.compile_all
