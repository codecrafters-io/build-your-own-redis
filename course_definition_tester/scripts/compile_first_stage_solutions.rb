require_relative "../lib/first_stage_solutions_compiler"
require_relative "../lib/first_stage_explanations_compiler"

solutions_compiler = FirstStageSolutionsCompiler.new(
  course: Course.load_from_file("../course-definition.yml"),
  starters_directory: "../compiled_starters",
  solutions_directory: "../solutions"
)

explanations_compiler = FirstStageExplanationsCompiler.new(
  course: Course.load_from_file("../course-definition.yml"),
  starters_directory: "../compiled_starters",
  solutions_directory: "../solutions"
)

solutions_compiler.compile_all
explanations_compiler.compile_all
