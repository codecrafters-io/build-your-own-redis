require_relative "../lib/solution_diffs_compiler"
require_relative "../lib/models"

solution_diffs_compiler = SolutionDiffsCompiler.new(
  course: Course.load_from_file("../course-definition.yml"),
  solutions_directory: "../solutions",
  starters_directory: "../compiled_starters"
)

solution_diffs_compiler.compile_all
