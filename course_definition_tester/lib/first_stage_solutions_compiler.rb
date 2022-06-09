class FirstStageSolutionsCompiler
  def initialize(course, compiled_solutions_directory, solutions_directory)
    @course = course
    @compiled_starters_directory = compiled_solutions_directory
    @solutions_directory = solutions_directory
  end

  def compile
    starter_repository_directories.each do |starter_repository_directory|
      compile_for_starter_repository_directory(starter_repository_directory)
    end
  end

  def compile_for_starter_repository_directory(starter_repository_directory)
    language = Language.find_by_slug!(File.basename(starter_repository_directory).split("-").last)
    first_stage_solution_directory = File.join(@solutions_directory, @course.slug, @course.first_stage.slug, "code")

    FileUtils.rm_rf(first_stage_solution_directory) if File.exist?(first_stage_solution_directory)
    FileUtils.mkdir_p(first_stage_solution_directory)

    diffs = StarterCodeUncommenter.new(starter_repository_directory, language).uncomment

    diffs.each do |diff|
      if diff.to_s.empty?
        log_error("Expected uncommenting code to return a diff")
        log_error("Are you sure there's a contiguous block of comments after the 'Uncomment this' marker?")

        exit(1)
      end

      puts ""
      puts diff.to_s(:color)
      puts ""
    end
  end

  protected

  def starter_repository_directories
    Dir.glob(File.join(@compiled_starters_directory, "#{@course.slug}-starter-*"))
  end
end
