require "fileutils"

require_relative "../lib/models"
require_relative "../lib/starter_code_uncommenter"

class FirstStageSolutionsCompiler
  def initialize(course:, starters_directory:, solutions_directory:)
    @course = course
    @starters_directory = starters_directory
    @solutions_directory = solutions_directory
  end

  def compile_all
    starter_repository_directories.each do |starter_repository_directory|
      compile_for_starter_repository_directory(starter_repository_directory)
    end
  end

  def compile_for_starter_repository_directory(starter_repository_directory)
    language = Language.find_by_slug!(File.basename(starter_repository_directory).split("-").last)
    first_stage_solution_directory = File.join(@solutions_directory, language.slug, @course.first_stage.slug, "code")

    FileUtils.rm_rf(first_stage_solution_directory) if File.exist?(first_stage_solution_directory)
    FileUtils.mkdir_p(first_stage_solution_directory)
    FileUtils.cp_r("#{starter_repository_directory}/.", first_stage_solution_directory)

    diffs = StarterCodeUncommenter.new(first_stage_solution_directory, language.slug).uncomment

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
    Dir.glob(File.join(@starters_directory, "#{@course.slug}-starter-*"))
  end
end
