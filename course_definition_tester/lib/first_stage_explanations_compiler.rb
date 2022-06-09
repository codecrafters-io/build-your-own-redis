require "fileutils"

require_relative "../lib/models"
require_relative "../lib/starter_code_uncommenter"

class FirstStageExplanationsCompiler
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
    explanation_file_path = File.join(@solutions_directory, language.slug, @course.first_stage.slug, "explanation.md")

    File.delete(explanation_file_path) if File.exist?(explanation_file_path)
    FileUtils.mkdir_p(File.dirname(explanation_file_path))

    blocks = StarterCodeUncommenter.new(starter_repository_directory, language).uncommented_blocks_with_markers
    File.write(explanation_file_path, blocks.join("\n\n"))
  end

  protected

  def ensure_blocks_exist!(blocks)
    blocks.each do |block|
      if block.strip.empty?
        log_error("Expected uncommenting code to return a block with a marker")
        log_error("Are you sure there's a contiguous block of comments after the 'Uncomment this' marker?")

        exit(1)
      end

      puts ""
      puts blocks.inspect
      puts ""
    end
  end

  def starter_repository_directories
    Dir.glob(File.join(@starters_directory, "#{@course.slug}-starter-*"))
  end
end
