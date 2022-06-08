require_relative 'models'
require_relative 'definitions'
require "fileutils"
require "pmap"

class TemplateCompiler
  POSTPROCESSORS = {
    "md" => Proc.new { |filepath| `npx prettier --prose-wrap="always" --write #{filepath}` },
    "js" => Proc.new { |filepath| `npx prettier --write #{filepath}` }
  }

  def initialize(templates_path, output_directory)
    @templates_path = templates_path
    @output_directory = output_directory
  end

  def compile_all
    DEFINITIONS.pmap do |definition|
      puts "compiling #{definition.course.slug}-#{definition.language.slug}"
      compile_definition(definition)
    end
  end

  def compile_one(course_slug, language_slug)
    DEFINITIONS.each do |definition|
      next unless definition.course.slug.eql?(course_slug) && definition.language.slug.eql?(language_slug)
      puts "compiling #{definition.course.slug}-#{definition.language.slug}"
      compile_definition(definition)
    end
  end

  private

  def compile_definition(definition)
    directory = File.join(@output_directory, definition.repo_name)
    FileUtils.rmtree(directory)

    definition.files(@templates_path).each do |file|
      path = File.join(directory, file[:path])
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, file[:contents])
      FileUtils.chmod(0755, path) if file[:is_executable]
      postprocess!(path)
    end
  end

  def postprocess!(filepath)
    POSTPROCESSORS["md"].call(filepath) if filepath.end_with?(".md")
    POSTPROCESSORS["js"].call(filepath) if filepath.end_with?(".js")
  end
end
