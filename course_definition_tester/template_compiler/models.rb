require "mustache"

class Course
  attr_reader :slug
  attr_reader :name

  def initialize(slug:, name:)
    @slug = slug
    @name = name
  end
end

class Language
  attr_reader :slug
  attr_reader :name
  attr_reader :repo_suffix

  def initialize(slug:, name:, repo_suffix:)
    @slug = slug
    @name = name
    @repo_suffix = repo_suffix
  end
end

class FileMapping
  attr_reader :destination_path
  attr_reader :template_path
  attr_reader :is_executable

  def initialize(destination_path, template_path, is_executable=false)
    @destination_path = destination_path
    @template_path = template_path
    @is_executable = is_executable
  end
end

class StarterRepoDefinition
  attr_reader :course
  attr_reader :language
  attr_reader :file_mappings
  attr_reader :template_attrs

  def initialize(course:, language:, file_mappings:, template_attrs:)
    @course = course
    @language = language
    @file_mappings = file_mappings
    @template_attrs = template_attrs
  end

  def repo_name
    "#{course.slug}-starter-#{language.repo_suffix}"
  end

  def files(template_dir)
    @file_mappings.map do |mapping|
      template_contents = File.read(File.join(template_dir, mapping.template_path))
      {
        path: mapping.destination_path,
        contents: Mustache.render(template_contents, template_context),
        is_executable: mapping.is_executable,
      }
    end
  end

  private

  def template_context
    {
      "language_name": @language.name,
      "language_slug": @language.slug,
      "language_is_#{@language.slug}": true,
      "course_name": @course.name,
      "course_slug": @course.name,
    }.merge(@template_attrs)
  end
end
