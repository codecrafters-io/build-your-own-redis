require "mustache"
require "yaml"

class FileMapping
  attr_reader :destination_path
  attr_reader :template_path

  def initialize(destination_path, template_path)
    @destination_path = destination_path
    @template_path = template_path
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

  def self.load_from_files(course_definition_file_path, starter_definitions_file_path)
    course = Course.load_from_file(course_definition_file_path)
    starter_definitions_yaml = YAML.load_file(starter_definitions_file_path)

    starter_definitions_yaml.map do |starter_definition_yaml|
      StarterRepoDefinition.new(
        course: course,
        file_mappings: starter_definition_yaml.fetch("file_mappings").map { |fm| FileMapping.new(fm.fetch("target"), fm.fetch("source")) },
        language: LANGUAGES.detect { |language| language.slug == starter_definition_yaml.fetch("language") },
        template_attrs: starter_definition_yaml.fetch("template_attributes")
      )
    end
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
        is_executable: File.executable?(File.join(template_dir, mapping.template_path))
      }
    end
  end

  private

  def template_context
    {
      language_name: @language.name,
      language_slug: @language.slug,
      course_name: @course.name,
      course_slug: @course.name
    }.merge(@template_attrs)
  end
end
