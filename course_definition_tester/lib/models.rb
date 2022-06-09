require "yaml"

class CourseStage
  attr_reader :slug

  def initialize(slug:)
    @slug = slug
  end
end

class Course
  attr_reader :slug
  attr_reader :name
  attr_reader :stages

  def initialize(slug:, name:, stages:)
    @slug = slug
    @name = name
    @stages = stages
  end

  def first_stage
    stages.first
  end

  def self.load_from_file(file_path)
    course_definition_yaml = YAML.load_file(file_path)

    new(
      name: course_definition_yaml.fetch("name"),
      slug: course_definition_yaml.fetch("slug"),
      stages: course_definition_yaml.fetch("stages").map { |stage_yaml| CourseStage.new(slug: stage_yaml.fetch("slug")) }
    )
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

  def self.find_by_slug!(slug)
    require_relative "languages"
    LANGUAGES.detect(-> { raise "Language with slug #{slug} not found" }) { |language| language.slug.eql?(slug) }
  end

  def code_file_extension
    {
      "c" => "c",
      "clojure" => "clj",
      "crystal" => "cr",
      "csharp" => "cs",
      "elixir" => "ex",
      "go" => "go",
      "haskell" => "hs",
      "java" => "java",
      "javascript" => "js",
      "kotlin" => "kt",
      "nim" => "nim",
      "php" => "php",
      "python" => "py",
      "ruby" => "rb",
      "rust" => "rs"
    }.fetch(@slug)
  end
end
