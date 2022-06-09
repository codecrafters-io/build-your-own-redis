require_relative "uncommenter"
require "diffy"

class UncommentMarkerNotFound < Exception
  def initialize(marker_pattern, files)
    super <<~EOF
      Didn't find a line that matches #{marker_pattern.inspect} in any of these files: #{files}.
    EOF
  end
end

class StarterCodeUncommenter
  attr_reader :dir, :language_slug

  UNCOMMENT_MARKER_PATTERN = /Uncomment this/

  def initialize(dir, language_slug)
    @dir = dir
    @language_slug = language_slug
  end

  def uncomment
    raise "No code files found" if code_files.empty?

    diffs = code_files.map do |file_path|
      old_contents = File.read(file_path)
      new_contents = Uncommenter.new(
        language_slug,
        old_contents,
        UNCOMMENT_MARKER_PATTERN
      ).uncommented

      next nil if old_contents == new_contents

      File.write(file_path, new_contents)
      post_processors.each { |post| post.call(file_path) }

      new_contents = File.read(file_path)
      Diffy::Diff.new(old_contents, new_contents)
    end.compact

    raise UncommentMarkerNotFound.new(UNCOMMENT_MARKER_PATTERN, code_files) if diffs.empty?

    diffs
  end

  def post_processors
    {
      # Imports are commented using the regular mechanism now.
      #
      # "go" => [
      #   Proc.new { |file_path|
      #     `goimports -w #{file_path}`
      #     if $? != 0
      #       raise RuntimeError.new("Running goimports failed")
      #     end
      #   },
      # ]
    }.fetch(language_slug, [])
  end

  def code_files
    Dir["#{dir}/**/*.#{code_file_extension}"]
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
    }.fetch(language_slug)
  end
end
