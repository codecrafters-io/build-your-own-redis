require "diffy"

class LineMarkerNotFound < Exception
  def initialize(marker_pattern, files)
    super <<~EOF
      Didn't find a line that matches #{marker_pattern.inspect} in any of these files: #{files}.
    EOF
  end
end

class LineWithCommentRemover
  attr_reader :dir, :language

  LINE_MARKER_PATTERN = /You can use print/

  def initialize(dir, language)
    @dir = dir
    @language = language
  end

  def process!
    raise "No code files found" if code_files.empty?

    diffs = code_files.map do |file_path|
      old_contents = File.read(file_path)
      new_contents = process_file_contents(old_contents)

      next nil if old_contents == new_contents

      File.write(file_path, new_contents)

      new_contents = File.read(file_path)
      Diffy::Diff.new(old_contents, new_contents)
    end.compact

    raise LineMarkerNotFound.new(LINE_MARKER_PATTERN, code_files) if diffs.empty?

    diffs
  end

  def code_files
    Dir["#{dir}/**/*.#{code_file_extension}"]
  end

  def process_file_contents(old_contents)
    old_lines = old_contents.split("\n")
    marker_index = old_lines.each_with_index.find { |line, _| line.match(LINE_MARKER_PATTERN) }.first

    old_lines.delete_at(marker_index) # Delete marker line
    old_lines.delete_at(marker_index) # Delete line after

    if /^\s*$/.match?(old_lines[marker_index])
      old_lines.delete_at(marker_index) # Delete blank line after
    end

    old_lines.join("\n")
  end
end
