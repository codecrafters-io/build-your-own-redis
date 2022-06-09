class Uncommenter
  POUND_SIGN = /(^\s*)(#\s{0,1})/
  DOUBLE_SLASHES = /(^\s*)(\/\/\s{0,1})/
  DOUBLE_HYPHENS = /(^\s*)(\-\-\s{0,1})/

  REGEX_PATTERNS = {
    "c" => DOUBLE_SLASHES,
    "clojure" => /(^\s*)(;;\s{0,1})/,
    "csharp" => DOUBLE_SLASHES,
    "elixir" => POUND_SIGN,
    "go" => DOUBLE_SLASHES,
    "haskell" => DOUBLE_HYPHENS,
    "java" => DOUBLE_SLASHES,
    "javascript" => DOUBLE_SLASHES,
    "kotlin" => DOUBLE_SLASHES,
    "nim" => POUND_SIGN,
    "php" => DOUBLE_SLASHES,
    "python" => POUND_SIGN,
    "ruby" => POUND_SIGN,
    "rust" => DOUBLE_SLASHES,
    "crystal" => POUND_SIGN
  }

  attr_reader :language, :code, :uncomment_marker_pattern

  def initialize(language, code, uncomment_marker_pattern)
    @language = language
    @code = code
    @uncomment_marker_pattern = uncomment_marker_pattern
  end

  def uncommented
    self.code
      .lines
      .map { |line| line[0..-2] }
      .each_with_index
      .map { |line, index|
        within_uncomment_bounds(index) ? uncomment_line(line) : line
      }
      .join("\n") + "\n"
  end

  def uncomment_line(line)
    matches = line.match(line_regex)
    uncommented = line.sub(matches[2], "")
    uncommented.strip.eql?("") ? "" : uncommented
  end

  def within_uncomment_bounds(index)
    self.uncomment_bounds_pairs.any? do |uncomment_bounds|
      (index >= uncomment_bounds[0]) && (index <= uncomment_bounds[1])
    end
  end

  def uncomment_bounds_pairs
    self.uncomment_line_indices.map do |uncomment_line_index|
      start_index = uncomment_line_index + 1
      end_index = start_index - 1

      code.lines.each_with_index do |line, index|
        next if index < start_index

        unless !!line_regex.match(line)
          break
        end

        end_index = index
      end

      [start_index, end_index]
    end
  end

  def uncomment_line_indices
    code
      .lines
      .each_with_index
      .select { |line, index| line_regex.match?(line) && uncomment_marker_pattern.match?(line) }
      .map { |line, index| index }
  end

  def line_regex
    REGEX_PATTERNS.fetch(language)
  end
end
