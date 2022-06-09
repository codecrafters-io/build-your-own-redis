class Unindenter
  def self.unindent(string)
    largest_tab_indent = string.scan(/^\t*(?=\S)/).map(&:length).max
    largest_space_indent = string.scan(/^ *(?=\S)/).map(&:length).max

    string
      .lines
      .map { |line|
        line[(largest_space_indent || largest_tab_indent || 0)..-1]&.strip || ""
      }
      .join("\n")
  end
end
