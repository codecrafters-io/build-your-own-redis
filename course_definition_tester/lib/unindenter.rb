class Unindenter
  def self.unindent(string)
    smallest_tab_indent = string.scan(/^\t*(?=\S)/).map(&:length).min
    smallest_space_indent = string.scan(/^ *(?=\S)/).map(&:length).min

    string
      .lines
      .map { |line|
        if smallest_tab_indent
          line.sub(/^\t{#{smallest_tab_indent}}/, "")
        elsif smallest_space_indent
          line.sub(/^ {#{smallest_space_indent}}/, "")
        else
          line
        end
      }
      .map(&:rstrip)
      .join("\n")
  end
end
