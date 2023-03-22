use proc_macro2::LineColumn;

#[derive(Debug)]
pub struct Text<'a> {
    pub str: &'a str,
    line_offsets: LineOffsets,
}
impl<'a> Text<'a> {
    pub fn new(str: &'a str) -> Self {
        let line_offsets = LineOffsets::new(str);
        Self { str, line_offsets }
    }
    pub fn offset_of(&self, line_column: LineColumn) -> usize {
        let offset = self.offset_of_line(line_column.line);
        let s = &self.str[offset..];
        if let Some((index, _)) = s.char_indices().nth(line_column.column) {
            offset + index
        } else {
            self.str.len()
        }
    }
    pub fn offset_of_line(&self, line: usize) -> usize {
        self.line_offsets.0[line - 1]
    }
}

#[derive(Debug)]
struct LineOffsets(Vec<usize>);

impl LineOffsets {
    fn new(input: &str) -> Self {
        let mut offsets = vec![0];
        let mut is_newline = false;
        for (i, c) in input.char_indices() {
            match (c, is_newline) {
                ('\n', _) => {
                    offsets.push(i + 1);
                    is_newline = false;
                }
                ('\r', _) => {
                    if is_newline {
                        offsets.push(i + 1);
                    } else {
                        is_newline = true;
                    }
                }
                (_, true) => {
                    offsets.push(i);
                    is_newline = false;
                }
                (_, false) => {}
            }
        }
        if is_newline {
            offsets.push(input.len());
        }
        offsets.push(input.len());
        Self(offsets)
    }
}
