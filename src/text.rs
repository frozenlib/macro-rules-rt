use proc_macro2::LineColumn;

pub(crate) struct Text {
    str: String,
    line_offsets: LineOffsets,
}
impl Text {
    pub fn new(str: String) -> Self {
        let line_offsets = LineOffsets::new(&str);
        Self { str, line_offsets }
    }
    pub fn get(&self, start: usize, end: usize) -> &str {
        &self.str[start..end]
    }
    pub fn offset_of(&self, line_column: LineColumn) -> usize {
        self.line_offsets.0[line_column.line - 1] + line_column.column - 1
    }
}

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
