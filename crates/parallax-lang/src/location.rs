#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    pub file_id: usize,
    pub range: Range,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl Range {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn from_byte_span(source: &str, start_byte: usize, end_byte: usize) -> Self {
        let mut start_line = 1;
        let mut start_column = 1;
        let mut end_line = 1;
        let mut end_column = 1;

        for (idx, c) in source.char_indices() {
            if idx == start_byte {
                start_line = end_line;
                start_column = end_column;
            }
            if idx == end_byte {
                break;
            }
            if c == '\n' {
                end_line += 1;
                end_column = 1;
            } else {
                end_column += 1;
            }
        }

        Self {
            start: Position::new(start_line, start_column),
            end: Position::new(end_line, end_column),
        }
    }
}

impl Location {
    pub fn new(file_id: usize, range: Range) -> Self {
        Self { file_id, range }
    }
} 