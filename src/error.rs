use std::fmt::Formatter;

/// Error struct for all runtime errors in the lexing and parsing process.
///
/// This struct will pretty-print the error message when printed through the Display trait.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParceError {
    /// The full input string
    pub input: String,
    /// The index in [input](Self::input) where the error occurred
    pub start: usize,
    /// Other info about the error
    pub info: ParceErrorInfo
}

/// Further infomation about a ParceError
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParceErrorInfo {
    /// Error occurred in the lexing phase, when the lexer was in the given mode.
    Lex {
        mode: String
    },
    /// Error occurred in the parsing phase, with a given failure.
    Parse {
        failure: ParsePhaseFailure
    },
    /// Error occurred in the assembly phase
    Assemble
}

impl ParceErrorInfo {
    /// Creates a new lex variant
    pub fn lex(mode: String) -> ParceErrorInfo {
        ParceErrorInfo::Lex {
            mode
        }
    }
    /// Creates a new parse variant
    pub fn parse(failure: ParsePhaseFailure) -> ParceErrorInfo {
        ParceErrorInfo::Parse {
            failure
        }
    }
    /// Creates a new assemble variant
    pub fn assemble() -> ParceErrorInfo {
        ParceErrorInfo::Assemble
    }
}

/// Possible failure modes during the parsing phase
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParsePhaseFailure {
    /// Some productions were still alive, but the input ended before any could be completed.
    InputEndedTooSoon,
    /// All productions failed to match the input.
    NoMatches,
    /// When using [parse_all](crate::reexports::Parse::parse_all), the longest match did not
    /// use all of the lexemes in the input.
    LeftoverLexemes,
    /// Input vec of lexemes was empty.
    NothingToParse
}

impl std::fmt::Display for ParceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        match &self.info {
            ParceErrorInfo::Lex {mode} => {
                let start = self.start.saturating_sub(10);
                let end = std::cmp::min(self.input.len(), self.start + 10);
                let short = format!("{}{}{}",
                                    if start != 0 {
                                        "..."
                                    } else {
                                        ""
                                    },
                                    &self.input[start..end],
                                    if end != self.input.len() {
                                        "..."
                                    } else {
                                        ""
                                    }
                );

                write!(
                    f,
                    "Lexer Error: {}\nLexer Mode: {}\nInput: {}{}\n{}{}",
                    "no possible lexemes matched this input".red(),
                    mode.bright_blue(),
                    &short[..self.start - if start != 0 { start - 3 } else { 0 }],
                    &short[(self.start - if start != 0 { start - 3 } else { 0 })..].red(),
                    " ".repeat(7 + self.start - if start != 0 { start - 3 } else { 0 }),
                    "^".red(),
                )
            }
            _ => todo!()
        }
    }
}

impl std::error::Error for ParceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> { None }
}