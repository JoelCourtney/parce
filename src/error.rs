use std::fmt::Formatter;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParceError {
    pub input: String,
    pub start: usize,
    pub phase: ParcePhase
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParcePhase {
    Lex(String),
    Parse(ParsePhaseFailure),
    Assemble
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParsePhaseFailure {
    InputEndedTooSoon,
    NoMatches,
    LeftoverLexemes,
    NothingToParse
}

impl std::fmt::Display for ParceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        match &self.phase {
            ParcePhase::Lex(mode) => {
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