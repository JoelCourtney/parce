use crate::common::{ParceMacroError, RangeRuleMax};
use crate::lexer::LexerPattern;
use crate::parser::ParserPattern;
use crate::COUNT_PARSER;

pub(crate) fn lexer_discriminant(s: String) -> Result<LexerPattern, ParceMacroError> {
    let chars: Vec<char> = s.chars().collect();

    // Search for |
    let mut i = 0;
    let mut in_string = false;
    let mut group_depth = 0;
    let mut class_depth = 0;
    let mut splits = vec![-1_i32];
    while i < s.len() {
        match chars[i] {
            '(' if !in_string && class_depth == 0 => group_depth += 1,
            ')' if !in_string && class_depth == 0 => group_depth -= 1,
            '[' if !in_string && (i == 0 || chars[i-1] != '\\' || class_depth == 0) => class_depth += 1,
            ']' if !in_string && chars[i-1] != '\\' => class_depth -= 1,
            '\'' if (!in_string && class_depth == 0) || (in_string && chars[i-1] != '\\') => {
                in_string = !in_string;
            }
            '|' if !in_string && class_depth == 0 && group_depth == 0 => {
                splits.push(i as i32);
            }
            _ => {}
        }
        i += 1;
    }
    if splits.len() > 1 {
        splits.push(s.len() as i32);
        let mut options = vec![];
        for j in 0..splits.len()-1 {
            options.push(lexer_discriminant(s[(splits[j]+1) as usize..splits[j+1] as usize].to_string())?);
        }
        return Ok(LexerPattern::Or(options));
    }

    // Generate other lexemes. If just one return it, otherwise return And.
    let mut result = vec![];
    i = 0;
    while i < s.len() {
        match chars[i] {
            '\'' => {
                let mut j = i + 1;
                while j < s.len() && chars[j] != '\'' && chars[j-1] != '\\' {
                    j += 1;
                }
                if j != s.len() {
                    result.push(LexerPattern::Literal(s[i+1..j].to_string()));
                    i = j;
                } else {
                    return Err(ParceMacroError(Box::new(s), "reached end of pattern before string was closed".to_string()));
                }
            }
            '[' => {
                let mut j = i + 1;
                let mut class_depth = 1;
                while j < s.len() {
                    match chars[j] {
                        '[' if chars[j-1] != '\\' => class_depth += 1,
                        ']' if chars[j-1] != '\\' => {
                            class_depth -= 1;
                            if class_depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    j += 1;
                }
                if j != s.len() {
                    result.push(LexerPattern::Class(s[i..j+1].to_string()));
                    i = j;
                }
            }
            '(' => {
                let mut j = i + 1;
                let mut class_depth: u32 = 0;
                let mut group_depth: u32 = 1;
                let mut in_string = false;
                while j < s.len() {
                    match chars[j] {
                        '(' if !in_string && class_depth == 0 => group_depth += 1,
                        ')' if !in_string && class_depth == 0 => {
                            group_depth -= 1;
                            if group_depth == 0 {
                                break;
                            }
                        }
                        '[' if !in_string && (chars[j-1] != '\\' || class_depth == 0) => class_depth += 1,
                        ']' if !in_string && chars[j-1] != '\\' => class_depth -= 1,
                        '\'' if (!in_string && class_depth == 0) || (in_string && chars[j-1] != '\\') => {
                            in_string = !in_string;
                        }
                        _ => {}
                    }
                    j += 1;
                }
                if j != s.len() {
                    result.push(lexer_discriminant(s[i+1..j].to_string())?);
                    i = j;
                } else {
                    return Err(ParceMacroError(Box::new(s), "reached end of string before () group was closed".to_string()));
                }
            }
            '{' => {
                match result.pop() {
                    Some(prev) => {
                        let mut j = i + 1;
                        while j < s.len() {
                            match chars[j] {
                                '}' => break,
                                _ => j += 1
                            }
                        }
                        if j != s.len() {
                            let captures = COUNT_PARSER.captures(&s[i..j + 1]);
                            match captures {
                                Some(cap) => {
                                    result.push(
                                        LexerPattern::Range(
                                            Box::new(prev),
                                            cap[1].parse().unwrap(),
                                            match cap.get(2) {
                                                Some(s) => RangeRuleMax::Some(s.as_str().parse().unwrap()),
                                                None => {
                                                    if s[i..j+1].contains(',') {
                                                        RangeRuleMax::Infinite
                                                    } else {
                                                        RangeRuleMax::Fixed
                                                    }
                                                }
                                            }
                                        )
                                    );
                                }
                                None => return Err(ParceMacroError(Box::new(s), "invalid counter operator".to_string()))
                            }
                            i = j;
                        }
                    }
                    None => return Err(ParceMacroError(Box::new(s), "{} was applied to nothing".to_string()))
                }
            }
            c if c.is_alphabetic() => {
                let mut j = i + 1;
                while j < s.len() && chars[j].is_alphanumeric() {
                    j += 1;
                }
                result.push(LexerPattern::Lexeme(s[i..j].to_string()));
                i = j - 1;
            }
            '.' => result.push(LexerPattern::Dot),
            '*' => match result.pop() {
                Some(l) => result.push(LexerPattern::Star(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "* was applied to nothing".to_string()))
            },
            '+' => match result.pop() {
                Some(l) => result.push(LexerPattern::Plus(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "+ was applied to nothing".to_string()))
            },
            '?' => match result.pop() {
                Some(l) => result.push(LexerPattern::Question(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "? was applied to nothing".to_string()))
            },
            c if c.is_whitespace() => {}
            c => return Err(ParceMacroError(Box::new(s), format!("{} is not a valid beginning to any lexer pattern", c)))
        }
        i += 1;
    }
    if result.len() == 0 {
        Err(ParceMacroError(Box::new(s), "this shouldn't be possible".to_string()))
    } else if result.len() == 1 {
        Ok(result.remove(0))
    } else {
        Ok(LexerPattern::And(result))
    }
}

pub(crate) fn parser_pattern(s: String) -> Result<ParserPattern, ParceMacroError> {
    let chars: Vec<char> = s.chars().collect();

    let mut i = 0;
    let mut splits = vec![-1];
    let mut group_depth = 0;
    while i < s.len() {
        match chars[i] {
            '(' => group_depth += 1,
            ')' => group_depth -= 1,
            '|' if group_depth == 0 => {
                splits.push(i as i32);
            }
            _ => {}
        }
        i += 1;
    }
    if splits.len() > 1 {
        splits.push(s.len() as i32);
        let mut options = vec![];
        for j in 0..splits.len()-1 {
            options.push(parser_pattern(s[(splits[j]+1) as usize..splits[j+1] as usize].to_string())?);
        }
        return Ok(ParserPattern::Or(options));
    }

    let mut result = vec![];
    i = 0;
    while i < s.len() {
        match chars[i] {
            '#' => {
                let mut j = i + 1;
                while j < s.len() && (chars[j].is_alphanumeric() || chars[j] == ':') {
                    j += 1;
                }
                result.push(ParserPattern::Rule(s[i+1..j].to_string()));
                i = j - 1;
            }
            '(' => {
                let mut j = i + 1;
                let mut group_depth: u32 = 1;
                while j < s.len() {
                    match chars[j] {
                        '(' => group_depth += 1,
                        ')' => {
                            group_depth -= 1;
                            if group_depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    j += 1;
                }
                if j != s.len() {
                    result.push(parser_pattern(s[i+1..j].to_string())?);
                    i = j;
                } else {
                    return Err(ParceMacroError(Box::new(s), "reached end of string before () group was closed".to_string()));
                }
            }
            c if c.is_alphabetic() => {
                let mut j = i + 1;
                while j < s.len() && (chars[j].is_alphanumeric() || chars[j] == '_') {
                    j += 1;
                }
                let name = s[i..j].to_string();
                if c.is_uppercase() {
                    result.push(ParserPattern::Lexeme(name));
                    i = j - 1;
                } else {
                    if j < s.len() - 1 && chars[j] == '=' {
                        let mut k = j + 2;
                        match chars[j + 1] {
                            c if c.is_alphabetic() || c == '#' => {
                                while k < s.len() {
                                    if chars[k].is_alphabetic() {
                                        k += 1;
                                    } else {
                                        break
                                    }
                                }
                            }
                            '(' => {
                                let mut group_depth = 1;
                                while k < s.len() {
                                    match chars[k] {
                                        '(' => group_depth += 1,
                                        ')' => {
                                            group_depth -= 1;
                                            if group_depth == 0 {
                                                break;
                                            }
                                        }
                                        _ => {}
                                    }
                                    k += 1;
                                }
                            }
                            other => return Err(ParceMacroError(Box::new(s.clone()), format!("'{}' is not valid after =", other)))
                        }
                        result.push(ParserPattern::AssignNamedField(name, Box::new(parser_pattern(s[j+1..k].to_string())?)));
                        i = k - 1;
                    } else {
                        result.push(ParserPattern::BareNamedField(name));
                        i = j - 1;
                    }
                }
            }
            c if c.is_numeric() => {
                let mut j = i + 1;
                while j < s.len() && chars[j].is_numeric() {
                    j += 1;
                }
                let name = match s[i..j].parse() {
                    Ok(n) => n,
                    Err(_) => panic!("how even")
                };
                if j < s.len() && chars[j] == '=' {
                    let mut k = j + 1;
                    let mut group_depth: u32 = 0;
                    while k < s.len() {
                        match chars[k] {
                            '(' => group_depth += 1,
                            ')' => group_depth -= 1,
                            c if c.is_whitespace() && group_depth == 0 => break,
                            _ => {}
                        }
                        k += 1;
                    }
                    result.push(ParserPattern::AssignUnnamedField(name, Box::new(parser_pattern(s[j+1..k].to_string())?)));
                    i = k - 1;
                } else {
                    result.push(ParserPattern::BareUnnamedField(name));
                    i = j - 1;
                }
            }
            '{' => {
                match result.pop() {
                    Some(prev) => {
                        let mut j = i + 1;
                        while j < s.len() {
                            match chars[j] {
                                '}' => break,
                                _ => j += 1
                            }
                        }
                        if j != s.len() {
                            let captures = COUNT_PARSER.captures(&s[i..j + 1]);
                            match captures {
                                Some(cap) => {
                                    result.push(
                                        ParserPattern::Range(
                                            Box::new(prev),
                                            cap[1].parse().unwrap(),
                                            match cap.get(2) {
                                                Some(s) => RangeRuleMax::Some(s.as_str().parse().unwrap()),
                                                None => {
                                                    if s[i..j+1].contains(',') {
                                                        RangeRuleMax::Infinite
                                                    } else {
                                                        RangeRuleMax::Fixed
                                                    }
                                                }
                                            }
                                        )
                                    );
                                }
                                None => return Err(ParceMacroError(Box::new(s), "invalid counter operator".to_string()))
                            }
                            i = j;
                        }
                    }
                    None => return Err(ParceMacroError(Box::new(s), "{} was applied to nothing".to_string()))
                }
            }
            '.' => result.push(ParserPattern::Dot),
            '*' => match result.pop() {
                Some(l) => result.push(ParserPattern::Star(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "* was applied to nothing".to_string()))
            },
            '+' => match result.pop() {
                Some(l) => result.push(ParserPattern::Plus(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "+ was applied to nothing".to_string()))
            },
            '?' => match result.pop() {
                Some(l) => result.push(ParserPattern::Question(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "? was applied to nothing".to_string()))
            },
            c if c.is_whitespace() => {}
            c => return Err(ParceMacroError(Box::new(s), format!("{} is not a valid beginning to any parser pattern", c)))
        }
        i += 1;
    }
    if result.len() == 0 {
        Err(ParceMacroError(Box::new(s), "this shouldn't be possible".to_string()))
    } else if result.len() == 1 {
        Ok(result.remove(0))
    } else {
        Ok(ParserPattern::And(result))
    }
}
