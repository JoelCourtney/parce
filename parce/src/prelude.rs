//! All the usual imports that the user will need to use.
//!
//! This is not the same as the prelude used by the generated code. Users should this this module,
//! (e.g. `use parce::prelude::*;`), while the generated code uses the internal prelude,
//! (e.g. `use parce::internal_prelude::*;`).

/// Generates a lexer enum and implements the [Lexer](crate::lexer::Lexer) trait for it.
///
/// Must be applied to an enum with patterns as the discriminants. Requires name of lexer to be
/// passed as argument. You will rarely (if ever) need to use the lexer directly,
/// and you can find those docs on the Lexer trait in the main crate.
///
/// This macro also generates a ParserSubmission struct, which has to be publicly accessible to the user,
/// but they shouldn't use it because it is only for internal use (it registers parsers through the
/// inventory crate).
///
/// # Examples
///
/// ## Basic Example
///
/// This lexer matches directly onto the characters 'a', 'b', and the words "Hello World!". Some valid inputs
/// to this lexer would be "ab", "aHello World!b", "abbabbbbaaababHello World!", etc. Notice how whitespace
/// will not be valid unless explicitly allowed inside one of the rules.
///
/// ```
/// use parce::prelude::*; // omitted in future examples
/// #[lexer(BasicLexer)]
/// enum BasicLexemes {
///     A = "'a'",
///     B = 'b',
///     HelloWorld = " 'Hello World!' "
/// }
///
/// assert!(BasicLexer::default().lex("aHello World!b").is_ok());
/// ```
///
/// Typically, all patterns must be surrounded by "double quotes", and all literal strings inside the patterns are
/// surrounded by 'single quotes'. *However* in the special case where the lexeme pattern is a single character,
/// the double quotes can be omitted (as in the B lexeme above).
///
/// ## Or operator
///
/// Lexemes can match on multiple different character sequences. One way to specify that is through
/// the pipe ( | ) "or" operator:
///
/// ```
/// # use parce::prelude::*;
/// #[lexer(OrLexer)]
/// enum OrExampleLexemes {
///     Hello = " 'world' | 'there' ",
///     ValOrVar = " 'va' ('l' | 'r') ",
/// }
///
/// assert!(OrLexer::default().lex("world").is_ok());
/// assert!(OrLexer::default().lex("var").is_ok());
/// ```
///
/// The Hello lexeme would match on either "world" or "there", and the ValOrVar lexeme would match on either
/// "val" or "var". Notice the use of parenthesis in the ValOrVar lexeme.
///
/// ## Character Classes
///
/// In cases where you want to match on many possible characters, using | can be extremely tedious. Instead,
/// you can use character classes like in regex. In fact, any character classes you use are forwarded
/// directly to the [regex] crate, so you can use all of the features available there. See [their docs](https://docs.rs/regex/1.5.4/regex/index.html#character-classes).
///
/// ```
/// # use parce::prelude::*;
/// #[lexer(ClassLexer)]
/// enum ClassLexemes {
///     Digit = " [0-9] ",
///     Alpha = " [[:alpha:]] "
/// }
/// ```
///
/// ## Repetition Operators
///
/// You can require or allow repetitions with the usual operators:
/// - `*`: Any number
/// - `+`: one or more
/// - `?`: one or one
/// - `{n}`: exactly n
/// - `{n,}`: n or more
/// - `{n,m}`: between n and m (inclusive)
///
/// ```
/// # use parce::prelude::*;
/// #[lexer(RepeatLexer)]
/// enum RepeatLexemes {
///     AB = " 'a'  ' '*  'b' ",
///     Integer = " [0-9]+ ",
///     // ...etc
/// }
/// ```
///
/// ## Skipped lexemes
///
/// Some lexemes can be skipped, meaning that if they are matched and would be added to the lexemes
/// output, the lexer instead just moves on. This is helpful for whitespace:
///
/// ```
/// # use parce::prelude::*;
/// #[lexer(SkipLexer)]
/// enum SkipLexemes {
///     #[skip] Whitespace = " [ \n\r\t] ",
///     A = 'a',
///     B = 'b'
/// }
/// ```
///
/// This lexer will act like all spaces, tabs, newlines, and carraige returns were removed from the input
/// string. *However* if, during parsing, you capture a portion of the input string in the output,
/// any characters associated with skipped lexemes inside that portion will still be present.
///
/// ## Nested Lexemes
///
/// Lexemes can require other lexemes. This can be helpful to avoid code reuse.
///
/// ```
/// # use parce::prelude::*;
/// #[lexer(NestLexer)]
/// enum NestLexemes {
///     Integer = " [0-9]+ ",
///     Float = " Integer? '.' Integer | Integer '.' "
/// }
/// assert!(NestLexer::default().lex("0.1").is_ok());
/// assert!(NestLexer::default().lex(".1").is_ok());
/// assert!(NestLexer::default().lex("0.").is_ok());
/// ```
///
/// Recursion is usually fine, but left recursive lexemes will not lex, and instead will stack overflow.
/// Your lexers are not checked for left recursion. An example is:
///
/// ```should_panic
/// # use parce::prelude::*;
/// #[lexer(BadLexer)]
/// enum BadLexemes {
///     RecurseLeft = " 'a' | RecurseLeft 'b' ", // <- don't be like this guy
/// }
///
/// // PANICS due to stack overflow!
/// BadLexer::default().lex("baaa");
/// ```
///
/// Technically this is valid syntax, and defines an unambiguous lexer, but in practice it will get stuck
/// in a loop.
///
/// ## Lexeme fragments
///
/// Lexeme fragments are lexemes that will not be matched on their own, but can be nested into other lexemes.
///
/// ```
/// # use parce::prelude::*;
/// #[lexer(FragmentLexer)]
/// enum FragmentLexemes {
///     #[frag] FragA = 'a',
///     AB = " FragA 'b' "
/// }
///
/// assert!(FragmentLexer::default().lex("ab").is_ok());
/// assert!(FragmentLexer::default().lex("a").is_err());
/// ```
///
/// Adding `#[skip]` to a lexeme that is already `#[frag]` will do nothing, and it will behave like a
/// normal fragment.
///
/// ## Modal Lexers
///
/// Lexers can have multiple modes. So far, all of the examples have been single-mode lexers, but you
/// can have as many modes as you like. First, you declare the modes you want to use above the enum.
/// The first mode in the list is the default mode that your lexer will start in.
///
/// Then, for each lexeme, you can assign it to one or modes, and it will only be matched if the lexer
/// is in one of those modes. Also, upon matching a given lexeme you can set the lexer to another mode
/// (which takes effect *after* the current lexeme is matched).
///
/// The mode attribute applies to the lexeme it is applied to and all further lexemes until you
/// use another mode attribute. Lexemes at the top of the enum that don't have a mode applied to them
/// will be in the default mode.
///
/// ```
/// # use parce::prelude::*;
/// #[lexer(ModalLexer)]
/// #[modes(Mode0, Mode1)] // Mode0 is the default
/// enum ModalLexemes {
///     A = 'a', // only in Mode0
///     #[set_mode(Mode1)] B = 'b', // only in Mode0; sets mode to Mode1 after being matched
///
///     #[mode(Mode1)]
///     C = 'c', // only in Mode1
///     #[set_mode(Mode0)] D = 'd', // only in Mode1; sets mode back to Mode0 after being matched
///
///     #[mode(Mode0, Mode1)]
///     E = 'e', // matched in either mode
///     #[skip] Whitespace = " [ \n\t\r] " // matched in either mode, and skipped
/// }
///
/// assert!(ModalLexer::default().lex("aeab ccecd ab cd").is_ok());
/// //                      Modes:     1--- 2---- 1- 2-
///
/// assert!(ModalLexer::default().lex("aaa c").is_err())
/// //                      Modes:     1-- x  fails because mode was not set to Mode1 before the c.
/// ```
///
/// Applying `#[mode]` or `#[set_mode]` to a fragment lexeme will do nothing. Fragments do not have modes,
/// they can be used in any mode that has a lexeme that requires them. They also cannot set a new mode
/// because they are never matched directly.
pub use parce_macros::lexer;

/// Generates an implementation of the [Parseable](crate::parser::Parseable) and [FromStr](std::str::FromStr)
/// traits for an enum.
///
/// Must be applied to an enum with patterns as the discriminants. Requires name of *lexer* to be
/// passed as argument.
///
/// # Examples
///
/// ## Basic Example
///
///
pub use parce_macros::parser;

pub use crate::lexer::Lexer;
pub use crate::parser::Parse;
pub use crate::parser::ParseCompletion;
pub use crate::error::{ParceError};