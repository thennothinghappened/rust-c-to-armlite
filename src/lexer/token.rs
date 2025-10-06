use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Token<'a> {
    Semicolon,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Ampersand,
    Comma,
    Star,
    Assign,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    BooleanEqual,
    If,
    Else,
    While,
    Return,
    Struct,
    Union,
    TypeDef,
    Enum,
    Const,
    Signed,
    Unsigned,
    Short,
    Bool,
    Int,
    Long,
    Char,
    Float,
    Double,
    Void,
    SizeOf,
    QuestionMark,
    Colon,
    StringLiteral(&'a str),
    IntLiteral(i32),
    Ident(&'a str),
    Unknown(char),
}

impl<'a> TryFrom<Token<'a>> for &'static str {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        Ok(match value {
            Token::Semicolon => ";",
            Token::OpenParen => "(",
            Token::CloseParen => ")",
            Token::OpenCurly => "{",
            Token::CloseCurly => "}",
            Token::Ampersand => "&",
            Token::Star => "*",
            Token::Comma => ",",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::PlusPlus => "++",
            Token::Minus => "-",
            Token::MinusMinus => "--",
            Token::BooleanEqual => "==",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::Return => "return",
            Token::Struct => "struct",
            Token::Union => "union",
            Token::TypeDef => "typedef",
            Token::Enum => "enum",
            Token::Unsigned => "unsigned",
            Token::Signed => "signed",
            Token::Bool => "bool",
            Token::Int => "int",
            Token::Long => "long",
            Token::Short => "short",
            Token::Char => "char",
            Token::Float => "float",
            Token::Double => "double",
            Token::Void => "void",
            Token::SizeOf => "sizeof",
            Token::Const => "const",
            Token::QuestionMark => "?",
            Token::Colon => ":",
            _ => return Err(()),
        })
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(str) = <&'static str>::try_from(*self) {
            return write!(f, "{str}");
        }

        match self {
            Token::StringLiteral(content) => write!(f, "\"{content}\""),
            Token::IntLiteral(int) => write!(f, "{int}"),
            Token::Ident(name) => write!(f, "Ident({name})"),
            Token::Unknown(char) => write!(f, "Unknown({char})"),
            _ => panic!("unhandled case {self:?}, somebody forgot to add it"),
        }
    }
}
