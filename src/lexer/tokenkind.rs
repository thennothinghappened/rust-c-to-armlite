use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum TokenKind<'a> {
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
    Eof,
}

impl<'a> TryFrom<TokenKind<'a>> for &'static str {
    type Error = ();

    fn try_from(value: TokenKind<'a>) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Semicolon => ";",
            TokenKind::OpenParen => "(",
            TokenKind::CloseParen => ")",
            TokenKind::OpenCurly => "{",
            TokenKind::CloseCurly => "}",
            TokenKind::Ampersand => "&",
            TokenKind::Star => "*",
            TokenKind::Comma => ",",
            TokenKind::Assign => "=",
            TokenKind::Plus => "+",
            TokenKind::PlusPlus => "++",
            TokenKind::Minus => "-",
            TokenKind::MinusMinus => "--",
            TokenKind::BooleanEqual => "==",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::Return => "return",
            TokenKind::Struct => "struct",
            TokenKind::Union => "union",
            TokenKind::TypeDef => "typedef",
            TokenKind::Enum => "enum",
            TokenKind::Unsigned => "unsigned",
            TokenKind::Signed => "signed",
            TokenKind::Bool => "bool",
            TokenKind::Int => "int",
            TokenKind::Long => "long",
            TokenKind::Short => "short",
            TokenKind::Char => "char",
            TokenKind::Float => "float",
            TokenKind::Double => "double",
            TokenKind::Void => "void",
            TokenKind::SizeOf => "sizeof",
            TokenKind::Const => "const",
            TokenKind::QuestionMark => "?",
            TokenKind::Colon => ":",

            TokenKind::StringLiteral(_)
            | TokenKind::IntLiteral(_)
            | TokenKind::Ident(_)
            | TokenKind::Unknown(_) => return Err(()),

            TokenKind::Eof => "<<End Of File>>",
        })
    }
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(str) = <&'static str>::try_from(*self) {
            return write!(f, "{str}");
        }

        match self {
            TokenKind::StringLiteral(content) => write!(f, "\"{content}\""),
            TokenKind::IntLiteral(int) => write!(f, "{int}"),
            TokenKind::Ident(name) => write!(f, "Ident({name})"),
            TokenKind::Unknown(char) => write!(f, "Unknown({char})"),
            _ => panic!("unhandled case {self:?}, somebody forgot to add it"),
        }
    }
}
