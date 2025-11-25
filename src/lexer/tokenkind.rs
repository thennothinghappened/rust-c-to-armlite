use std::fmt::Display;

use crate::lexer::Token;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum TokenKind {
    Semicolon,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    Ampersand,
    Comma,
    Star,
    Assign,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    BooleanEqual,
    LessThan,
    GreaterThan,
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
    StringLiteral(IdentId),
    IntLiteral(i32),
    Ident(IdentId),
    Unknown(char),
    Eof,

    /// Not a real source-code token! This will never be seen by the parser, and is an indication
    /// inside the lexer regarding what's going on.
    MacroExpansionMarker,
}

pub(crate) type IdentId = usize;

impl TryFrom<TokenKind> for &'static str {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Semicolon => ";",
            TokenKind::OpenParen => "(",
            TokenKind::CloseParen => ")",
            TokenKind::OpenCurly => "{",
            TokenKind::CloseCurly => "}",
            TokenKind::OpenSquare => "[",
            TokenKind::CloseSquare => "]",
            TokenKind::Ampersand => "&",
            TokenKind::Star => "*",
            TokenKind::Comma => ",",
            TokenKind::Assign => "=",
            TokenKind::Plus => "+",
            TokenKind::PlusPlus => "++",
            TokenKind::Minus => "-",
            TokenKind::MinusMinus => "--",
            TokenKind::BooleanEqual => "==",
            TokenKind::LessThan => "<",
            TokenKind::GreaterThan => ">",
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

            TokenKind::StringLiteral(_) => "string",
            TokenKind::Ident(_) => "identifier",

            TokenKind::IntLiteral(_) | TokenKind::Unknown(_) => return Err(()),

            TokenKind::MacroExpansionMarker => "<<Macro Expansion>>",
            TokenKind::Eof => "<<End Of File>>",
        })
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(str) = <&'static str>::try_from(*self) {
            return write!(f, "{str}");
        }

        match self {
            TokenKind::IntLiteral(int) => write!(f, "{int}"),
            TokenKind::Unknown(char) => write!(f, "Unknown({char})"),
            _ => panic!("unhandled case {self:?}, somebody forgot to add it"),
        }
    }
}
