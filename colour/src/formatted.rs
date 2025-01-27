use std::fmt::Display;

#[derive(Default)]
pub struct Formatted {
    text: String,
    bold: bool,
    italicised: bool,
    colour: Option<Colour>,
}

/// Four bit colours supported by the terminal
pub enum Colour {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
}

impl Formatted {
    pub fn from(text: String) -> Self {
        Self {
            text,
            ..Default::default()
        }
    }

    pub fn bold(self) -> Self {
        Self { bold: true, ..self }
    }

    pub fn italics(self) -> Self {
        Self {
            italicised: true,
            ..self
        }
    }

    pub fn colour(self, colour: Colour) -> Self {
        Self {
            colour: Some(colour),
            ..self
        }
    }
}

/// Converts to stuff that is comptible with the terminal. The table code and format was found
/// here: https://stackoverflow.com/a/33206814
impl Display for Formatted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start_escape_code = "\x1b[";
        let end_escape_code = "m";
        let reset = "\x1b[0m"; // so that whatever outputted after that is normal

        let mut codes = vec![];
        if self.bold {
            codes.push("1");
        }
        if self.italicised {
            codes.push("3");
        }
        if let Some(colour) = &self.colour {
            match colour {
                Colour::Red => codes.push("31"),
                Colour::Green => codes.push("32"),
                Colour::Blue => codes.push("34"),
                Colour::Yellow => codes.push("33"),
                Colour::Black => codes.push("30"),
                Colour::Magenta => codes.push("35"),
                Colour::Cyan => codes.push("36"),
                Colour::White => codes.push("37"),
            }
        }

        write!(
            f,
            "{start_escape_code}{}{end_escape_code}{}{reset}",
            codes.join(";"),
            self.text
        )
    }
}
