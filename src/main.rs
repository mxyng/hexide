use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::time::{Duration, Instant};

use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    prelude::*,
    symbols,
    widgets::{
        Block, BorderType, Borders, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
    },
};

// Converts a path to a short representation, replacing the home directory with ~
pub fn get_short_path<P: AsRef<Path>>(path: P) -> String {
    let path = path.as_ref();

    // Get the home directory
    if let Ok(home) = env::var("HOME") {
        let home_path = Path::new(&home);

        // Check if the path starts with the home directory
        if let Some(path_str) = path.to_str() {
            if let Some(home_str) = home_path.to_str() {
                if path_str.starts_with(home_str) {
                    // Replace home directory with ~
                    return format!("~{}", &path_str[home_str.len()..]);
                }
            }
        }
    }

    // If we can't replace with ~, return the path as is
    path.to_string_lossy().into_owned()
}

enum Mode {
    Normal,
    Command(String),
}

struct Hexide {
    file_data: Vec<u8>,
    vertical_scroll: usize,
    horizontal_scroll: usize,
    filename: String,
    mode: Mode,
    highlighted_line: usize, // New field to track highlighted line
}

impl Hexide {
    fn new(filename: &str) -> Result<Self> {
        let path = Path::new(filename);
        let mut file = File::open(path).context("Failed to open file")?;
        let mut file_data = Vec::new();
        file.read_to_end(&mut file_data)
            .context("Failed to read file")?;

        Ok(Self {
            file_data,
            vertical_scroll: 0,
            horizontal_scroll: 0,
            filename: get_short_path(path),
            mode: Mode::Normal,
            highlighted_line: 0, // Start with the first line highlighted
        })
    }

    fn max_offset(&self) -> usize {
        format!("{:x}", self.file_data.len()).len().max(8)
    }

    // Calculate the total content width
    fn content_width(&self) -> usize {
        // 10 for offset, 3 per byte in hex, 2 for separator, 16 for ASCII, 2 for borders
        1 + self.max_offset() + 1 + 16 * 3 + 2 + 16 + 2 + 1
    }

    // Calculate the total number of rows in the file
    fn total_rows(&self) -> usize {
        (self.file_data.len() + 15) / 16 // Ceiling division by 16
    }

    // Calculate the maximum scroll position based on visible rows
    fn max_vertical_scroll(&self, visible_rows: usize) -> usize {
        let total = self.total_rows();
        if total <= visible_rows {
            0 // No scrolling needed if all content fits
        } else {
            total - visible_rows
        }
    }

    // Ensure highlighted line is visible by adjusting scroll if needed
    fn ensure_highlighted_visible(&mut self, visible_rows: usize) {
        // If highlighted line is before visible area, scroll up
        if self.highlighted_line < self.vertical_scroll {
            self.vertical_scroll = self.highlighted_line;
        }
        // If highlighted line is after visible area, scroll down
        else if self.highlighted_line >= self.vertical_scroll + visible_rows {
            self.vertical_scroll = self.highlighted_line.saturating_sub(visible_rows) + 1;
        }

        // Make sure we don't scroll past the end
        let max_scroll = self.max_vertical_scroll(visible_rows);
        self.vertical_scroll = self.vertical_scroll.min(max_scroll);
    }

    fn run(&mut self, terminal: &mut Terminal<impl Backend>) -> Result<()> {
        let mut last_tick = Instant::now();
        let tick_rate = Duration::from_millis(250);

        loop {
            terminal.draw(|f| self.ui(f))?;

            let timeout = tick_rate
                .checked_sub(last_tick.elapsed())
                .unwrap_or_else(|| Duration::from_secs(0));

            if crossterm::event::poll(timeout)? {
                if let Event::Key(key) = event::read()? {
                    if key.kind == KeyEventKind::Press {
                        let terminal_size = terminal.size()?;
                        let visible_rows = terminal_size.height.saturating_sub(2) as usize;
                        let visible_width = terminal_size.width.saturating_sub(4) as usize;

                        match self.mode {
                            Mode::Normal => match key.code {
                                KeyCode::Char('q') => return Ok(()),
                                KeyCode::Down => {
                                    // Move highlight down
                                    let max_line = self.total_rows().saturating_sub(1);
                                    self.highlighted_line =
                                        self.highlighted_line.saturating_add(1).min(max_line);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::Up => {
                                    // Move highlight up
                                    self.highlighted_line = self.highlighted_line.saturating_sub(1);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::Char('j') => {
                                    // Scroll down without moving highlight
                                    let max_scroll = self.max_vertical_scroll(visible_rows);
                                    self.vertical_scroll =
                                        self.vertical_scroll.saturating_add(1).min(max_scroll);
                                }
                                KeyCode::Char('k') => {
                                    // Scroll up without moving highlight
                                    self.vertical_scroll = self.vertical_scroll.saturating_sub(1);
                                }
                                KeyCode::Right | KeyCode::Char('l') => {
                                    let content_width = self.content_width();

                                    // Only scroll if content is wider than visible area
                                    if content_width > visible_width {
                                        let max_scroll =
                                            content_width.saturating_sub(visible_width);
                                        self.horizontal_scroll = self
                                            .horizontal_scroll
                                            .saturating_add(1)
                                            .min(max_scroll);
                                    }
                                }
                                KeyCode::Left | KeyCode::Char('h') => {
                                    self.horizontal_scroll =
                                        self.horizontal_scroll.saturating_sub(1);
                                }
                                KeyCode::PageDown => {
                                    // Move highlight down by a page
                                    let max_line = self.total_rows().saturating_sub(1);
                                    self.highlighted_line = self
                                        .highlighted_line
                                        .saturating_add(visible_rows)
                                        .min(max_line);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::PageUp => {
                                    // Move highlight up by a page
                                    let page_size = visible_rows;
                                    self.highlighted_line =
                                        self.highlighted_line.saturating_sub(page_size);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::Home => {
                                    self.highlighted_line = 0;
                                    self.vertical_scroll = 0;
                                    self.horizontal_scroll = 0;
                                }
                                KeyCode::End => {
                                    self.highlighted_line = self.total_rows().saturating_sub(1);
                                    self.vertical_scroll = self.max_vertical_scroll(visible_rows);
                                }
                                KeyCode::Char(':') => {
                                    self.mode = Mode::Command(String::new());
                                }
                                _ => {}
                            },
                            Mode::Command(ref mut cmd) => match key.code {
                                KeyCode::Esc => {
                                    self.mode = Mode::Normal;
                                }
                                KeyCode::Enter => {
                                    // Parse the command to get a decimal offset
                                    if let Ok(offset) = cmd.trim().parse::<u64>() {
                                        // Convert offset to line number (each line shows 16 bytes)
                                        let line = offset / 16;
                                        // Convert to usize and handle potential overflow
                                        let max_line = self.total_rows().saturating_sub(1);
                                        self.highlighted_line = (line as usize).min(max_line);
                                        self.ensure_highlighted_visible(visible_rows);
                                    }

                                    self.mode = Mode::Normal;
                                }
                                KeyCode::Backspace => {
                                    cmd.pop();
                                }
                                KeyCode::Char(c) => {
                                    if c.is_ascii_digit() {
                                        cmd.push(c);
                                    }
                                }
                                _ => {}
                            },
                        }
                    }
                }
            }

            if last_tick.elapsed() >= tick_rate {
                last_tick = Instant::now();
            }
        }
    }

    fn ui(&self, f: &mut Frame) {
        let area = f.area();

        // Calculate how many rows we can display
        let visible_rows = area.height.saturating_sub(2) as usize;

        // Generate the hex dump content
        let content = self.format_hex_dump_colored(self.vertical_scroll, visible_rows);

        // Create a block with rounded borders
        let block = Block::default()
            .title(format!(" hexide - {} ", self.filename))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded);

        // Create a paragraph with the hex dump content
        let paragraph = Paragraph::new(content)
            .block(block)
            .scroll((0, self.horizontal_scroll as u16));

        // Render the paragraph
        f.render_widget(paragraph, area);

        // Calculate total rows and create vertical scrollbar
        let total_rows = self.total_rows();

        // Only show vertical scrollbar if content is taller than visible area
        if total_rows > visible_rows {
            let vertical_scrollbar = Scrollbar::default()
                .orientation(ScrollbarOrientation::VerticalRight)
                .symbols(symbols::scrollbar::VERTICAL)
                .begin_symbol(Some("↑"))
                .end_symbol(Some("↓"));

            let mut vertical_scrollbar_state = ScrollbarState::default()
                .content_length(total_rows - visible_rows)
                .position(self.vertical_scroll)
                .viewport_content_length(visible_rows);

            let vertical_scrollbar_area = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Min(1), Constraint::Length(1)])
                .split(area);

            let vertical_scrollbar_area = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(1),
                    Constraint::Min(1),
                    Constraint::Length(1),
                ])
                .split(vertical_scrollbar_area[1]);

            f.render_stateful_widget(
                vertical_scrollbar,
                vertical_scrollbar_area[1],
                &mut vertical_scrollbar_state,
            );
        }

        // Add a help message at the bottom
        let help_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(1), Constraint::Length(1)])
            .split(area);

        let help_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(1),
                Constraint::Min(1),
                Constraint::Length(1),
            ])
            .split(help_layout[1]);

        match self.mode {
            Mode::Normal => {
                let help_text = " ↑/↓: Move highlight | j/k: Scroll | PgUp/PgDn: Page | Home/End: Jump | q: Quit ";
                let help_paragraph = Paragraph::new(Line::from(help_text).left_aligned())
                    .style(Style::default().fg(Color::Gray));
                f.render_widget(help_paragraph, help_layout[1]);
            }
            Mode::Command(ref cmd) => {
                let help_text = format!(" Offset: {} ", cmd);
                let help_paragraph = Paragraph::new(Line::from(help_text).left_aligned())
                    .style(Style::default().fg(Color::Gray));
                f.render_widget(help_paragraph, help_layout[1]);
            }
        }
    }

    fn get_byte_color(&self, byte: u8) -> Color {
        // Determine color based on byte content
        if byte == 0x00 {
            // Null byte
            Color::Black
        } else if byte.is_ascii_digit() {
            // Numbers (0-9)
            Color::Red
        } else if byte.is_ascii_alphabetic() {
            // Letters (a-z, A-Z)
            Color::Magenta
        } else if byte.is_ascii_punctuation() {
            // Symbols
            Color::Blue
        } else if byte.is_ascii_whitespace() {
            // Whitespace
            Color::Green
        } else if byte.is_ascii_control() {
            // Control characters
            Color::Yellow
        } else {
            // Other characters
            Color::Cyan
        }
    }

    fn format_hex_dump_colored(&self, start_row: usize, visible_rows: usize) -> Text<'static> {
        let mut lines = Vec::new();

        for row in 0..visible_rows {
            let current_row = start_row + row;
            let offset = current_row * 16;
            if offset >= self.file_data.len() {
                break;
            }

            let mut line_spans = Vec::new();

            // Determine if this is the highlighted line
            let is_highlighted = current_row == self.highlighted_line;
            let base_style = if is_highlighted {
                Style::default().bg(Color::Black)
            } else {
                Style::default()
            };

            // Add offset in gray
            line_spans.push(Span::styled(
                format!(" {:0fill$x}:  ", offset, fill = self.max_offset()),
                base_style.fg(Color::DarkGray),
            ));

            // Add hex values with colors based on content
            for i in 0..16 {
                let pos = offset + i;
                if pos < self.file_data.len() {
                    let byte = self.file_data[pos];
                    let color = self.get_byte_color(byte);
                    line_spans.push(Span::styled(format!("{:02x} ", byte), base_style.fg(color)));
                } else {
                    line_spans.push(Span::styled("   ", base_style));
                }

                // Add extra space in the middle
                if i == 7 {
                    line_spans.push(Span::styled(" ", base_style));
                }
            }

            // Add ASCII representation with matching colors
            line_spans.push(Span::styled(" |", base_style));
            for i in 0..16 {
                let pos = offset + i;
                if pos < self.file_data.len() {
                    let byte = self.file_data[pos];
                    let color = self.get_byte_color(byte);
                    let c = if byte >= 32 && byte <= 126 {
                        // Printable ASCII
                        byte as char
                    } else {
                        // Non-printable character
                        '.'
                    };
                    line_spans.push(Span::styled(c.to_string(), base_style.fg(color)));
                }
            }
            line_spans.push(Span::styled("| ", base_style));

            // Add the line to our collection
            lines.push(Line::from(line_spans));
        }

        Text::from(lines)
    }
}

fn main() -> Result<()> {
    // Parse command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app and run it
    let mut app = Hexide::new(filename)?;
    let res = app.run(&mut terminal);

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen,)?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err);
    }

    Ok(())
}
