use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::time::{Duration, Instant};

use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    prelude::*,
    symbols,
    widgets::{
        Block, BorderType, Borders, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
    },
};

enum Mode {
    Normal,
    Command(String),
}

struct Hexide {
    bytes: Vec<u8>,
    cursor_row: usize,
    scroll_row: usize,
    scroll_col: usize,
    filename: String,
    mode: Mode,
}

impl Hexide {
    fn new(filename: &str) -> Result<Self> {
        let mut file = File::open(filename).context("Failed to open file")?;
        let mut file_data = Vec::new();
        file.read_to_end(&mut file_data)
            .context("Failed to read file")?;

        Ok(Self {
            bytes: file_data,
            scroll_row: 0,
            scroll_col: 0,
            filename: filename.to_string(),
            mode: Mode::Normal,
            cursor_row: 0,
        })
    }

    fn max_offset(&self) -> usize {
        format!("{:x}", self.bytes.len()).len().max(8)
    }

    fn content_width(&self) -> usize {
        1 + self.max_offset() + 1 + 16 * 3 + 2 + 16 + 2 + 1
    }

    fn total_rows(&self) -> usize {
        (self.bytes.len() + 15) / 16
    }

    fn max_vertical_scroll(&self, visible_rows: usize) -> usize {
        let total = self.total_rows();
        if total <= visible_rows {
            0
        } else {
            total - visible_rows
        }
    }

    fn ensure_highlighted_visible(&mut self, visible_rows: usize) {
        if self.cursor_row < self.scroll_row {
            self.scroll_row = self.cursor_row;
        } else if self.cursor_row >= self.scroll_row + visible_rows {
            self.scroll_row = self.cursor_row.saturating_sub(visible_rows) + 1;
        }

        self.scroll_row = self.scroll_row.min(self.max_vertical_scroll(visible_rows));
    }

    fn run(&mut self, terminal: &mut Terminal<impl Backend>) -> Result<()> {
        let mut last_tick = Instant::now();
        let tick_rate = Duration::from_millis(250);

        loop {
            terminal.draw(|f| self.ui(f))?;

            let timeout = tick_rate
                .checked_sub(last_tick.elapsed())
                .unwrap_or_default();

            if crossterm::event::poll(timeout)? {
                if let Event::Key(key) = event::read()? {
                    if key.kind == KeyEventKind::Press {
                        let terminal_size = terminal.size()?;
                        let visible_rows = terminal_size.height.saturating_sub(2) as usize;
                        let visible_width = terminal_size.width.saturating_sub(4) as usize;

                        match &mut self.mode {
                            Mode::Normal => match key.code {
                                KeyCode::Char('q') => return Ok(()),
                                KeyCode::Down | KeyCode::Char('j') => {
                                    let max_line = self.total_rows().saturating_sub(1);
                                    self.cursor_row =
                                        self.cursor_row.saturating_add(1).min(max_line);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::Up | KeyCode::Char('k') => {
                                    self.cursor_row = self.cursor_row.saturating_sub(1);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::Right | KeyCode::Char('l') => {
                                    let content_width = self.content_width();
                                    if content_width > visible_width {
                                        let max_scroll =
                                            content_width.saturating_sub(visible_width);
                                        self.scroll_col =
                                            self.scroll_col.saturating_add(1).min(max_scroll);
                                    }
                                }
                                KeyCode::Left | KeyCode::Char('h') => {
                                    self.scroll_col = self.scroll_col.saturating_sub(1);
                                }
                                KeyCode::PageDown => {
                                    let max_line = self.total_rows().saturating_sub(1);
                                    self.cursor_row =
                                        self.cursor_row.saturating_add(visible_rows).min(max_line);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::PageUp => {
                                    self.cursor_row = self.cursor_row.saturating_sub(visible_rows);
                                    self.ensure_highlighted_visible(visible_rows);
                                }
                                KeyCode::Home => {
                                    self.cursor_row = 0;
                                    self.scroll_row = 0;
                                    self.scroll_col = 0;
                                }
                                KeyCode::End => {
                                    self.cursor_row = self.total_rows().saturating_sub(1);
                                    self.scroll_row = self.max_vertical_scroll(visible_rows);
                                }
                                KeyCode::Char(':') => {
                                    self.mode = Mode::Command(String::new());
                                }
                                _ => {}
                            },
                            Mode::Command(cmd) => match key.code {
                                KeyCode::Esc => {
                                    self.mode = Mode::Normal;
                                }
                                KeyCode::Enter => {
                                    if let Ok(offset) = cmd.trim().parse::<u64>() {
                                        let line = offset / 16;
                                        let max_line = self.total_rows().saturating_sub(1);
                                        self.cursor_row = (line as usize).min(max_line);
                                        self.ensure_highlighted_visible(visible_rows);
                                    }
                                    self.mode = Mode::Normal;
                                }
                                KeyCode::Backspace => {
                                    cmd.pop();
                                }
                                KeyCode::Char(c) if c.is_ascii_digit() => {
                                    cmd.push(c);
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

    fn truncate_path(&self, width: usize) -> String {
        let display_path = if let Ok(home) = env::var("HOME") {
            if self.filename.starts_with(&home) {
                self.filename.replacen(&home, "~", 1)
            } else {
                self.filename.clone()
            }
        } else {
            self.filename.clone()
        };

        display_path
            .split('/')
            .last()
            .unwrap_or(&display_path)
            .chars()
            .take(width)
            .collect()
    }

    fn ui(&self, f: &mut Frame) {
        let area = f.area();
        let visible_rows = area.height.saturating_sub(2) as usize;

        // Create block and paragraph
        let block = Block::default()
            .title(format!(
                " hexide - {} ",
                self.truncate_path((area.width as usize) - " hexide -  ".len() - 2)
            ))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded);

        let paragraph = Paragraph::new(self.format_hex_dump_colored(self.scroll_row, visible_rows))
            .block(block)
            .scroll((0, self.scroll_col as u16));

        f.render_widget(paragraph, area);

        // Render scrollbar if needed
        let total_rows = self.total_rows();
        if total_rows > visible_rows {
            let vertical_scrollbar = Scrollbar::default()
                .orientation(ScrollbarOrientation::VerticalRight)
                .symbols(symbols::scrollbar::VERTICAL)
                .begin_symbol(None)
                .end_symbol(None);

            let mut scrollbar_state = ScrollbarState::default()
                .content_length(total_rows - visible_rows)
                .position(self.scroll_row)
                .viewport_content_length(visible_rows);

            let scrollbar_area = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Min(1), Constraint::Length(1)])
                .split(area);

            let scrollbar_area = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(1),
                    Constraint::Min(1),
                    Constraint::Length(1),
                ])
                .split(scrollbar_area[1]);

            f.render_stateful_widget(vertical_scrollbar, scrollbar_area[1], &mut scrollbar_state);
        }

        // Render help text
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

        let help_text = match &self.mode {
            Mode::Normal => " j/k: ↑/↓ | h/l: ←/→ | q: Quit ",
            Mode::Command(cmd) => &format!(" Offset: {} ", cmd),
        };

        let help_paragraph = Paragraph::new(Line::from(help_text).left_aligned())
            .style(Style::default().fg(Color::Gray));
        f.render_widget(help_paragraph, help_layout[1]);
    }

    fn get_byte_color(&self, byte: u8) -> Color {
        match byte {
            0x00 => Color::Black,
            b if b.is_ascii_digit() => Color::Red,
            b if b.is_ascii_alphabetic() => Color::Magenta,
            b if b.is_ascii_punctuation() => Color::Blue,
            b if b.is_ascii_whitespace() => Color::Green,
            b if b.is_ascii_control() => Color::Yellow,
            _ => Color::Cyan,
        }
    }

    fn format_hex_dump_colored(&self, start_row: usize, visible_rows: usize) -> Text<'static> {
        let mut lines = Vec::with_capacity(visible_rows);

        for row in 0..visible_rows {
            let current_row = start_row + row;
            let offset = current_row * 16;
            if offset >= self.bytes.len() {
                break;
            }

            let mut line_spans = Vec::new();
            let is_highlighted = current_row == self.cursor_row;
            let base_style = if is_highlighted {
                Style::default().bg(Color::Black)
            } else {
                Style::default()
            };

            // Add offset
            line_spans.push(Span::styled(
                format!(" {:0fill$x}:  ", offset, fill = self.max_offset()),
                base_style.fg(Color::DarkGray),
            ));

            // Add hex values
            for i in 0..16 {
                let pos = offset + i;
                if pos < self.bytes.len() {
                    let byte = self.bytes[pos];
                    line_spans.push(Span::styled(
                        format!("{:02x} ", byte),
                        base_style.fg(self.get_byte_color(byte)),
                    ));
                } else {
                    line_spans.push(Span::styled("   ", base_style));
                }

                // Add extra space in the middle
                if i == 7 {
                    line_spans.push(Span::styled(" ", base_style));
                }
            }

            // Add ASCII representation
            line_spans.push(Span::styled(" |", base_style));
            for i in 0..16 {
                let pos = offset + i;
                if pos < self.bytes.len() {
                    let byte = self.bytes[pos];
                    let c = if byte >= 32 && byte <= 126 {
                        byte as char
                    } else {
                        '.'
                    };
                    line_spans.push(Span::styled(
                        c.to_string(),
                        base_style.fg(self.get_byte_color(byte)),
                    ));
                }
            }
            line_spans.push(Span::styled("| ", base_style));

            lines.push(Line::from(line_spans));
        }

        Text::from(lines)
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    // Setup terminal
    terminal::enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Run app
    let mut app = Hexide::new(&args[1])?;
    let res = app.run(&mut terminal);

    // Restore terminal
    terminal::disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err);
    }

    Ok(())
}
