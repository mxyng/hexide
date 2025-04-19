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
    Search(String),
}

struct SearchState {
    query: String,
    results: Vec<usize>,
    current_result: Option<usize>,
    search_in_progress: bool,
    search_position: usize, // Track where we are in the incremental search
    chunk_size: usize,      // How many bytes to search at once
}

impl SearchState {
    fn new() -> Self {
        Self {
            query: String::new(),
            results: Vec::new(),
            current_result: None,
            search_in_progress: false,
            search_position: 0,
            chunk_size: 16 << 10, // Default chunk size for searching
        }
    }

    fn next_result(&mut self) -> Option<usize> {
        if self.results.is_empty() {
            return None;
        }

        match self.current_result {
            None => {
                self.current_result = Some(0);
                Some(self.results[0])
            }
            Some(idx) if idx + 1 < self.results.len() => {
                self.current_result = Some(idx + 1);
                Some(self.results[idx + 1])
            }
            _ => self.current_result.map(|idx| self.results[idx]),
        }
    }

    fn prev_result(&mut self) -> Option<usize> {
        if self.results.is_empty() {
            return None;
        }

        match self.current_result {
            None => {
                self.current_result = Some(0);
                Some(self.results[0])
            }
            Some(idx) if idx > 0 => {
                self.current_result = Some(idx - 1);
                Some(self.results[idx - 1])
            }
            _ => self.current_result.map(|idx| self.results[idx]),
        }
    }

    fn reset_search(&mut self, query: &str) {
        self.query = query.to_string();
        self.results.clear();
        self.current_result = None;
        self.search_in_progress = true;
        self.search_position = 0;
    }
}

struct Hexide {
    bytes: Vec<u8>,
    cursor_row: usize,
    scroll_row: usize,
    scroll_col: usize,
    filename: String,
    mode: Mode,
    search: SearchState,
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
            search: SearchState::new(),
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

    fn goto_search_result(&mut self, offset: usize, visible_rows: usize) {
        let row = offset / 16;
        self.cursor_row = row;
        self.ensure_highlighted_visible(visible_rows);
    }

    fn next_search_result(&mut self, visible_rows: usize) -> bool {
        if let Some(offset) = self.search.next_result() {
            self.goto_search_result(offset, visible_rows);
            true
        } else {
            false
        }
    }

    fn prev_search_result(&mut self, visible_rows: usize) -> bool {
        if let Some(offset) = self.search.prev_result() {
            self.goto_search_result(offset, visible_rows);
            true
        } else {
            false
        }
    }

    fn start_search(&mut self, query: &str) {
        if query.is_empty() {
            return;
        }
        self.search.reset_search(query);

        // Search visible region first
        let visible_start = self.scroll_row * 16;
        let visible_end = (self.scroll_row + 30) * 16; // Approximate visible area
        let visible_end = visible_end.min(self.bytes.len());

        self.search_chunk(visible_start, visible_end);

        // Set search position to start from beginning for the rest of the file
        self.search.search_position = 0;
    }

    fn continue_search(&mut self) -> bool {
        if !self.search.search_in_progress {
            return false;
        }

        let start = self.search.search_position;
        let end = (start + self.search.chunk_size).min(self.bytes.len());

        // Skip the visible region if we've already searched it
        let visible_start = self.scroll_row * 16;
        let visible_end = (self.scroll_row + 30) * 16;
        let visible_end = visible_end.min(self.bytes.len());

        if start < visible_end && end > visible_start {
            // We're in the visible region, which we've already searched
            self.search.search_position = visible_end;
            return self.continue_search();
        }

        let found_results = self.search_chunk(start, end);

        // Update search position
        self.search.search_position = end;

        // Check if search is complete
        if end >= self.bytes.len() {
            self.search.search_in_progress = false;
        }

        found_results
    }

    fn search_chunk(&mut self, start: usize, end: usize) -> bool {
        let query = &self.search.query;
        let mut found_results = false;

        // Check if the query is a hex value
        if query.starts_with("0x") {
            // Parse hex value
            let hex_query = if query.starts_with("0x") {
                &query[2..]
            } else {
                query
            };

            if let Ok(byte_value) = u8::from_str_radix(hex_query, 16) {
                // Search for the byte value in this chunk
                for i in start..end {
                    if self.bytes[i] == byte_value {
                        self.search.results.push(i);
                        found_results = true;
                    }
                }
            }
        } else {
            // Search for ASCII string in this chunk
            let query_bytes = query.as_bytes();
            if !query_bytes.is_empty() {
                for i in start..=end.saturating_sub(query_bytes.len()) {
                    if i + query_bytes.len() <= self.bytes.len()
                        && self.bytes[i..].starts_with(query_bytes)
                    {
                        self.search.results.push(i);
                        found_results = true;
                    }
                }
            }
        }

        found_results
    }

    fn run(&mut self, terminal: &mut Terminal<impl Backend>) -> Result<()> {
        let mut last_tick = Instant::now();
        let tick_rate = Duration::from_millis(250);

        loop {
            // Continue incremental search if in progress
            if self.search.search_in_progress {
                self.continue_search();
            }

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
                                KeyCode::Char('/') => {
                                    self.mode = Mode::Search(String::new());
                                }
                                KeyCode::Char('n') => {
                                    self.next_search_result(visible_rows);
                                }
                                KeyCode::Char('N') => {
                                    self.prev_search_result(visible_rows);
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
                            Mode::Search(query) => match key.code {
                                KeyCode::Esc => {
                                    self.mode = Mode::Normal;
                                }
                                KeyCode::Enter => {
                                    let query_clone = query.clone();
                                    if !query_clone.is_empty() {
                                        self.start_search(&query_clone);
                                        self.next_search_result(visible_rows);
                                    }
                                    self.mode = Mode::Normal;
                                }
                                KeyCode::Backspace => {
                                    query.pop();
                                }
                                KeyCode::Char(c) => {
                                    query.push(c);
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
            Mode::Normal => {
                let search_info = if !self.search.results.is_empty() {
                    let progress = if self.search.search_in_progress {
                        format!(
                            "[{}/{}...]",
                            self.search.current_result.map_or(0, |i| i + 1),
                            self.search.results.len()
                        )
                    } else {
                        format!(
                            "[{}/{}]",
                            self.search.current_result.map_or(0, |i| i + 1),
                            self.search.results.len()
                        )
                    };
                    progress
                } else if self.search.search_in_progress {
                    "[...]".to_string()
                } else {
                    String::new()
                };
                format!(
                    " j/k: ↑/↓ | h/l: ←/→ | :: Offset | /: Search{} | q: Quit ",
                    search_info
                )
            }
            Mode::Command(cmd) => format!(" Offset: {} ", cmd),
            Mode::Search(query) => format!(" Search: {} ", query),
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

    fn is_byte_in_search_result(&self, pos: usize) -> bool {
        if self.search.results.is_empty() {
            return false;
        }

        // Check if this byte is part of any search result
        for &result_pos in &self.search.results {
            let query_len = if self.search.query.starts_with("0x")
                || self.search.query.chars().all(|c| c.is_digit(16))
            {
                1 // Hex search is always 1 byte
            } else {
                self.search.query.len()
            };

            if pos >= result_pos && pos < result_pos + query_len {
                return true;
            }
        }

        false
    }

    fn is_current_search_result(&self, pos: usize) -> bool {
        if let Some(current_idx) = self.search.current_result {
            if current_idx < self.search.results.len() {
                let result_pos = self.search.results[current_idx];
                let query_len = if self.search.query.starts_with("0x")
                    || self.search.query.chars().all(|c| c.is_digit(16))
                {
                    1 // Hex search is always 1 byte
                } else {
                    self.search.query.len()
                };

                return pos >= result_pos && pos < result_pos + query_len;
            }
        }
        false
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
                if is_highlighted {
                    base_style.fg(Color::Gray)
                } else {
                    base_style.fg(Color::DarkGray)
                },
            ));

            // Add hex values
            for i in 0..16 {
                let pos = offset + i;
                if pos < self.bytes.len() {
                    let byte = self.bytes[pos];
                    let mut byte_style = base_style.fg(self.get_byte_color(byte));
                    let mut space_style = byte_style; // Default: space has same style as byte

                    // Highlight search results
                    if self.is_byte_in_search_result(pos) {
                        byte_style = if self.is_current_search_result(pos) {
                            byte_style.bg(Color::Yellow).fg(Color::Black)
                        } else {
                            byte_style.bg(Color::Rgb(0x45, 0x39, 0x35)).fg(Color::White)
                        };

                        // Check if this is the last byte of a search result
                        let is_last_byte_of_search = !self.is_byte_in_search_result(pos + 1);
                        if is_last_byte_of_search {
                            space_style = base_style; // Use base style for space after last byte
                        } else {
                            space_style = byte_style; // Otherwise, space has same highlight as byte
                        }
                    }

                    line_spans.push(Span::styled(format!("{:02x}", byte), byte_style));
                    line_spans.push(Span::styled(" ", space_style));
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

                    let mut style = base_style.fg(self.get_byte_color(byte));

                    // Highlight search results
                    if self.is_byte_in_search_result(pos) {
                        style = if self.is_current_search_result(pos) {
                            style.bg(Color::Yellow).fg(Color::Black)
                        } else {
                            style.bg(Color::Rgb(0x45, 0x39, 0x35)).fg(Color::White)
                        };
                    }

                    line_spans.push(Span::styled(c.to_string(), style));
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
