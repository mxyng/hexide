use std::env;
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom};
use std::panic;
use std::sync::{Arc, Mutex};
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

// ===== Application State =====

enum Mode {
    Normal,
    Offset(String),
    Search(String),
}

struct SearchState {
    query: String,
    results: Vec<usize>,
    current_result: Option<usize>,
    search_in_progress: bool,
    search_position: usize,
    chunk_size: usize,
}

impl SearchState {
    fn new() -> Self {
        Self {
            query: String::new(),
            results: Vec::new(),
            current_result: None,
            search_in_progress: false,
            search_position: 0,
            chunk_size: 16 << 10, // 16KB
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

    fn reset(&mut self, query: &str) {
        self.query = query.to_string();
        self.results.clear();
        self.current_result = None;
        self.search_in_progress = true;
        self.search_position = 0;
    }

    fn is_match(&self, pos: usize, query_len: usize) -> bool {
        self.results
            .iter()
            .any(|&result_pos| pos >= result_pos && pos < result_pos + query_len)
    }

    fn is_current_match(&self, pos: usize, query_len: usize) -> bool {
        if let Some(idx) = self.current_result {
            if idx < self.results.len() {
                let result_pos = self.results[idx];
                return pos >= result_pos && pos < result_pos + query_len;
            }
        }
        false
    }

    fn query_len(&self) -> usize {
        if let Some(hex_query) = self.query.strip_prefix("0x") {
            if hex_query.chars().all(|c| c.is_ascii_hexdigit()) {
                return hex_query.len() / 2;
            }
        }
        self.query.len()
    }

    fn status_text(&self) -> String {
        if self.results.is_empty() {
            if self.search_in_progress {
                "[...]".to_string()
            } else {
                String::new()
            }
        } else {
            let progress = if self.search_in_progress { "..." } else { "" };
            format!(
                "[{}/{}{}]",
                self.current_result.map_or(0, |i| i + 1),
                self.results.len(),
                progress
            )
        }
    }
}

struct Hexide {
    cache: Vec<u8>,
    cache_start: usize, // file offset of cache[0]
    cache_end: usize,   // file offset immediately after cache
    total_file_size: usize,
    cursor_row: usize,
    scroll_row: usize,
    scroll_col: usize,
    filename: String,
    mode: Mode,
    search: SearchState,
    start_offset: usize,
}

impl Hexide {
    fn new(filename: &str, start_offset: usize, _max_bytes: Option<usize>) -> Result<Self> {
        let mut file = File::open(filename).context("Failed to open file")?;
        let metadata = file.metadata().context("Failed to get file metadata")?;
        let file_size = metadata.len() as usize;

        if start_offset >= file_size {
            return Err(anyhow::anyhow!("Start offset is beyond file size"));
        }

        let initial_cache_size = 256;
        let cache_end = (start_offset + initial_cache_size).min(file_size);
        let cache_size = cache_end - start_offset;
        let mut cache = vec![0u8; cache_size];
        file.seek(SeekFrom::Start(start_offset as u64))
            .context("Failed to seek in file")?;
        file.read_exact(&mut cache)
            .context("Failed to read initial cache window")?;

        Ok(Self {
            cache,
            cache_start: start_offset,
            cache_end,
            total_file_size: file_size,
            scroll_row: 0,
            scroll_col: 0,
            filename: filename.to_string(),
            mode: Mode::Normal,
            cursor_row: 0,
            search: SearchState::new(),
            start_offset,
        })
    }

    fn max_offset(&self) -> usize {
        format!("{:x}", self.start_offset + self.total_file_size)
            .len()
            .max(8)
    }

    fn content_width(&self) -> usize {
        1 + self.max_offset() + 1 + 16 * 3 + 2 + 16 + 2 + 1
    }

    fn total_rows(&self) -> usize {
        self.total_file_size.div_ceil(16) // round up division
    }

    fn max_vertical_scroll(&self, visible_rows: usize) -> usize {
        let total = self.total_rows();
        total.saturating_sub(visible_rows)
    }

    fn ensure_cache_contains_visible_region(
        &mut self,
        visible_start: usize,
        visible_length: usize,
    ) -> Result<()> {
        let desired_start = visible_start.saturating_sub(visible_length);
        let mut desired_end = visible_start + (2 * visible_length);
        if desired_end > self.total_file_size {
            desired_end = self.total_file_size;
        }
        if desired_start < self.cache_start || desired_end > self.cache_end {
            let new_cache_size = desired_end - desired_start;
            let mut new_cache = vec![0u8; new_cache_size];
            let mut file = File::open(&self.filename)?;
            file.seek(SeekFrom::Start(desired_start as u64))?;
            file.read_exact(&mut new_cache)?;
            self.cache = new_cache;
            self.cache_start = desired_start;
            self.cache_end = desired_end;
        }
        Ok(())
    }

    fn get_byte_at(&mut self, offset: usize) -> Result<u8> {
        if offset >= self.cache_start && offset < self.cache_end {
            Ok(self.cache[offset - self.cache_start])
        } else {
            let mut file = File::open(&self.filename)?;
            file.seek(SeekFrom::Start(offset as u64))?;
            let mut buf = [0u8; 1];
            file.read_exact(&mut buf)?;
            Ok(buf[0])
        }
    }

    // ----- Navigation -----
    fn ensure_cursor_visible(&mut self, visible_rows: usize) {
        if self.cursor_row < self.scroll_row {
            self.scroll_row = self.cursor_row;
        } else if self.cursor_row >= self.scroll_row + visible_rows {
            self.scroll_row = self.cursor_row.saturating_sub(visible_rows) + 1;
        }
        self.scroll_row = self.scroll_row.min(self.max_vertical_scroll(visible_rows));
    }

    fn goto_offset(&mut self, offset: usize, visible_rows: usize) {
        let row = offset / 16;
        self.cursor_row = row;
        self.ensure_cursor_visible(visible_rows);
    }

    // ----- Search Functionality -----
    ///
    /// Initiates a search (resets search state and scans the visible region once).
    fn start_search(&mut self, query: &str, visible_rows: usize) -> Result<()> {
        if query.is_empty() {
            return Ok(());
        }
        self.search.reset(query);
        let visible_start = self.scroll_row * 16;
        let visible_end = (self.scroll_row + visible_rows) * 16;
        let _ = self.search_chunk(visible_start, visible_end)?;
        self.search.search_position = 0;
        Ok(())
    }

    /// Called repeatedly by the background search task.
    fn continue_search(&mut self) -> Result<bool> {
        if !self.search.search_in_progress {
            return Ok(false);
        }
        let start = self.search.search_position;
        let end = (start + self.search.chunk_size).min(self.total_file_size);
        let visible_start = self.scroll_row * 16;
        let visible_end = ((self.scroll_row + 30) * 16).min(self.total_file_size);
        if start < visible_end && end > visible_start {
            self.search.search_position = visible_end;
            return self.continue_search();
        }
        let found = self.search_chunk(start, end)?;
        self.search.search_position = end;
        if end >= self.total_file_size {
            self.search.search_in_progress = false;
        }
        Ok(found)
    }

    fn search_chunk(&mut self, start: usize, end: usize) -> Result<bool> {
        let mut found_results = false;
        let query = self.search.query.clone();
        if let Some(hex_query) = query.strip_prefix("0x") {
            if let Ok(byte_value) = u8::from_str_radix(hex_query, 16) {
                for pos in start..end {
                    let byte = self.get_byte_at(pos)?;
                    if byte == byte_value {
                        self.search.results.push(pos);
                        found_results = true;
                    }
                }
            }
        } else {
            let query_bytes = query.as_bytes();
            if !query_bytes.is_empty() {
                let qlen = query_bytes.len();
                for pos in start..=end.saturating_sub(qlen) {
                    let mut match_found = true;
                    for (j, _) in query_bytes.iter().enumerate().take(qlen) {
                        let b = self.get_byte_at(pos + j)?;
                        if b != query_bytes[j] {
                            match_found = false;
                            break;
                        }
                    }
                    if match_found {
                        self.search.results.push(pos);
                        found_results = true;
                    }
                }
            }
        }
        Ok(found_results)
    }

    fn next_search_result(&mut self, visible_rows: usize) -> bool {
        if let Some(offset) = self.search.next_result() {
            self.goto_offset(offset, visible_rows);
            true
        } else {
            false
        }
    }

    fn prev_search_result(&mut self, visible_rows: usize) -> bool {
        if let Some(offset) = self.search.prev_result() {
            self.goto_offset(offset, visible_rows);
            true
        } else {
            false
        }
    }

    // ----- UI Helpers & Rendering -----
    fn truncate_path(&self, width: usize) -> String {
        self.filename
            .split('/')
            .next_back()
            .unwrap_or(&self.filename)
            .chars()
            .take(width)
            .collect()
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

    fn ui(&mut self, f: &mut Frame) {
        let area = f.area();
        let visible_rows = area.height.saturating_sub(2) as usize;
        let visible_start = self.scroll_row * 16;
        let visible_length = visible_rows * 16;
        let _ = self.ensure_cache_contains_visible_region(visible_start, visible_length);

        let block = Block::default()
            .title(format!(
                " hexide - {} ",
                self.truncate_path((area.width as usize) - " hexide - {} ".len())
            ))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded);
        let paragraph = Paragraph::new(self.format_hex_dump(self.scroll_row, visible_rows))
            .block(block)
            .scroll((0, self.scroll_col as u16));
        f.render_widget(paragraph, area);
        self.render_scrollbar(f, area, visible_rows);
        self.render_status_bar(f, area);
    }

    fn render_scrollbar(&self, f: &mut Frame, area: Rect, visible_rows: usize) {
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
    }

    fn render_status_bar(&self, f: &mut Frame, area: Rect) {
        let status_text = match &self.mode {
            Mode::Normal => format!(
                " j/k: ↑/↓ | h/l: ←/→ | :: Offset | /: Search{} | q: Quit ",
                self.search.status_text()
            ),
            Mode::Offset(cmd) => format!(" Offset: {} ", cmd),
            Mode::Search(query) => format!(" Search: {} ", query),
        };
        let status_paragraph = Paragraph::new(Line::from(status_text).left_aligned())
            .style(Style::default().fg(Color::Gray));
        let status_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(1), Constraint::Length(1)])
            .split(area);
        let status_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(1),
                Constraint::Min(1),
                Constraint::Length(1),
            ])
            .split(status_layout[1]);
        f.render_widget(status_paragraph, status_layout[1]);
    }

    fn format_hex_dump(&mut self, start_row: usize, visible_rows: usize) -> Text<'static> {
        let mut lines = Vec::with_capacity(visible_rows);
        for row in 0..visible_rows {
            let current_row = start_row + row;
            let file_offset = current_row * 16;
            if file_offset >= self.total_file_size {
                break;
            }
            let mut line_spans = Vec::new();
            let is_highlighted = current_row == self.cursor_row;
            let base_style = if is_highlighted {
                Style::default().bg(Color::Black)
            } else {
                Style::default()
            };
            line_spans.push(Span::styled(
                format!(
                    " {:0fill$x}:  ",
                    file_offset + self.start_offset,
                    fill = self.max_offset()
                ),
                if is_highlighted {
                    base_style.fg(Color::Gray)
                } else {
                    base_style.fg(Color::DarkGray)
                },
            ));
            for i in 0..16 {
                let pos = file_offset + i;
                if pos < self.total_file_size {
                    self.add_hex_byte(&mut line_spans, pos, base_style);
                } else {
                    line_spans.push(Span::styled("   ", base_style));
                }
                if i == 7 {
                    line_spans.push(Span::styled(" ", base_style));
                }
            }
            line_spans.push(Span::styled(" │", base_style));
            for i in 0..16 {
                let pos = file_offset + i;
                if pos < self.total_file_size {
                    self.add_ascii_char(&mut line_spans, pos, base_style);
                }
            }
            line_spans.push(Span::styled("│ ", base_style));
            lines.push(Line::from(line_spans));
        }
        Text::from(lines)
    }

    fn add_hex_byte(&mut self, line_spans: &mut Vec<Span<'static>>, pos: usize, base_style: Style) {
        let byte = self.get_byte_at(pos).unwrap_or(0);
        let mut byte_style = base_style.fg(self.get_byte_color(byte));
        let mut space_style = byte_style;
        let query_len = self.search.query_len();
        if self.search.is_match(pos, query_len) {
            byte_style = if self.search.is_current_match(pos, query_len) {
                byte_style.bg(Color::Yellow).fg(Color::Black)
            } else {
                byte_style.bg(Color::Rgb(0x45, 0x39, 0x35)).fg(Color::White)
            };
            space_style = if !self.search.is_match(pos + 1, query_len) {
                base_style
            } else {
                byte_style
            };
        }
        line_spans.push(Span::styled(format!("{:02x}", byte), byte_style));
        line_spans.push(Span::styled(" ", space_style));
    }

    fn add_ascii_char(
        &mut self,
        line_spans: &mut Vec<Span<'static>>,
        pos: usize,
        base_style: Style,
    ) {
        let byte = self.get_byte_at(pos).unwrap_or(0);
        let c = if (32..=126).contains(&byte) {
            byte as char
        } else {
            '.'
        };
        let mut style = base_style.fg(self.get_byte_color(byte));
        let query_len = self.search.query_len();
        if self.search.is_match(pos, query_len) {
            style = if self.search.is_current_match(pos, query_len) {
                style.bg(Color::Yellow).fg(Color::Black)
            } else {
                style.bg(Color::Rgb(0x45, 0x39, 0x35)).fg(Color::White)
            };
        }
        line_spans.push(Span::styled(c.to_string(), style));
    }

    // ----- Main Application Loop -----
    fn run(
        &mut self,
        terminal: &mut Terminal<impl Backend>,
        app_arc: Arc<Mutex<Hexide>>,
    ) -> Result<()> {
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
                        if !self.handle_key_event(
                            key.code,
                            visible_rows,
                            visible_width,
                            app_arc.clone(),
                        ) {
                            return Ok(());
                        }
                    }
                }
            }
            if last_tick.elapsed() >= tick_rate {
                last_tick = Instant::now();
            }
        }
    }

    // Updated handle_key_event with an Arc clone passed for background search.
    fn handle_key_event(
        &mut self,
        key: KeyCode,
        visible_rows: usize,
        visible_width: usize,
        app_arc: Arc<Mutex<Hexide>>,
    ) -> bool {
        match &mut self.mode {
            Mode::Normal => match key {
                KeyCode::Char('q') => return false,
                KeyCode::Down | KeyCode::Char('j') => {
                    let max_line = self.total_rows().saturating_sub(1);
                    self.cursor_row = self.cursor_row.saturating_add(1).min(max_line);
                    self.ensure_cursor_visible(visible_rows);
                }
                KeyCode::Up | KeyCode::Char('k') => {
                    self.cursor_row = self.cursor_row.saturating_sub(1);
                    self.ensure_cursor_visible(visible_rows);
                }
                KeyCode::Right | KeyCode::Char('l') => {
                    let content_width = self.content_width();
                    if content_width > visible_width {
                        let max_scroll = content_width.saturating_sub(visible_width);
                        self.scroll_col = self.scroll_col.saturating_add(1).min(max_scroll);
                    }
                }
                KeyCode::Left | KeyCode::Char('h') => {
                    self.scroll_col = self.scroll_col.saturating_sub(1);
                }
                KeyCode::PageDown => {
                    let max_line = self.total_rows().saturating_sub(1);
                    self.cursor_row = self.cursor_row.saturating_add(visible_rows).min(max_line);
                    self.ensure_cursor_visible(visible_rows);
                }
                KeyCode::PageUp => {
                    self.cursor_row = self.cursor_row.saturating_sub(visible_rows);
                    self.ensure_cursor_visible(visible_rows);
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
                    self.mode = Mode::Offset(String::new());
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
            Mode::Offset(cmd) => match key {
                KeyCode::Esc => {
                    self.mode = Mode::Normal;
                }
                KeyCode::Enter => {
                    if let Ok(offset) = cmd.trim().parse::<u64>() {
                        let line = offset / 16;
                        let max_line = self.total_rows().saturating_sub(1);
                        self.cursor_row = (line as usize).min(max_line);
                        self.ensure_cursor_visible(visible_rows);
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
            Mode::Search(query) => match key {
                KeyCode::Esc => {
                    self.mode = Mode::Normal;
                }
                KeyCode::Enter => {
                    let query_clone = query.clone();
                    if !query_clone.is_empty() {
                        if let Err(e) = self.start_search(&query_clone, visible_rows) {
                            eprintln!("Search error: {:?}", e);
                        }
                        // Spawn a background search task using the shared app_arc.
                        let app_for_search = app_arc.clone();
                        tokio::spawn(async move {
                            tokio::task::spawn_blocking(move || {
                                loop {
                                    // Lock the app and run a search chunk.
                                    let mut app = app_for_search.lock().unwrap();
                                    match app.continue_search() {
                                        Ok(false) => break,
                                        Ok(true) => { /* continue searching */ }
                                        Err(e) => {
                                            eprintln!("Error during background search: {:?}", e);
                                            break;
                                        }
                                    }
                                    // Small sleep to yield time.
                                    std::thread::sleep(Duration::from_millis(10));
                                }
                            })
                            .await
                            .unwrap();
                        });
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
        true
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename> [-s offset] [-n bytes]", args[0]);
        std::process::exit(1);
    }
    let mut filename = None;
    let mut start_offset = 0;
    let mut _max_bytes = None;
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-s" => {
                if i + 1 < args.len() {
                    start_offset = args[i + 1].parse::<usize>().unwrap_or_else(|_| {
                        eprintln!("Invalid offset value: {}", args[i + 1]);
                        std::process::exit(1);
                    });
                    i += 2;
                } else {
                    eprintln!("Missing value for -s option");
                    std::process::exit(1);
                }
            }
            "-n" => {
                if i + 1 < args.len() {
                    _max_bytes = Some(args[i + 1].parse::<usize>().unwrap_or_else(|_| {
                        eprintln!("Invalid bytes value: {}", args[i + 1]);
                        std::process::exit(1);
                    }));
                    i += 2;
                } else {
                    eprintln!("Missing value for -n option");
                    std::process::exit(1);
                }
            }
            _ => {
                filename = Some(args[i].clone());
                i += 1;
            }
        }
    }
    let filename = match filename {
        Some(name) => name,
        None => {
            eprintln!("No filename specified");
            std::process::exit(1);
        }
    };

    let app = Hexide::new(&filename, start_offset, _max_bytes)?;
    // Wrap our app in Arc<Mutex<>> so that it can be shared safely.
    let app_arc = Arc::new(Mutex::new(app));

    terminal::enable_raw_mode()?;
    panic::set_hook(Box::new(|info| {
        let _ = terminal::disable_raw_mode();
        println!("Error: {:?}", info);
    }));
    let stdout = io::stdout();
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    execute!(terminal.backend_mut(), EnterAlternateScreen)?;
    terminal.hide_cursor()?;

    {
        // We use a scoped lock to obtain mutable reference to run our UI.
        let mut app = app_arc.lock().unwrap();
        app.run(&mut terminal, app_arc.clone())?;
    }

    terminal.show_cursor()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal::disable_raw_mode()?;
    Ok(())
}
