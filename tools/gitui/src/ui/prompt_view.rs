use crate::state::{AppState, PromptFocus};
use crate::ui::common::{centered_rect, draw_error};
use ratatui::Frame;
use ratatui::layout::{Alignment, Constraint, Direction, Layout};
use ratatui::style::{Color, Style};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};

pub fn draw_prompt(f: &mut Frame, state: &AppState) {
    let prompt = match &state.prompt {
        Some(p) => p,
        None => return,
    };

    let is_multiline = prompt.action.is_multiline();
    let height = if is_multiline { 12 } else { 6 };
    let area = centered_rect(60, height, f.area());

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(3)])
        .split(area);

    let block = Block::default()
        .title(prompt.title.clone())
        .borders(Borders::ALL)
        .border_style(if prompt.focus == PromptFocus::Input {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::DarkGray)
        });

    let input = Paragraph::new(prompt.value.clone()).block(block);
    f.render_widget(Clear, area);
    f.render_widget(input, chunks[0]);

    // Render buttons
    let button_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Min(0),
            Constraint::Length(12), // [ OK ]
            Constraint::Length(12), // [ Cancel ]
            Constraint::Min(0),
        ])
        .split(chunks[1]);

    let ok_style = if prompt.focus == PromptFocus::Ok {
        Style::default().bg(Color::Yellow).fg(Color::Black)
    } else {
        Style::default().fg(Color::White)
    };
    let ok_button = Paragraph::new("    OK    ")
        .block(Block::default().borders(Borders::ALL))
        .style(ok_style)
        .alignment(Alignment::Center);
    f.render_widget(ok_button, button_chunks[1]);

    let cancel_style = if prompt.focus == PromptFocus::Cancel {
        Style::default().bg(Color::Yellow).fg(Color::Black)
    } else {
        Style::default().fg(Color::White)
    };
    let cancel_button = Paragraph::new("  Cancel  ")
        .block(Block::default().borders(Borders::ALL))
        .style(cancel_style)
        .alignment(Alignment::Center);
    f.render_widget(cancel_button, button_chunks[2]);

    // Calculate cursor position for multi-line
    let mut cursor_x = 0;
    let mut cursor_y = 0;
    for (i, c) in prompt.value.chars().enumerate() {
        if i >= prompt.cursor_position {
            break;
        }
        if c == '\n' {
            cursor_x = 0;
            cursor_y += 1;
        } else {
            cursor_x += 1;
        }
    }

    // Set cursor position only if Input is focused
    if prompt.focus == PromptFocus::Input {
        f.set_cursor_position((
            chunks[0].x + cursor_x as u16 + 1,
            chunks[0].y + cursor_y as u16 + 1,
        ));
    }

    if let Some(error) = &state.error_message {
        draw_error(f, error);
    }
}
