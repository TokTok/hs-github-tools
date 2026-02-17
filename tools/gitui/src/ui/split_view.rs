use crate::diff_utils::LineType;
use crate::split_state::SplitViewMode;
use crate::state::AppState;
use crate::ui::SPINNERS;
use crate::ui::common::{centered_rect, draw_error, render_scrollbar};
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Gauge, List, ListItem, Paragraph};

pub fn draw_split_view(f: &mut Frame, state: &mut AppState) {
    let split_state = match &mut state.split_state {
        Some(s) => s,
        None => {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Min(0), Constraint::Length(3)])
                .split(f.area());

            let area = centered_rect(60, 3, chunks[0]);
            let gauge = Gauge::default()
                .block(Block::default().borders(Borders::ALL).title(format!(
                    "{} {}",
                    state.progress_message, SPINNERS[state.spinner_index]
                )))
                .gauge_style(Style::default().fg(Color::Yellow))
                .percent((state.progress_percentage * 100.0) as u16);
            f.render_widget(gauge, area);

            let help = Paragraph::new("q: cancel")
                .block(Block::default().borders(Borders::ALL).title("Help"));
            f.render_widget(help, chunks[1]);
            return;
        }
    };

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(3)])
        .split(f.area());

    let mut items = Vec::new();
    use crate::split_state::RenderedItem;

    for (item_idx, item) in split_state.rendered_items.iter().enumerate() {
        let is_selected = Some(item_idx) == split_state.list_state.selected();

        match item {
            RenderedItem::FileHeader { file_idx } => {
                let file = &split_state.files[*file_idx];
                let selected_count = file
                    .hunks
                    .iter()
                    .enumerate()
                    .filter(|(h_idx, _)| {
                        split_state
                            .current_selection
                            .contains(&(file.path.clone(), *h_idx))
                    })
                    .count();

                let checkbox = if selected_count == file.hunks.len() {
                    "[x] "
                } else if selected_count > 0 {
                    "[-] "
                } else {
                    "[ ] "
                };

                let mut spans = vec![
                    Span::raw(if is_selected { ">>" } else { "  " }),
                    Span::raw(checkbox),
                    Span::styled(
                        &file.path,
                        Style::default()
                            .add_modifier(Modifier::BOLD)
                            .fg(Color::Cyan),
                    ),
                ];

                if is_selected {
                    for span in &mut spans {
                        span.style = span.style.fg(Color::Yellow);
                    }
                }
                items.push(ListItem::new(Line::from(spans)));
            }
            RenderedItem::HunkHeader { file_idx, hunk_idx } => {
                let file = &split_state.files[*file_idx];
                let hunk = &file.hunks[*hunk_idx];
                let selected = split_state
                    .current_selection
                    .contains(&(file.path.clone(), *hunk_idx));

                let mut spans = vec![
                    Span::raw(if is_selected { "    >>" } else { "      " }),
                    Span::raw(if selected { "[x] " } else { "[ ] " }),
                    Span::styled(&hunk.header, Style::default().fg(Color::Yellow)),
                ];
                if is_selected {
                    for span in &mut spans {
                        span.style = span.style.fg(Color::Yellow);
                    }
                }
                items.push(ListItem::new(Line::from(spans)));
            }
            RenderedItem::Line {
                file_idx,
                hunk_idx,
                line_idx,
            } => {
                let file = &split_state.files[*file_idx];
                let hunk = &file.hunks[*hunk_idx];
                let line = &hunk.lines[*line_idx];

                let style = match line.line_type {
                    LineType::Addition => Style::default().fg(Color::Green),
                    LineType::Deletion => Style::default().fg(Color::Red),
                    _ => Style::default().fg(Color::DarkGray),
                };

                let prefix = match line.line_type {
                    LineType::Addition => "+",
                    LineType::Deletion => "-",
                    _ => " ",
                };

                let mut spans = vec![
                    Span::raw(if is_selected { "      >>" } else { "        " }),
                    Span::raw("    "),
                    Span::styled(format!("{}{}", prefix, line.content.trim_end()), style),
                ];
                if is_selected {
                    spans[0].style = Style::default().fg(Color::Yellow);
                }
                items.push(ListItem::new(Line::from(spans)));
            }
        }
    }

    let mut title_parts = Vec::new();
    for part in &split_state.parts {
        title_parts.push(part.name.as_str());
    }
    title_parts.push(state.splitting_branch.as_deref().unwrap_or("remainder"));

    let list = List::new(items.clone())
        .block(Block::default().borders(Borders::ALL).title(format!(
            "Split Commit: {} -> [{}]",
            state.splitting_branch.as_deref().unwrap_or(""),
            title_parts.join(" | ")
        )))
        .highlight_style(Style::default().add_modifier(Modifier::BOLD))
        .highlight_symbol(">>");

    f.render_stateful_widget(list, chunks[0], &mut split_state.list_state);

    render_scrollbar(f, chunks[0], items.len(), split_state.list_state.offset());

    let enter_desc = if split_state.current_selection.is_empty() {
        "finish"
    } else {
        "new part"
    };

    let help_text = match split_state.mode {
        SplitViewMode::Files => {
            format!(
                "j/k: navigate | space: toggle file | ->: view hunks | enter: {} | q: cancel",
                enter_desc
            )
        }
        SplitViewMode::Hunks => {
            format!(
                "j/k: navigate hunks | space: toggle hunk | ->: view lines | <-: back to files | enter: {} | q: cancel",
                enter_desc
            )
        }
        SplitViewMode::Lines => {
            format!(
                "j/k: navigate lines | space: toggle hunk | <-: back to hunks | enter: {} | q: cancel",
                enter_desc
            )
        }
    };

    let help =
        Paragraph::new(help_text).block(Block::default().borders(Borders::ALL).title("Help"));
    f.render_widget(help, chunks[1]);

    if let Some(error) = &state.error_message {
        draw_error(f, error);
    }
}
