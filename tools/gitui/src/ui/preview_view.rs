use crate::engine::Operation;
use crate::state::AppState;
use crate::ui::common::draw_error;
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, List, ListItem, Paragraph};

pub fn draw_preview(f: &mut Frame, plan: &[Operation], state: &AppState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(3)])
        .split(f.area());

    let mut items: Vec<ListItem> = if plan.is_empty() {
        vec![ListItem::new("No operations to perform.")]
    } else {
        plan.iter()
            .map(|op| {
                let mut spans = vec![Span::raw("> ")];
                match op {
                    Operation::Rebase {
                        branch,
                        onto,
                        upstream: _,
                        predicted_conflict,
                    } => {
                        spans.push(Span::raw(format!("git rebase {} {}", onto, branch)));
                        if let Some(conflict) = predicted_conflict {
                            if *conflict {
                                spans.push(Span::styled(
                                    " [CONFLICT]",
                                    Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
                                ));
                            } else {
                                spans.push(Span::styled(
                                    " [CLEAN]",
                                    Style::default()
                                        .fg(Color::Green)
                                        .add_modifier(Modifier::BOLD),
                                ));
                            }
                        }
                    }
                    Operation::Sync {
                        branch,
                        onto,
                        predicted_conflict,
                    } => {
                        spans.push(Span::raw(format!("git sync {} from {}", branch, onto)));
                        if let Some(conflict) = predicted_conflict {
                            if *conflict {
                                spans.push(Span::styled(
                                    " [NOT FF]",
                                    Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
                                ));
                            } else {
                                spans.push(Span::styled(
                                    " [FF]",
                                    Style::default()
                                        .fg(Color::Green)
                                        .add_modifier(Modifier::BOLD),
                                ));
                            }
                        }
                    }
                    _ => {
                        spans.push(Span::raw(format!("{}", op)));
                    }
                }

                ListItem::new(Line::from(spans))
            })
            .collect()
    };

    if state.is_predicting_conflicts {
        items.push(ListItem::new(""));
        let pct = if plan.is_empty() {
            100
        } else {
            ((state.prediction_index + 1) * 100) / plan.len()
        };
        items.push(
            ListItem::new(format!("Predicting conflicts... {}%", pct)).style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::ITALIC),
            ),
        );
    }

    let list = List::new(items).block(
        Block::default()
            .borders(Borders::ALL)
            .title("Planned Operations"),
    );

    f.render_widget(list, chunks[0]);

    let help = Paragraph::new("q: back to tree | c: commit all")
        .block(Block::default().borders(Borders::ALL).title("Help"));
    f.render_widget(help, chunks[1]);

    if let Some(error) = &state.error_message {
        draw_error(f, error);
    }
}
