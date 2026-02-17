use crate::engine::BranchInfo;
use crate::state::{AppState, SidebarState};
use crate::ui::SPINNERS;
use crate::ui::common::{centered_rect, draw_error, render_scrollbar};
use ratatui::Frame;
use ratatui::layout::{Alignment, Constraint, Direction, Layout};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Gauge, List, ListItem, Paragraph};

pub fn draw_main(f: &mut Frame, state: &mut AppState) {
    let branch_map: std::collections::HashMap<String, &BranchInfo> =
        state.branches.iter().map(|b| (b.name.clone(), b)).collect();

    let mut constraints = vec![Constraint::Min(0), Constraint::Length(3)];
    if state.is_loading {
        constraints.insert(1, Constraint::Length(3));
    }

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(constraints)
        .split(f.area());

    let spinner = if state.is_loading {
        Some(SPINNERS[state.spinner_index])
    } else {
        None
    };

    let mut title = if let Some(s) = spinner {
        format!("Branches {}", s)
    } else {
        "Branches".to_string()
    };

    if state.is_dirty {
        title = format!("{} [DIRTY - Read Only Mode]", title);
    }

    let max_name_len = state
        .flattened_tree
        .iter()
        .map(|(name, depth)| {
            let mut base_name_len = name.len();
            let intent = state.get_intent(name);
            if intent.pending_localize
                && let Some(slash_idx) = name.find('/')
            {
                base_name_len = name[slash_idx + 1..].len();
            }
            base_name_len + (*depth * 2)
        })
        .max()
        .unwrap_or(0);

    let items: Vec<ListItem> = state
        .flattened_tree
        .iter()
        .map(|(name, depth)| {
            let default_info = BranchInfo {
                name: name.clone(),
                ..Default::default()
            };
            let branch_info = branch_map.get(name).copied().unwrap_or(&default_info);
            render_branch_row(name, *depth, branch_info, state, max_name_len)
        })
        .collect();

    let list = List::new(items)
        .block(Block::default().borders(Borders::ALL).title(title))
        .highlight_style(
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        )
        .highlight_symbol(">>");

    let main_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(60), Constraint::Percentage(40)])
        .split(chunks[0]);

    let content_length = state.flattened_tree.len();

    f.render_stateful_widget(list, main_layout[0], &mut state.list_state);

    render_scrollbar(f, main_layout[0], content_length, state.list_state.offset());

    let log_items: Vec<ListItem> = match &state.sidebar {
        SidebarState::Ready { commits, .. } => {
            if commits.is_empty() {
                vec![
                    ListItem::new("(No unique commits)")
                        .style(Style::default().fg(Color::DarkGray)),
                ]
            } else {
                commits
                    .iter()
                    .map(|commit| {
                        let header = Line::from(vec![
                            Span::styled(&commit.id, Style::default().fg(Color::Yellow)),
                            Span::raw(" "),
                            Span::styled(&commit.author, Style::default().fg(Color::Cyan)),
                        ]);
                        let summary = Line::from(vec![Span::raw("  "), Span::raw(&commit.summary)]);
                        ListItem::new(vec![header, summary, Line::from("")])
                    })
                    .collect()
            }
        }
        _ => Vec::new(),
    };

    let mut log_title = "Recent Commits".to_string();
    if state.is_log_loading() {
        log_title = format!("Recent Commits {}", SPINNERS[state.spinner_index]);
    }

    let log_list =
        List::new(log_items).block(Block::default().borders(Borders::ALL).title(log_title));
    f.render_widget(log_list, main_layout[1]);

    let help_chunk = if state.is_loading {
        let gauge = Gauge::default()
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title(state.progress_message.clone()),
            )
            .gauge_style(Style::default().fg(Color::Yellow))
            .percent((state.progress_percentage * 100.0) as u16);
        f.render_widget(gauge, chunks[1]);
        chunks[2]
    } else {
        chunks[1]
    };

    let help_parts = generate_help_parts(state, &branch_map);
    let help = Paragraph::new(help_parts.join(" | "))
        .block(Block::default().borders(Borders::ALL).title("Help"));
    f.render_widget(help, help_chunk);

    if state.show_quit_confirmation {
        let area = centered_rect(40, 7, f.area());
        let popup_block = Block::default()
            .title("Confirm Quit")
            .borders(Borders::ALL)
            .style(Style::default().fg(Color::Yellow));

        let message =
            Paragraph::new("\nYou have pending operations!\n\nDiscard changes and quit? (y/n)")
                .block(popup_block)
                .style(Style::default().add_modifier(Modifier::BOLD))
                .alignment(Alignment::Center);

        f.render_widget(Clear, area);
        f.render_widget(message, area);
    }

    if let Some(error) = &state.error_message {
        draw_error(f, error);
    }
}

pub fn render_branch_row(
    name: &str,
    depth: usize,
    branch_info: &BranchInfo,
    state: &AppState,
    max_name_len: usize,
) -> ListItem<'static> {
    let intent = state.get_intent(name);
    let indent = "  ".repeat(depth);
    let mut style = Style::default();

    let is_grabbed = state.grabbed_branch.as_deref() == Some(name);
    let is_target_parent = state.target_parent.as_deref() == Some(name);

    let mut base_name = name.to_string();
    if intent.pending_localize
        && let Some(slash_idx) = name.find('/')
    {
        base_name = name[slash_idx + 1..].to_string();
    }

    let display_name = format!("{}{}", indent, base_name);
    let padding = " ".repeat(max_name_len.saturating_sub(display_name.len()) + 2);

    let mut ahead = branch_info.ahead;
    let mut behind = branch_info.behind;

    if intent.pending_reset {
        ahead = 0;
        behind = 0;
    } else if intent.pending_push {
        ahead = 0;
    }

    if branch_info.is_remote {
        style = style.fg(Color::DarkGray);
    }

    if ahead > 0 || behind > 0 {
        style = style.fg(Color::Yellow);
    }

    if is_grabbed {
        style = style.fg(Color::Magenta).add_modifier(Modifier::BOLD);
    }

    if is_target_parent {
        style = style.add_modifier(Modifier::UNDERLINED);
    }

    let effective_parent = intent
        .parent
        .flatten()
        .or_else(|| branch_info.original_parent.clone());
    let is_reparented = effective_parent != branch_info.original_parent
        || (is_grabbed && state.target_parent != branch_info.original_parent);

    if is_reparented {
        style = style.fg(Color::Cyan);
    }

    let mut spans = vec![Span::styled(display_name, style)];

    if !branch_info.aliases.is_empty() {
        spans.push(Span::raw(padding));
        spans.push(Span::styled(
            branch_info.aliases.join(", "),
            Style::default().fg(Color::DarkGray),
        ));
    }

    if ahead > 0 || behind > 0 {
        spans.push(Span::styled(
            format!(" ({}↑ {}↓)", ahead, behind),
            Style::default().fg(Color::Yellow),
        ));
    }

    if intent.pending_push {
        spans.push(Span::styled(
            " [PUSH]",
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::ITALIC),
        ));
    }

    if intent.pending_submit {
        spans.push(Span::styled(
            " [SUBMIT]",
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        ));
    } else if branch_info.can_submit() {
        spans.push(Span::styled(
            " [READY]",
            Style::default()
                .fg(Color::Green)
                .add_modifier(Modifier::BOLD),
        ));
    }

    if intent.pending_reset {
        if branch_info.upstream.is_some() {
            spans.push(Span::styled(" [REBASE]", Style::default().fg(Color::Cyan)));
        } else {
            spans.push(Span::styled(
                " [RESET]",
                Style::default()
                    .fg(Color::Gray)
                    .add_modifier(Modifier::CROSSED_OUT),
            ));
        }
    } else if effective_parent != branch_info.original_parent {
        spans.push(Span::styled(" [REBASE]", Style::default().fg(Color::Cyan)));
    }

    if intent.pending_delete {
        spans.push(Span::styled(
            " [DELETE]",
            Style::default()
                .fg(Color::Gray)
                .add_modifier(Modifier::CROSSED_OUT),
        ));
    } else if branch_info.is_merged {
        spans.push(Span::styled(" [MERGED]", Style::default().fg(Color::Green)));
    }

    if intent.pending_localize {
        spans.push(Span::styled(
            " [LOCALIZE]",
            Style::default().fg(Color::Cyan),
        ));
    }

    if intent.pending_amend {
        if intent.pending_amend_message.is_some() {
            spans.push(Span::styled(
                " [AMEND MSG]",
                Style::default().fg(Color::Cyan),
            ));
        } else {
            spans.push(Span::styled(" [AMEND]", Style::default().fg(Color::Cyan)));
        }
    }

    if let Some(new_name) = &intent.pending_rename {
        spans.push(Span::styled(
            format!(" [RENAME -> {}]", new_name),
            Style::default().fg(Color::Cyan),
        ));
    }

    if is_reparented && intent.has_conflict {
        spans.push(Span::styled(
            " (CONFLICT)",
            Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
        ));
    }

    let is_diverged = (branch_info.heuristic_parent.is_some()
        && effective_parent.as_ref() != branch_info.heuristic_parent.as_ref())
        || (branch_info.original_parent.is_some() && branch_info.parent_behind > 0);

    if is_diverged {
        spans.push(Span::styled(
            " [DIVERGED]",
            Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
        ));
    }

    ListItem::new(Line::from(spans))
}

fn generate_help_parts(
    state: &AppState,
    branch_map: &std::collections::HashMap<String, &BranchInfo>,
) -> Vec<&'static str> {
    let mut help_parts = vec!["q: quit", "j/k: navigate", "v: preview", "c: commit"];
    if state.grabbed_branch.is_some() {
        help_parts = vec![
            "esc: cancel",
            "space: release",
            "j/k: select parent",
            "h: move to root",
        ];
    } else {
        help_parts.push("a: toggle all");
        if let Some(idx) = state.list_state.selected()
            && idx < state.flattened_tree.len()
        {
            let name = &state.flattened_tree[idx].0;
            let intent = state.get_intent(name);
            if let Some(b) = branch_map.get(name) {
                if b.is_local {
                    if !state.is_dirty || intent.pending_amend {
                        help_parts.push("space: grab");
                    }

                    let effective_parent = intent
                        .parent
                        .flatten()
                        .or_else(|| b.original_parent.clone());
                    let will_change = intent.pending_amend
                        || intent.pending_split.is_some()
                        || effective_parent != b.original_parent
                        || (name == &state.initial_branch && state.is_dirty);
                    if intent.pending_push || b.ahead > 0 || will_change {
                        help_parts.push("p: push");
                    }

                    if b.can_submit() {
                        help_parts.push("s: submit");
                    }
                    if !state.is_dirty {
                        help_parts.push("r: reset");
                    }
                    if name == &state.initial_branch {
                        help_parts.push("f: fetch/localize");
                        help_parts.push("m: amend");
                        help_parts.push("M: amend w/ msg");
                        help_parts.push("R: rename");
                        help_parts.push("c: commit plan");
                    }
                    if b.parent_ahead == 1 {
                        help_parts.push("x: split");
                    }
                    if let Some(h_parent) = &b.heuristic_parent
                        && effective_parent.as_ref() != Some(h_parent)
                    {
                        help_parts.push("u: converge");
                    }
                    help_parts.push("d: delete");
                } else if b.is_remote && !state.is_dirty {
                    help_parts.push("f: localize");
                }
            }
        }
    }
    help_parts
}
