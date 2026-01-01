use gitui::state::{AppState, Msg};
use gitui::ui;
use ratatui::{Terminal, backend::TestBackend};

#[test]
fn test_ui_renders_error_message() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState {
        error_message: Some("Detailed error message here".to_string()),
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_error = false;
    let mut found_msg = false;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("Error") {
            found_error = true;
        }
        if line.contains("Detailed error message here") {
            found_msg = true;
        }
    }

    assert!(found_error, "Error title not found in UI");
    assert!(found_msg, "Error message body not found in UI");
}

#[test]
fn test_error_dismissal() {
    let mut state = AppState {
        error_message: Some("Some error".to_string()),
        ..AppState::default()
    };

    // Any key should dismiss the error
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('x'),
        KeyModifiers::NONE,
    )));

    assert!(
        state.error_message.is_none(),
        "Error message should be dismissed on key press"
    );
}
