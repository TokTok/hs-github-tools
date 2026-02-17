use crate::state::{AppState, Effect, Msg};

pub fn reduce(mut state: AppState, msg: Msg) -> (AppState, Vec<Effect>) {
    let effects = state.update(msg);
    (state, effects)
}
