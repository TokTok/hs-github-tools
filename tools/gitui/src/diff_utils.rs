use git2::{Diff, DiffFormat, DiffLineType};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum LineType {
    #[default]
    Context,
    Addition,
    Deletion,
    Header,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DiffLine {
    pub content: String,
    pub line_type: LineType,
    pub old_lineno: Option<u32>,
    pub new_lineno: Option<u32>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Hunk {
    pub header: String,
    pub lines: Vec<DiffLine>,
    pub old_start: u32,
    pub old_lines: u32,
    pub new_start: u32,
    pub new_lines: u32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FileDiff {
    pub path: String,
    pub hunks: Vec<Hunk>,
}

pub fn parse_diff(diff: &Diff) -> anyhow::Result<Vec<FileDiff>> {
    let mut file_diffs = Vec::new();
    let mut current_file: Option<FileDiff> = None;
    let mut current_hunk: Option<Hunk> = None;

    diff.print(DiffFormat::Patch, |delta, hunk, line| {
        let path_str = delta
            .new_file()
            .path()
            .and_then(|p| p.to_str())
            .unwrap_or("");

        let is_different_file = match &current_file {
            Some(f) => f.path != path_str,
            None => true,
        };

        if is_different_file {
            if let Some(h) = current_hunk.take()
                && let Some(ref mut f) = current_file
            {
                f.hunks.push(h);
            }
            if let Some(f) = current_file.take() {
                file_diffs.push(f);
            }
            current_file = Some(FileDiff {
                path: path_str.to_string(),
                hunks: Vec::new(),
            });
        }

        if let Some(h) = hunk {
            let is_different_hunk = match &current_hunk {
                Some(curr) => {
                    curr.old_start != h.old_start()
                        || curr.old_lines != h.old_lines()
                        || curr.new_start != h.new_start()
                        || curr.new_lines != h.new_lines()
                }
                None => true,
            };

            if is_different_hunk {
                if let Some(h_val) = current_hunk.take()
                    && let Some(ref mut f) = current_file
                {
                    f.hunks.push(h_val);
                }

                let header = std::str::from_utf8(h.header())
                    .unwrap_or("")
                    .trim()
                    .to_string();

                current_hunk = Some(Hunk {
                    header,
                    lines: Vec::new(),
                    old_start: h.old_start(),
                    old_lines: h.old_lines(),
                    new_start: h.new_start(),
                    new_lines: h.new_lines(),
                });
            }
        }

        let line_type = match line.origin_value() {
            DiffLineType::Context => LineType::Context,
            DiffLineType::Addition => LineType::Addition,
            DiffLineType::Deletion => LineType::Deletion,
            _ => LineType::Header,
        };

        if line_type != LineType::Header
            && let Some(ref mut h_val) = current_hunk
        {
            h_val.lines.push(DiffLine {
                content: std::str::from_utf8(line.content())
                    .unwrap_or("")
                    .to_string(),
                line_type,
                old_lineno: line.old_lineno(),
                new_lineno: line.new_lineno(),
            });
        }

        true
    })?;

    if let Some(h) = current_hunk
        && let Some(ref mut f) = current_file
    {
        f.hunks.push(h);
    }
    if let Some(f) = current_file {
        file_diffs.push(f);
    }

    Ok(file_diffs)
}
