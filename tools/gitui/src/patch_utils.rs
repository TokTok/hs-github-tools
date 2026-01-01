use git2::{Diff, DiffFormat, Oid, Repository, Tree};
use std::collections::HashSet;

pub fn create_filtered_diff<'a>(
    diff: &Diff<'a>,
    selected_hunks: &HashSet<(String, usize)>, // (file_path, hunk_index)
) -> anyhow::Result<Vec<u8>> {
    let mut patch = Vec::new();
    let mut current_file_path = String::new();
    let mut hunk_index = 0;
    let mut hunk_selected = false;
    let mut file_header = Vec::new();
    let mut file_header_printed = false;
    let mut skew: isize = 0;

    diff.print(DiffFormat::Patch, |delta, hunk, line| {
        let path_str = delta
            .new_file()
            .path()
            .and_then(|p| p.to_str())
            .unwrap_or("");

        if path_str != current_file_path {
            if !current_file_path.is_empty()
                && !file_header_printed
                && selected_hunks.contains(&(current_file_path.clone(), 0))
                && hunk_index == 0
            {
                patch.extend_from_slice(&file_header);
            }

            current_file_path = path_str.to_string();
            hunk_index = 0;
            hunk_selected = false;
            file_header.clear();
            file_header_printed = false;
            skew = 0;
        }

        if let Some(hunk) = hunk {
            if line.origin() == 'H' {
                // Hunk header line
                hunk_selected = selected_hunks.contains(&(current_file_path.clone(), hunk_index));

                if hunk_selected {
                    let old_start = hunk.old_start();
                    let old_lines = hunk.old_lines();
                    let new_start = (hunk.new_start() as isize - skew).max(1) as u32;
                    let new_lines = hunk.new_lines();

                    // Reconstruct header with context
                    // We need to preserve the function context if present in the original header
                    let content = String::from_utf8_lossy(line.content());
                    let suffix = if let Some(idx) = content.find(" @@") {
                        // Find the second " @@" which ends the numbers part
                        // Usually header is "@@ -x,y +z,w @@ function_name"
                        // content starts with "@@ "
                        &content[idx..]
                    } else {
                        " @@\n"
                    };

                    // Ensure suffix ends with newline
                    let suffix = if suffix.ends_with('\n') {
                        suffix.to_string()
                    } else {
                        format!("{}\n", suffix)
                    };

                    let header = format!(
                        "@@ -{},{} +{},{}{}",
                        old_start, old_lines, new_start, new_lines, suffix
                    );

                    if !file_header_printed {
                        patch.extend_from_slice(&file_header);
                        file_header_printed = true;
                    }

                    patch.extend_from_slice(header.as_bytes());
                } else {
                    let shift = hunk.new_lines() as isize - hunk.old_lines() as isize;
                    skew += shift;
                }

                hunk_index += 1;
            } else if hunk_selected {
                let origin = line.origin();
                match origin {
                    '+' | '-' | ' ' => {
                        patch.push(origin as u8);
                        patch.extend_from_slice(line.content());
                    }
                    _ => {
                        // Ignore other line types in body
                    }
                }
                // Ensure newline if missing (though git usually provides it or handled by origin)
                if (origin == '+' || origin == '-' || origin == ' ') && !patch.ends_with(b"\n") {
                    // check if line content had newline
                    if !line.content().ends_with(b"\n") {
                        patch.push(b'\n');
                    }
                }
                if origin == '>' || origin == '<' {
                    // We should probably preserve these if they relate to selected hunks
                    patch.extend_from_slice(line.content());
                    if !line.content().ends_with(b"\n") {
                        patch.push(b'\n');
                    }
                }
            }
        } else {
            // This is a file header line
            file_header.extend_from_slice(line.content());
        }
        true
    })?;

    if !current_file_path.is_empty()
        && !file_header_printed
        && selected_hunks.contains(&(current_file_path, 0))
        && hunk_index == 0
    {
        patch.extend_from_slice(&file_header);
    }

    Ok(patch)
}

pub fn apply_selected_hunks_to_tree(
    repo: &Repository,
    base_tree: &Tree,
    diff: &Diff,
    selected_hunks: &HashSet<(String, usize)>,
) -> anyhow::Result<Oid> {
    let patch_data = create_filtered_diff(diff, selected_hunks)?;
    if patch_data.is_empty() {
        return Ok(base_tree.id());
    }

    let filtered_diff = Diff::from_buffer(&patch_data)?;
    let mut index = repo.apply_to_tree(base_tree, &filtered_diff, None)?;
    let tree_oid = index.write_tree_to(repo)?;
    Ok(tree_oid)
}
