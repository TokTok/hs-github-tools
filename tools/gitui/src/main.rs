use clap::Parser;
use gitui::engine::Operation;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value = ".")]
    path: String,

    /// Print the branch tree and exit
    #[arg(short, long)]
    tree: bool,

    /// Show remote branches from origin and upstream
    #[arg(short, long)]
    all: bool,

    /// Show the plan to submit a specific branch
    #[arg(long)]
    submit: Option<String>,

    /// Show the plan to converge a specific branch (move to heuristic parent)
    #[arg(long)]
    converge: Option<String>,

    /// Show the plan to sync a branch with its upstream
    #[arg(long)]
    sync: Option<String>,

    /// Emit machine-readable JSON instead of human output
    #[arg(long)]
    json: bool,

    /// Execute a single rebase operation: --rebase BRANCH ONTO [UPSTREAM]
    #[arg(long, num_args = 2..=3, value_names = ["BRANCH", "ONTO", "UPSTREAM"])]
    rebase: Option<Vec<String>>,

    /// Report current repo state (rebase-in-progress, conflicted paths, etc.)
    #[arg(long)]
    status: bool,

    /// Execute the plan instead of printing it. Works with --submit and
    /// --sync; the latter runs the cascade-aware plan (build_cascade_plan
    /// + apply_plan). Exits non-zero if any op was not Ok.
    #[arg(long)]
    apply: bool,

    /// With --sync --apply: leave a branch that conflicts as it was and go on
    /// with the branches that don't sit on it, instead of stopping in the
    /// conflict.
    #[arg(long, requires = "apply")]
    keep_going: bool,
}

fn warn_skipped(skipped: &[String]) {
    if !skipped.is_empty() {
        eprintln!(
            "warning: {} branch(es) skipped from cascade — no detectable parent:",
            skipped.len()
        );
        for b in skipped {
            eprintln!("  - {}", b);
        }
        eprintln!("run `gitui --converge BRANCH` to anchor manually.");
    }
}

fn print_op_results(results: &[gitui::OpResult]) {
    for r in results {
        match r {
            gitui::OpResult::Ok { branch } => println!("  {} ok", branch),
            gitui::OpResult::Conflict {
                branch,
                conflicted_paths,
                rebase_in_progress,
            } => {
                println!("  {} CONFLICT", branch);
                for p in conflicted_paths {
                    println!("    - {}", p);
                }
                if *rebase_in_progress {
                    println!(
                        "  rebase in progress on {}; resolve and re-run `--apply` to continue",
                        branch
                    );
                }
            }
            gitui::OpResult::Error { branch, message } => {
                println!("  {} ERROR: {}", branch, message);
            }
            gitui::OpResult::Blocked { branch, parent } => {
                println!("  {} not rebased: {} has a conflict", branch, parent);
            }
        }
    }
}

fn emit<T: serde::Serialize + std::fmt::Debug>(value: &T, json: bool) -> anyhow::Result<()> {
    if json {
        println!("{}", serde_json::to_string(value)?);
    } else {
        println!("{:#?}", value);
    }
    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(rebase) = args.rebase {
        let branch = rebase[0].clone();
        let onto = rebase[1].clone();
        let upstream = rebase.get(2).cloned();
        let op = Operation::Rebase {
            branch,
            onto,
            upstream,
            predicted_conflict: None,
        };
        let res = gitui::execute_op(&args.path, &op)?;
        emit(&res, args.json)?;
        let ok = matches!(res, gitui::OpResult::Ok { .. });
        std::process::exit(if ok { 0 } else { 1 });
    }

    if args.status {
        let s = gitui::repo_status(&args.path)?;
        emit(&s, args.json)?;
        return Ok(());
    }

    if let Some(branch_name) = args.submit {
        let plan = gitui::build_submit_plan(&args.path, &branch_name)?;
        if args.apply {
            let results = gitui::apply_plan(&args.path, &plan)?;
            let all_ok = results
                .iter()
                .all(|r| matches!(r, gitui::OpResult::Ok { .. }));
            if args.json {
                println!("{}", serde_json::to_string(&results)?);
            } else {
                println!("Submitting {}:", branch_name);
                print_op_results(&results);
            }
            std::process::exit(if all_ok { 0 } else { 1 });
        }
        if args.json {
            println!("{}", serde_json::to_string(&plan)?);
        } else {
            gitui::print_plan_to(&plan, &mut std::io::stdout())?;
        }
    } else if let Some(branch_name) = args.converge {
        let plan = gitui::build_converge_plan(&args.path, &branch_name)?;
        if args.json {
            println!("{}", serde_json::to_string(&plan)?);
        } else {
            gitui::print_plan_to(&plan, &mut std::io::stdout())?;
        }
    } else if let Some(branch_name) = args.sync {
        if args.apply {
            let plan = gitui::build_cascade_plan(&args.path, &branch_name)?;
            warn_skipped(&plan.skipped_branches);
            let on_conflict = if args.keep_going {
                gitui::OnConflict::Abort
            } else {
                gitui::OnConflict::Stop
            };
            let results = gitui::apply_plan_with(&args.path, &plan, on_conflict)?;
            let all_ok = results
                .iter()
                .all(|r| matches!(r, gitui::OpResult::Ok { .. }));
            if args.json {
                println!("{}", serde_json::to_string(&results)?);
            } else {
                println!("Applying plan for {}:", branch_name);
                print_op_results(&results);
            }
            std::process::exit(if all_ok { 0 } else { 1 });
        }
        // Use the cascade plan for preview too, so `--sync BRANCH` and
        // `--sync BRANCH --apply` always agree on what work will be done.
        let plan = gitui::build_cascade_plan(&args.path, &branch_name)?;
        warn_skipped(&plan.skipped_branches);
        if args.json {
            println!("{}", serde_json::to_string(&plan)?);
        } else {
            gitui::print_plan_to(&plan, &mut std::io::stdout())?;
        }
    } else if args.tree {
        gitui::print_tree(&args.path, args.all)?;
    } else {
        gitui::run(&args.path).await?;
    }
    Ok(())
}
