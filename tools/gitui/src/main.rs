use clap::Parser;

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
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    if let Some(branch_name) = args.submit {
        gitui::print_submit_plan(&args.path, &branch_name)?;
    } else if let Some(branch_name) = args.converge {
        gitui::print_converge_plan(&args.path, &branch_name)?;
    } else if let Some(branch_name) = args.sync {
        gitui::print_sync_plan(&args.path, &branch_name)?;
    } else if args.tree {
        gitui::print_tree(&args.path, args.all)?;
    } else {
        gitui::run(&args.path).await?;
    }
    Ok(())
}
