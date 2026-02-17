use gitui::testing::mock_oid;
use gitui::topology::{Intent, TopologyNode, VirtualTopology};

#[test]
fn test_basic_branch_parenting() {
    let mut topo = VirtualTopology::new();
    let root_oid = mock_oid(0);
    let master_oid = mock_oid(1);
    let feature_oid = mock_oid(2);

    topo.add_branch("master", master_oid);
    topo.add_branch("feature", feature_oid);

    topo.set_parent("master", None, Some(root_oid), Intent::Implicit)
        .unwrap();
    topo.set_parent("feature", Some("master"), None, Intent::Implicit)
        .unwrap();

    let parents = topo.get_parents("feature");
    assert_eq!(parents.len(), 1);
    match &parents[0] {
        TopologyNode::Branch { name, .. } => assert_eq!(name, "master"),
        _ => panic!("Expected branch parent"),
    }
}

#[test]
fn test_flatten_linear() {
    let mut topo = VirtualTopology::new();
    topo.add_branch("a", mock_oid(1));
    topo.add_branch("b", mock_oid(2));
    topo.add_branch("c", mock_oid(3));

    topo.set_parent("b", Some("a"), None, Intent::Implicit)
        .unwrap();
    topo.set_parent("c", Some("b"), None, Intent::Implicit)
        .unwrap();

    let flat = topo.flatten();
    assert_eq!(
        flat,
        vec![
            ("a".to_string(), 0),
            ("b".to_string(), 1),
            ("c".to_string(), 2),
        ]
    );
}

#[test]
fn test_stable_flatten_visual_memory() {
    let mut topo = VirtualTopology::new();
    topo.add_branch("master", mock_oid(1));
    topo.add_branch("feature1", mock_oid(2));
    topo.add_branch("feature2", mock_oid(3));

    topo.set_parent("feature1", Some("master"), None, Intent::Implicit)
        .unwrap();
    topo.set_parent("feature2", Some("master"), None, Intent::Implicit)
        .unwrap();

    // Default alphabetical: feature1, feature2
    assert_eq!(
        topo.flatten(),
        vec![
            ("master".to_string(), 0),
            ("feature1".to_string(), 1),
            ("feature2".to_string(), 1),
        ]
    );

    // Set visual memory to swap them - but alphabetical is now primary
    topo.set_visual_memory(vec![
        "master".to_string(),
        "feature2".to_string(),
        "feature1".to_string(),
    ]);

    // Alphabetical still wins: feature1, feature2
    assert_eq!(
        topo.flatten(),
        vec![
            ("master".to_string(), 0),
            ("feature1".to_string(), 1),
            ("feature2".to_string(), 1),
        ]
    );
}

#[test]
fn test_intent_based_stability() {
    let mut topo = VirtualTopology::new();
    let master = mock_oid(1);
    let feature = mock_oid(2);

    topo.add_branch("master", master);
    topo.add_branch("feature", feature);
    topo.set_parent("feature", Some("master"), None, Intent::Implicit)
        .unwrap();

    // Visual Memory: master, feature
    topo.set_visual_memory(vec!["master".to_string(), "feature".to_string()]);

    // Simulate "Synchronizing" reset (r)
    // Even if it moves to a root OID or alias, we want to maintain visual relationship.
    let root = mock_oid(0);
    topo.set_parent("feature", None, Some(root), Intent::Synchronizing)
        .unwrap();

    // Feature becomes a root but visual memory keeps it after master
    let flat = topo.flatten();
    assert_eq!(flat[0].0, "master");
    assert_eq!(flat[1].0, "feature");
}

#[test]
fn test_cycle_prevention() {
    let mut topo = VirtualTopology::new();
    topo.add_branch("a", mock_oid(1));
    topo.add_branch("b", mock_oid(2));

    topo.set_parent("b", Some("a"), None, Intent::Implicit)
        .unwrap();

    let res = topo.set_parent("a", Some("b"), None, Intent::Structural);
    assert!(res.is_err());
}

#[test]
fn test_flatten_with_commit_bridge() {
    let mut topo = VirtualTopology::new();
    let root_branch = "master";
    let bridge_commit = mock_oid(100);
    let child_branch = "feature";

    topo.add_branch(root_branch, mock_oid(1));
    topo.add_branch(child_branch, mock_oid(2));

    topo.set_parent(child_branch, None, Some(bridge_commit), Intent::Implicit)
        .unwrap();
    topo.add_commit_parent(bridge_commit, root_branch).unwrap();

    let flat = topo.flatten();
    assert_eq!(
        flat,
        vec![("master".to_string(), 0), ("feature".to_string(), 1),]
    );
}

#[test]
fn test_remove_branch() {
    let mut topo = VirtualTopology::new();
    topo.add_branch("a", mock_oid(1));
    topo.add_branch("b", mock_oid(2));
    topo.set_parent("b", Some("a"), None, Intent::Implicit)
        .unwrap();

    topo.remove_branch("a");

    assert!(topo.get_branch_oid("a").is_none());
    assert_eq!(topo.get_parents("b"), Vec::<TopologyNode>::new());
}

#[test]
fn test_list_roots() {
    let mut topo = VirtualTopology::new();
    topo.add_branch("master", mock_oid(1));
    topo.add_branch("feature", mock_oid(2));
    topo.set_parent("feature", Some("master"), None, Intent::Implicit)
        .unwrap();

    assert_eq!(topo.list_roots(), vec!["master"]);
}

#[test]
fn test_chained_structural_move() {
    let mut topo = VirtualTopology::new();
    let r = mock_oid(0);
    let a = mock_oid(1);
    let b = mock_oid(2);
    let m = mock_oid(3);

    topo.add_branch("a", a);
    topo.add_branch("b", b);
    topo.add_branch("master", m);

    topo.set_parent("a", None, Some(r), Intent::Implicit)
        .unwrap();
    topo.set_parent("b", Some("a"), None, Intent::Implicit)
        .unwrap();
    topo.set_parent("master", None, Some(r), Intent::Implicit)
        .unwrap();

    // Stack: a <- b, master
    assert_eq!(
        topo.flatten(),
        vec![
            ("a".to_string(), 0),
            ("b".to_string(), 1),
            ("master".to_string(), 0),
        ]
    );

    // Move 'a' onto 'master' structurally
    topo.set_parent("a", Some("master"), None, Intent::Structural)
        .unwrap();

    assert_eq!(
        topo.flatten(),
        vec![
            ("master".to_string(), 0),
            ("a".to_string(), 1),
            ("b".to_string(), 2),
        ]
    );
}

#[test]
fn test_ambiguous_tie_breaking_no_memory() {
    let mut topo = VirtualTopology::new();
    let common = mock_oid(1);

    // Two branches at the same OID with no visual memory
    topo.add_branch("z-branch", common);
    topo.add_branch("a-branch", common);

    let flat = topo.flatten();
    // Should fall back to alphabetical: a-branch, z-branch
    assert_eq!(flat[0].0, "a-branch");
    assert_eq!(flat[1].0, "z-branch");
}

#[test]
fn test_parent_child_preservation_across_renders() {
    let mut topo = VirtualTopology::new();
    let r = mock_oid(0);
    let a = mock_oid(1);
    let b = mock_oid(2);

    topo.add_branch("root", r);
    topo.add_branch("middle", a);
    topo.add_branch("leaf", b);

    topo.set_parent("middle", Some("root"), None, Intent::Implicit)
        .unwrap();
    topo.set_parent("leaf", Some("middle"), None, Intent::Implicit)
        .unwrap();

    // Store order
    let order: Vec<String> = topo.flatten().into_iter().map(|(n, _)| n).collect();
    assert_eq!(order, vec!["root", "middle", "leaf"]);

    // New render with visual memory
    let mut topo2 = VirtualTopology::new();
    topo2.set_visual_memory(order);
    topo2.add_branch("root", r);
    topo2.add_branch("middle", a);
    topo2.add_branch("leaf", b);
    topo2
        .set_parent("middle", Some("root"), None, Intent::Implicit)
        .unwrap();
    topo2
        .set_parent("leaf", Some("middle"), None, Intent::Implicit)
        .unwrap();

    assert_eq!(
        topo2.flatten(),
        vec![
            ("root".to_string(), 0),
            ("middle".to_string(), 1),
            ("leaf".to_string(), 2),
        ]
    );
}

#[test]
fn test_diamond_merge_visit_uniqueness() {
    let mut topo = VirtualTopology::new();
    let base = mock_oid(0);
    let side1 = mock_oid(1);
    let side2 = mock_oid(2);
    let merge = mock_oid(3);

    topo.add_branch("base", base);
    topo.add_branch("side1", side1);
    topo.add_branch("side2", side2);
    topo.add_branch("merge", merge);

    topo.set_parent("side1", Some("base"), None, Intent::Implicit)
        .unwrap();
    topo.set_parent("side2", Some("base"), None, Intent::Implicit)
        .unwrap();

    // Manually create a second parent edge for merge
    let merge_idx = topo.branches["merge"];
    let side1_idx = topo.branches["side1"];
    let side2_idx = topo.branches["side2"];
    topo.graph.add_edge(merge_idx, side1_idx, Intent::Implicit);
    topo.graph.add_edge(merge_idx, side2_idx, Intent::Implicit);

    let flat = topo.flatten();
    // "merge" should only appear once despite having two parents
    let merge_count = flat.iter().filter(|(n, _)| n == "merge").count();
    assert_eq!(merge_count, 1);
}

#[test]
fn test_visual_memory_with_new_branches() {
    let mut topo = VirtualTopology::new();
    topo.add_branch("old1", mock_oid(1));
    topo.add_branch("old2", mock_oid(2));

    topo.set_visual_memory(vec!["old2".to_string(), "old1".to_string()]);

    // Add a NEW branch that wasn't in memory
    topo.add_branch("newbie", mock_oid(3));

    let flat = topo.flatten();
    // alphabetical: newbie < old1 < old2
    assert_eq!(flat[0].0, "newbie");
    assert_eq!(flat[1].0, "old1");
    assert_eq!(flat[2].0, "old2");
}

#[test]
fn test_resolve_visible_parent() {
    use gitui::topology::HistoryContext;
    let mut history = HistoryContext::new();

    let oid1 = mock_oid(1);
    let oid2 = mock_oid(2);
    let oid3 = mock_oid(3);

    history
        .oid_to_visible_branch
        .insert(oid1, "master".to_string());
    history
        .oid_to_visible_branch
        .insert(oid2, "feat1".to_string());

    history.oid_to_ancestor.insert(oid3, Some(oid2));
    history.oid_to_ancestor.insert(oid2, Some(oid1));

    // From oid3, the nearest visible is feat1
    assert_eq!(
        history.resolve_visible_parent(oid3, None),
        Some("feat1".to_string())
    );

    // From oid3, excluding feat1, the nearest visible is master
    assert_eq!(
        history.resolve_visible_parent(oid3, Some("feat1")),
        Some("master".to_string())
    );

    // From oid2, excluding feat1, the nearest visible is master
    assert_eq!(
        history.resolve_visible_parent(oid2, Some("feat1")),
        Some("master".to_string())
    );

    // From oid1, excluding master, there is nothing
    assert_eq!(history.resolve_visible_parent(oid1, Some("master")), None);
}

// end of file
