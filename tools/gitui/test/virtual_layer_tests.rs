use git2::Oid;
use gitui::topology::VirtualTopology;
use gitui::topology::virtual_layer::VirtualLayer;

#[test]
fn test_virtual_layer_apply() {
    let mut topo = VirtualTopology::new();
    let root_oid = Oid::from_bytes(&[1; 20]).unwrap();
    let branch_oid = Oid::from_bytes(&[2; 20]).unwrap();

    topo.add_branch("master", root_oid);
    topo.add_branch("feat", branch_oid);
    let _ = topo.set_parent(
        "feat",
        Some("master"),
        None,
        gitui::topology::Intent::Implicit,
    );

    let mut layer = VirtualLayer::new();
    layer.hide_branch("feat");
    layer.add_virtual_branch("feat-1".to_string(), Some("master".to_string()), None);
    layer.add_virtual_branch(
        "feat".to_string(),
        Some("feat-1".to_string()),
        Some(branch_oid),
    );

    layer.apply(&mut topo);
    topo.set_visual_memory(vec![
        "master".to_string(),
        "feat-1".to_string(),
        "feat".to_string(),
    ]);

    let flattened = topo.flatten();
    assert_eq!(flattened.len(), 3);
    assert_eq!(flattened[0], ("master".to_string(), 0));
    assert_eq!(flattened[1], ("feat-1".to_string(), 1));
    assert_eq!(flattened[2], ("feat".to_string(), 2));
}

#[test]
fn test_child_preservation_after_split_replacement() {
    let mut topo = VirtualTopology::new();
    let root_oid = Oid::from_bytes(&[1; 20]).unwrap();
    let parent_oid = Oid::from_bytes(&[2; 20]).unwrap();
    let child_oid = Oid::from_bytes(&[3; 20]).unwrap();

    topo.add_branch("master", root_oid);
    topo.add_branch("parent", parent_oid);
    topo.add_branch("child", child_oid);

    let _ = topo.set_parent(
        "parent",
        Some("master"),
        None,
        gitui::topology::Intent::Implicit,
    );
    let _ = topo.set_parent(
        "child",
        Some("parent"),
        None,
        gitui::topology::Intent::Implicit,
    );

    let mut layer = VirtualLayer::new();
    // Splitting "parent" into "part1" and "parent"
    layer.hide_branch("parent");
    layer.add_virtual_branch("part1".to_string(), Some("master".to_string()), None);
    layer.add_virtual_branch(
        "parent".to_string(),
        Some("part1".to_string()),
        Some(parent_oid),
    );

    layer.apply(&mut topo);

    let flattened = topo.flatten();
    // master
    //   part1
    //     parent
    //       child  <-- We want this!

    let child_pos = flattened.iter().position(|(n, _)| n == "child");
    assert!(
        child_pos.is_some(),
        "Child 'child' should be preserved in the flattened tree"
    );
    let (name, depth) = &flattened[child_pos.unwrap()];
    assert_eq!(
        *depth, 3,
        "Child {} should be at depth 3 (master -> part1 -> parent -> child)",
        name
    );
}

#[test]
fn test_child_preservation_complex_split() {
    let mut topo = VirtualTopology::new();
    let root_oid = Oid::from_bytes(&[1; 20]).unwrap();
    let parent_oid = Oid::from_bytes(&[2; 20]).unwrap();
    let child_oid = Oid::from_bytes(&[3; 20]).unwrap();

    topo.add_branch("master", root_oid);
    topo.add_branch("noise-ik", parent_oid);
    topo.add_branch("nc-modular", child_oid);

    topo.set_parent(
        "noise-ik",
        Some("master"),
        None,
        gitui::topology::Intent::Implicit,
    )
    .unwrap();
    topo.set_parent(
        "nc-modular",
        Some("noise-ik"),
        None,
        gitui::topology::Intent::Implicit,
    )
    .unwrap();

    let mut layer = VirtualLayer::new();
    layer.hide_branch("noise-ik");
    layer.add_virtual_branch("part1".to_string(), Some("master".to_string()), None);
    layer.add_virtual_branch("part2".to_string(), Some("part1".to_string()), None);
    layer.add_virtual_branch(
        "noise-ik".to_string(),
        Some("part2".to_string()),
        Some(parent_oid),
    );

    layer.apply(&mut topo);

    let flattened = topo.flatten();
    // master (0)
    //   part1 (1)
    //     part2 (2)
    //       noise-ik (3)
    //         nc-modular (4)

    let nc_pos = flattened
        .iter()
        .position(|(n, _)| n == "nc-modular")
        .expect("nc-modular missing");
    assert_eq!(flattened[nc_pos].1, 4, "nc-modular should be at depth 4");

    // Verify parent of nc-modular is noise-ik
    let parents = topo.get_parents("nc-modular");
    assert_eq!(parents.len(), 1);
    assert_eq!(parents[0].name(), Some("noise-ik"));
}
