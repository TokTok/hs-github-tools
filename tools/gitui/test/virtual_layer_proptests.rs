use gitui::testing::mock_oid;
use gitui::topology::virtual_layer::VirtualLayer;
use gitui::topology::{Intent, VirtualTopology};
use proptest::prelude::*;
use std::collections::HashSet;

#[derive(Debug, Clone)]
struct SplitScenario {
    base_branches: Vec<(String, u8)>,
    base_rels: Vec<(String, String)>,
    hidden: Vec<String>,
    virtuals: Vec<(String, Option<String>, Option<u8>)>,
}

/// Strategy for a base topology and a set of virtual replacements.
fn arb_split_scenario() -> impl Strategy<Value = SplitScenario> {
    // Fixed base structure: master -> feat1 -> feat2
    //                      feat1 -> child1
    //                      feat2 -> child2
    let base_branches = vec![
        ("master".to_string(), 1u8),
        ("feat1".to_string(), 2u8),
        ("feat2".to_string(), 3u8),
        ("child1".to_string(), 4u8),
        ("child2".to_string(), 5u8),
    ];
    let base_rels = vec![
        ("feat1".to_string(), "master".to_string()),
        ("feat2".to_string(), "feat1".to_string()),
        ("child1".to_string(), "feat1".to_string()),
        ("child2".to_string(), "feat2".to_string()),
    ];

    // Decide which of feat1, feat2 to "split" (hide and replace with virtual parts)
    (
        prop::sample::subsequence(vec!["feat1".to_string(), "feat2".to_string()], 1..2),
        prop::collection::vec("[a-z]{1,5}", 1..3), // prefix for virtual parts
    )
        .prop_map(move |(to_split, prefixes)| {
            let mut hidden = Vec::new();
            let mut virtuals = Vec::new();

            for (prefix_idx, name) in to_split.into_iter().enumerate() {
                hidden.push(name.clone());
                // Create a part and then the branch itself
                let part_name = format!("{}-part", prefixes[prefix_idx % prefixes.len()]);

                // The split: parent -> part -> original_name
                // We need to know the parent in the base rels
                let parent = base_rels
                    .iter()
                    .find(|(c, _)| c == &name)
                    .map(|(_, p)| p.clone());

                virtuals.push((part_name.clone(), parent, None));
                virtuals.push((name.clone(), Some(part_name), Some(3u8))); // Use feat2's oid or similar
            }

            SplitScenario {
                base_branches: base_branches.clone(),
                base_rels: base_rels.clone(),
                hidden,
                virtuals,
            }
        })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn test_virtual_layer_relationship_preservation(
        scenario in arb_split_scenario()
    ) {
        let SplitScenario { base_branches, base_rels, hidden, virtuals } = scenario;
        let mut topo = VirtualTopology::new();
        for (name, oid_b) in &base_branches {
            topo.add_branch(name, mock_oid(*oid_b));
        }
        for (child, parent) in &base_rels {
            topo.set_parent(child, Some(parent), None, Intent::Implicit).unwrap();
        }

        let mut layer = VirtualLayer::new();
        for h in &hidden {
            layer.hide_branch(h);
        }
        for (v_name, v_parent, v_oid_b) in &virtuals {
            layer.add_virtual_branch(v_name.clone(), v_parent.clone(), v_oid_b.map(mock_oid));
        }

        layer.apply(&mut topo);
        let flattened = topo.flatten();

        // Property: If a branch 'C' was a child of 'P' in the base,
        // and 'P' was replaced by a virtual branch with the same name 'P',
        // 'P' must still be an ancestor of 'C'.

        for (child, parent) in &base_rels {
            // If the parent was hidden but REPLACED by a virtual branch of the same name
            let is_parent_replaced = hidden.contains(parent) && virtuals.iter().any(|(n, _, _)| n == parent);
            // If the child was NOT truly removed
            let is_child_removed = hidden.contains(child) && !virtuals.iter().any(|(n, _, _)| n == child);

            if is_parent_replaced && !is_child_removed {
                let child_idx = flattened.iter().position(|(n, _)| n == child)
                    .unwrap_or_else(|| panic!("Child {} should exist", child));
                let parent_idx = flattened.iter().position(|(n, _)| n == parent)
                    .unwrap_or_else(|| panic!("Parent {} should exist", parent));

                prop_assert!(parent_idx < child_idx,
                    "Relationship broken: Parent '{}' (pos {}) must precede child '{}' (pos {}) after split",
                    parent, parent_idx, child, child_idx
                );

                // Also verify it's a descendant in the graph
                let mut current = child.clone();
                let mut found = false;
                let mut visited = HashSet::new();
                while let Some(p_node) = topo.get_parents(&current).first() {
                    if let Some(p_name) = p_node.name() {
                        if p_name == parent {
                            found = true;
                            break;
                        }
                        current = p_name.to_string();
                        if !visited.insert(current.clone()) { break; }
                    } else { break; }
                }
                prop_assert!(found, "Child '{}' is no longer a topological descendant of parent '{}'", child, parent);
            }
        }
    }
}
