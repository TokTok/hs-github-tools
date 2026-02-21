use gitui::testing::mock_oid;
use gitui::topology::{Intent, VirtualTopology};
use proptest::prelude::*;
use std::collections::HashSet;

/// Generates a random DAG of branch names and their parent relationships.
fn arb_topology(
    max_branches: usize,
) -> impl Strategy<Value = (Vec<String>, Vec<(String, String)>)> {
    // Generate between 1 and max_branches unique branch names
    prop::collection::vec("[a-z]{1,5}", 1..max_branches)
        .prop_map(|names| {
            let mut unique_names = HashSet::new();
            names
                .into_iter()
                .filter(|name| unique_names.insert(name.clone()))
                .collect::<Vec<_>>()
        })
        .prop_flat_map(|names| {
            let n = names.len();
            let mut strategies = Vec::new();
            for i in 1..n {
                let name = names[i].clone();
                let possible_parents = names[0..i].to_vec();
                strategies.push(prop::option::weighted(
                    0.7,
                    (Just(name), prop::sample::select(possible_parents)),
                ));
            }
            (Just(names), strategies)
        })
        .prop_map(|(names, relationships)| {
            let rels: Vec<(String, String)> = relationships.into_iter().flatten().collect();
            (names, rels)
        })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn test_flatten_is_complete_and_consistent(
        (names, rels) in arb_topology(20)
    ) {
        let mut topo = VirtualTopology::new();
        for (i, name) in names.iter().enumerate() {
            topo.add_branch(name, mock_oid(i as u8));
        }

        for (child, parent) in rels {
            topo.set_parent(&child, Some(&parent), None, Intent::Implicit).unwrap();
        }

        let flattened = topo.flatten();

        // 1. Completeness: All branches must be present exactly once
        prop_assert_eq!(flattened.len(), names.len(), "Flattened length {} does not match names length {}", flattened.len(), names.len());
        let flattened_names: HashSet<_> = flattened.iter().map(|(n, _)| n.clone()).collect();
        prop_assert_eq!(flattened_names.len(), names.len(), "Duplicate names in flattened result");
        for name in &names {
            prop_assert!(flattened_names.contains(name), "Name '{}' missing from flattened result", name);
        }

        // 2. Hierarchical Consistency: Parent must precede its children
        for (i, (name, depth)) in flattened.iter().enumerate() {
            let parents = topo.get_parents(name);
            if let Some(parent_node) = parents.first() {
                if let Some(parent_name) = parent_node.name() {
                    let parent_pos = flattened.iter().position(|(n, _)| n == parent_name)
                        .expect("Parent should be in flattened list");

                    prop_assert!(parent_pos < i, "Parent '{}' (pos {}) must precede child '{}' (pos {})", parent_name, parent_pos, name, i);

                    // Depth should be parent_depth + 1
                    let parent_depth = flattened[parent_pos].1;
                    prop_assert_eq!(*depth, parent_depth + 1, "Child '{}' depth {} should be parent '{}' depth {} + 1", name, depth, parent_name, parent_depth);
                }
            } else {
                // Root nodes should have depth 0
                prop_assert_eq!(*depth, 0, "Root node '{}' should have depth 0, found {}", name, depth);
            }
        }
    }

    #[test]
    fn test_set_parent_cycle_prevention(
        (names, rels) in arb_topology(10)
    ) {
        let mut topo = VirtualTopology::new();
        for (i, name) in names.iter().enumerate() {
            topo.add_branch(name, mock_oid(i as u8));
        }

        for (child, parent) in rels {
            topo.set_parent(&child, Some(&parent), None, Intent::Implicit).unwrap();
        }

        // Try to create a cycle by picking two random branches A and B
        // where B is an ancestor of A, and setting A as the parent of B.
        let flattened = topo.flatten();
        if flattened.len() >= 2 {
            for i in 0..flattened.len() {
                for j in i+1..flattened.len() {
                    let ancestor_name = &flattened[i].0;
                    let descendant_name = &flattened[j].0;

                    // Verify topological relationship in our flattened list
                    // (we just need to check if there's a path)
                    let mut is_descendant = false;
                    let mut current = descendant_name.clone();
                    while let Some(parent_node) = topo.get_parents(&current).first() {
                        if let Some(p_name) = parent_node.name() {
                            if p_name == ancestor_name {
                                is_descendant = true;
                                break;
                            }
                            current = p_name.to_string();
                        } else {
                            break;
                        }
                    }

                    if is_descendant {
                        // Setting descendant as parent of ancestor should fail
                        let res = topo.set_parent(ancestor_name, Some(descendant_name), None, Intent::Structural);
                        prop_assert!(res.is_err(), "Cycle should have been detected when setting {} as parent of {}", descendant_name, ancestor_name);
                    }
                }
            }
        }
    }
}
