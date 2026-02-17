use git2::{BranchType, Signature, Time};
use gitui::engine::{Git, RealGit};
use gitui::testing::setup_repo;
use std::collections::HashSet;

#[test]
fn test_repro_issue_noise_ik_split() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create base commit
    let path_crypto_test = dir.path().join("auto_tests").join("crypto_test.c");
    let path_crypto_core_c = dir.path().join("toxcore").join("crypto_core.c");
    let path_crypto_core_h = dir.path().join("toxcore").join("crypto_core.h");
    let path_crypto_core_test = dir.path().join("toxcore").join("crypto_core_test.cc");

    std::fs::create_dir_all(dir.path().join("auto_tests")).unwrap();
    std::fs::create_dir_all(dir.path().join("toxcore")).unwrap();

    // Content designed to produce 3 hunks when modified
    let crypto_test_base = r#"
#include "../toxcore/os_memory.h"
#include "../toxcore/os_random.h"
#include "check_compat.h"

static void rand_bytes(const Random *rng, uint8_t *b, size_t blen)
{
    // ...
}

static void test_memzero(void)
{
    // ...
}

int main(void)
{
    setvbuf(stdout, nullptr, _IONBF, 0);

    test_very_large_data();
    test_increment_nonce();
    test_memzero();

    return 0;
}
"#;
    std::fs::write(&path_crypto_test, crypto_test_base).unwrap();
    std::fs::write(&path_crypto_core_c, "base content c\n").unwrap();
    std::fs::write(&path_crypto_core_h, "base content h\n").unwrap();
    std::fs::write(&path_crypto_core_test, "base content test\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index
            .add_path(std::path::Path::new("auto_tests/crypto_test.c"))
            .unwrap();
        index
            .add_path(std::path::Path::new("toxcore/crypto_core.c"))
            .unwrap();
        index
            .add_path(std::path::Path::new("toxcore/crypto_core.h"))
            .unwrap();
        index
            .add_path(std::path::Path::new("toxcore/crypto_core_test.cc"))
            .unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base commit", &tree, &[&parent])
            .unwrap();
    }

    // 2. Modify files to create the "noise-ik" state
    let crypto_test_mod = r#"
#include "../toxcore/os_memory.h"
#include "../toxcore/os_random.h"
#include "check_compat.h"
// TODO(goldroom): necessary to print bytes
// #include "../other/fun/create_common.h"

static void rand_bytes(const Random *rng, uint8_t *b, size_t blen)
{
    // ...
}

static void test_memzero(void)
{
    // ...
}

/* Noise_IK_25519_ChaChaPoly_BLAKE2b test vectors */
static void test_noiseik(void)
{
    // ... huge implementation ...
}

int main(void)
{
    setvbuf(stdout, nullptr, _IONBF, 0);

    test_very_large_data();
    test_increment_nonce();
    test_memzero();
    test_noiseik();

    return 0;
}
"#;

    std::fs::write(&path_crypto_test, crypto_test_mod).unwrap();
    std::fs::write(&path_crypto_core_c, "base content c\nmodified\n").unwrap();
    std::fs::write(&path_crypto_core_h, "base content h\nmodified\n").unwrap();
    std::fs::write(&path_crypto_core_test, "base content test\nmodified\n").unwrap();

    let feat_oid = {
        let mut index = repo.index().unwrap();
        index
            .add_path(std::path::Path::new("auto_tests/crypto_test.c"))
            .unwrap();
        index
            .add_path(std::path::Path::new("toxcore/crypto_core.c"))
            .unwrap();
        index
            .add_path(std::path::Path::new("toxcore/crypto_core.h"))
            .unwrap();
        index
            .add_path(std::path::Path::new("toxcore/crypto_core_test.cc"))
            .unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("refs/heads/noise-ik"),
            &sig,
            &sig,
            "noise-ik commit",
            &tree,
            &[&parent],
        )
        .unwrap()
    };

    // Setup branches
    let giant_commit = repo.find_commit(feat_oid).unwrap();
    repo.set_head_detached(feat_oid).unwrap();
    repo.branch("noise-ik", &giant_commit, true).unwrap();

    let base_commit = giant_commit.parents().next().unwrap();
    repo.branch(&master, &base_commit, true).unwrap();

    let git = RealGit::new(path_str).unwrap();

    // 3. Define the split
    let mut selected = HashSet::new();
    selected.insert(("toxcore/crypto_core.c".to_string(), 0));
    selected.insert(("toxcore/crypto_core.h".to_string(), 0));
    selected.insert(("toxcore/crypto_core_test.cc".to_string(), 0));

    // Select hunks 1 and 2 for crypto_test.c (0-based)
    // In code we need to select indices.
    // The diff will have:
    // Hunk 0: The include
    // Hunk 1: The function
    // Hunk 2: The call
    // We want function (1) and call (2).
    selected.insert(("auto_tests/crypto_test.c".to_string(), 1));
    selected.insert(("auto_tests/crypto_test.c".to_string(), 2));

    let split_data = gitui::engine::SplitData {
        parts: vec![gitui::engine::SplitPartData {
            name: "noise-ik-part1".to_string(),
            commit_message: "Split part 1".to_string(),
            selected_hunks: selected,
        }],
    };

    // Split 'noise-ik' branch, using 'master' as the base/upstream
    git.split_branch("noise-ik", &master, &split_data).unwrap();

    // 4. Verify results
    let part1_branch = repo
        .find_branch("noise-ik-part1", BranchType::Local)
        .unwrap();
    let part1_commit = part1_branch.get().peel_to_commit().unwrap();
    let tree1 = part1_commit.tree().unwrap();

    // Check crypto_test.c content in part 1
    let obj = tree1
        .get_path(std::path::Path::new("auto_tests/crypto_test.c"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let content = std::str::from_utf8(obj.as_blob().unwrap().content()).unwrap();

    // Should NOT contain the include
    assert!(!content.contains("#include \"../other/fun/create_common.h\""));
    assert!(!content.contains("TODO(goldroom)"));

    // Should contain the function
    assert!(content.contains("static void test_noiseik(void)"));

    // Should contain the call
    assert!(content.contains("test_noiseik();"));

    // Check other files are modified
    let obj_c = tree1
        .get_path(std::path::Path::new("toxcore/crypto_core.c"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    assert!(
        std::str::from_utf8(obj_c.as_blob().unwrap().content())
            .unwrap()
            .contains("modified")
    );
}
