const EXPECTED_PIPELINE: [&str; 5] = [
    "ir::from_resolved",
    "closure_capture::materialize_envs",
    "ownership_classify",
    "drop_insert",
    "reuse_pair",
];

pub fn assert_perceus_pipeline_order(pass_names: &[&'static str]) {
    assert_eq!(
        pass_names, EXPECTED_PIPELINE,
        "internal perceus pipeline wiring bug: expected {:?}, got {:?}",
        EXPECTED_PIPELINE, pass_names
    );
}

#[cfg(test)]
mod tests {
    use super::assert_perceus_pipeline_order;

    #[test]
    fn accepts_expected_pipeline_order() {
        assert_perceus_pipeline_order(&[
            "ir::from_resolved",
            "closure_capture::materialize_envs",
            "ownership_classify",
            "drop_insert",
            "reuse_pair",
        ]);
    }

    #[test]
    #[should_panic(expected = "internal perceus pipeline wiring bug")]
    fn rejects_wrong_pipeline_order() {
        assert_perceus_pipeline_order(&[
            "ir::from_resolved",
            "ownership_classify",
            "closure_capture::materialize_envs",
            "drop_insert",
            "reuse_pair",
        ]);
    }
}
