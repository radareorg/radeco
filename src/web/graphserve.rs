use std::process::Command;

struct GraphServe {
	cached_graphs: 
}


fn dot(input: &str) -> Vec<u8> {
	let mut child = Command::new("dot").arg("-Tsvg")
		.stdin(Stdio::piped())
		.spawn().unwrap();

	child.stdin.get_mut().write_all(input.as_bytes());
	child.wait_with_output().stdout
}
