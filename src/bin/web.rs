extern crate radeco_lib;
extern crate iron;
extern crate mount;
extern crate staticfile;

use radeco_lib::web::Workspace;
use radeco_lib::frontend::r2::R2;
use radeco_lib::pipeline::Pipeline;

use iron::Iron;
use mount::Mount;
use staticfile::Static;

fn main() {
	let mut workspace = Workspace::new();

	workspace.expose::<R2>("R2");
	workspace.expose::<Pipeline>("Pipeline");

	workspace.add("r2", R2::new("-"));
	workspace.add("pipeline1", Pipeline::new());

	let mut mount = Mount::new();
	mount.mount("/api", workspace);
	mount.mount("/", Static::new("./web/"));

	let serve_on = "localhost:8320";
	println!("Serving on {}", serve_on);
	Iron::new(mount).http(serve_on).unwrap();
}
