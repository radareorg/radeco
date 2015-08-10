var Application = function() {
	this.component_types = {};
	this.named_objects = {};
};

Application.prototype.register_component_constructor = function(component_type, constructor) {
	this.component_types[component_type] = constructor;
};

Application.prototype.instanciate = function(description) {
	var name = description[0];
	var component = description[1];
	var payload = description[2];

	var constructor = this.component_types[component_type];

	if (constructor !== undefined)
		constructor = component_not_found_warning;

	var api = constructor(payload[2]);

	if (name !== null)
		this.named_objects[name] = api;
};

var GraphVizView = function(embed) {
	var doc = embed.getSVGDocument();
	var extracted_svg = doc.importNode(doc.children[0], true);
	embed.parentNode.replaceChild(extracted_svg, embed);
	extracted_svg.addEventListener("click", this.handle_click.bind(this));
};

GraphVizView.prototype.handle_click = function(event) {
	var name = null;
	for (var i in event.path) {
		var e = event.path[i];
		var ts = e.getElementsByTagName("title");
		if (ts.length > 0) {
			name = ts[0].textContent;
			break;
		}
	}
	console.log("You clicked", name);
};
