## SSA Construction

SSA Contruction is based on the algorithm taken from: "Simple and Efficient
Construction of Static Single Assignment Form".

```none
ssa_construct():
	while there exists an instruction:
		if the current instruction is not a jump:
			append_instruction(current_block, current_instruction)
		else:
			if the target of jump is discovered:
				append_instruction(current_block, current_instruction)
				true_block = split_block(target)
				add_edge(current_block, true_block, True)
				false_block = new_block()
				add_edge(current_block, false_block, False)
				current_block =  false_block
			else:
				append_instruction(current_block, current_instruction)
				true_block = new_block()
				add_edge(current_block, true_block, True)
				false_block = new_block()
				add_edge(current_block, false_block, False)
				current_block = false_block
	
	for block in blocks taken in reverse order:
		seal_block(block)

new_block():
	# Implementation dependant. Trivial.

split_block(at):
	for every def between current_instruction and at:
		if def is defined between at and the previous leader, let this be def_:
			replace_by_phi(def_, def)
	mark at as a leader
	add an unconditional edge from previous instruction to the new_block

# This results in a node that is a duplicate of def (call it new_node) and def
# is transformed into phi(new_node, def_)
replace_by_phi(def, def_):
	re-route operands of def to another node and copy the instruction
	add edge from this new_node to def
	redefine def to be a 'phi' node
	add an edge from def_ to def

append_instruction(block, instruction):
	operands = instruction.operands()
	for every operand, get its reaching definition using read_variable(operand)
	write_variable(block, instruction.destination, instruction)

add_edge(block, other_block, edge_type):
	# Implentation dependant. Trivial Logic.

seal_block(block):
	# TODO: Refer paper and add here

read_variable(variable, block):
	if current_def[variable] contains block:
		return current_def[variable][block]
	return read_variable_recursive(variable, block)

read_variable_recursive(variable, block):
	if block not in sealed_blocks:
		val = new_phi(block)
		incomplete_phis[block][variable]
	else if block.preds.len() == 1:
		val = read_variable(variable, block.preds[0])
	else:
		val = new_phi(block)
		write_variable(variable, block, val)
		val = add_phi_operands(variable, val)
	write_variable(variable, block, val)
	return val

add_phi_operands(variable, phi):
	for pred in phi.block.preds():
		phi.append_operand(read_variable(variable, pred))
	return try_remove_trivial_phi(phi)

try_remove_trivial_phi(phi):
	# Same as the one described in the paper

write_variable():
	# TODO.
```

caveats:
- split_block is expected to be called quite frequently and hence must be
  efficient. A good way to do this is to not have an explicit assiciation from
  an instruction to a block.
- While sealing we can create this explicit relation as now everything we have
  is concrete and no more blocks will be added.
