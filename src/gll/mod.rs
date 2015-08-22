use graph;
use graph::Graph;
use graph::Node as GNode;
use graph::Id;

use std::collections::HashMap;
use std::collections::HashSet;

pub trait Label: Id {

}

pub struct InputIndex(usize);

pub struct NodeData<L> {
    label: L,
    input_index: InputIndex,
}
pub fn NodeData<L>(label: L, input_index: InputIndex) -> NodeData<L> {
    NodeData { label: label, input_index: input_index }
}
pub type Node<'n, L> = graph::Node<'n, (L, InputIndex)>;

// descriptor (L1, u) where L1 is a label and u is a node = L2^j
pub struct Descriptor<'n, L:Label+'n> {
    pub label: L,
    pub node: &'n Node<'n, L>,
}

impl<'n, L:Label+'n> PartialEq for Descriptor<'n, L> {
    fn eq(&self, rhs: &Descriptor<'n, L>) -> bool {
        self.label.id() == rhs.label.id() &&
            self.node.id() == rhs.node.id()
    }
}
impl<'n, L:Label+'n> Eq for Descriptor<'n, L> { }

impl<'n, L:Label+'n> ::std::hash::Hash for Descriptor<'n, L> {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.label.id());
        state.write_usize(self.node.id());
    }

}


pub struct WorkSet<'n, L:Label+'n> {
    elems: Vec<Descriptor<'n, L>>,
}

impl<'n, L:Label+'n> WorkSet<'n, L> {
}

pub struct Context<'n, L:Label+'n> {
    i: usize,
    seen: HashSet<Descriptor<'n, L>>,
    popped: Vec<(&'n Node, usize)>,
    gss: Graph<'n, L>,
    nodes: HashMap<(Action, InputIndex), &'n Node<'n, L>>,
}

struct ActionArgs<'nodes, 'input: 'nodes, I: 'input> {
    i: usize,
    input: &'input [I],
    context: &'nodes mut Context<'nodes, Action<'nodes, 'input, I>>,
}

struct Action<'nodes, 'input: 'nodes, I: 'input>(
    pub fn(ActionArgs<'nodes, 'input, I>) -> Result<'nodes, 'input, I>
        );

impl<'nodes, 'input, I> graph::Id for Action<'nodes, 'input, I> {
    fn id(&self) -> usize {
        self.0 as usize
    }
}

impl<'nodes, 'input, I> Label for Action<'nodes, 'input, I> {

}

enum Result<'nodes, 'input: 'nodes, I: 'input> {
    // Report parse success on this path
    Success,
    // Report parse failure on this path
    Failure,
    // Schedule call to returned procedure
    Goto(Action<'nodes, 'input, I>),
    // Resume core interpretive loop L_0
    ResumeL0,
}

macro_rules! test {
    ($context, $x, $A, $alpha) => {
        { FIRST!($context, $alpha).has($x) ||
          (FIRST!($context, $alpha).has(epsilon) &&
           FOLLOW!($context, $A).has($x)) }
    }
}

impl<'n, L> Context<'n, L> {
    fn add(&mut self, label: L, u: &Node, j: InputIndex) {
        let U_j = self.context.seen[j];
        if ! U_j.has((label, u)) {
            U_j.add((label, u));
            context.queue.add((label, u, j));
        }
    }
    fn pop(&mut self, u: &Node, j: InputIndex) {
        if u != self.u_0 {
            self.popped.add((u, j));
            for v in u.children() {
                self.add(u.data.label, v, j);
            }
        }
    }
    fn create(&mut self, label: L, u: &Node, j: InputIndex) {
        let v = self.nodes.entry((label, j)).or_insert_with(|| {
            self.graph.add_node(NodeData(label, j))
        });
        if !v.children().contains(u) {
            v.add_child(u);
            for (v, k) in self.popped {
                self.add(L, u, k);
            }
        }
        return v;
    }
}

#[test]
fn demo_papers_grammar() {
    // S ::= A S d | B S
    // A ::= a | c
    // B ::= a | b

    struct WorkItem(Action<char>);
    let graph = Graph::new();
    let u_1 = graph.add_node((Result::ResumeL0, 0));
    let u_0 = graph.add_node((Result::Goto(Action(l_dummy)), 0));
    let queue = vec![(l_S, )];
    loop {
        if Some(s) = queue.pop() {
            unimplemented!();
        }
        unimplemented!();
    }
    fn l_dummy<N, L:Label>(a: ActionArgs<Option<char>, N, L>) -> Result<N, L> {
        panic!("should never invoke the dummy node action");
    }
    fn l_S<N, L:Label>(a: ActionArgs<Option<char>, N, L>) -> Result<N, L> {
        let ActionArgs { i, input, context } = a;
        if [Some('a'), Some('c')].contains(input[i]) { add(l_S1, context.c_u, i); }
        if [Some('a'), Some('b')].contains(input[i]) { add(l_S2, context.c_u, i); }
        if [Some('d'), None].contains(input[i]) { add(l_S3, context.c_u, i); }
        Result::Resume
    }
    
}
