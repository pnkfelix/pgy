use arena::{ArenaMut};
use arena::{ArenaVex, ArenaVexIter};

pub mod gss;

// Values carrying some state uniquely identifying them relative to
// their type.
pub trait Id {
    fn id(&self) -> usize;
}

impl<'a, T> Id for &'a Node<'a, T> {
    fn id(&self) -> usize {
        *self as *const Node<'a, T> as usize
    }
}

pub struct Graph<'nodes, T: 'nodes> {
    /// the actual backing store of nodes
    arena: ArenaMut<Node<'nodes, T>>,

    /// the nodes of this graph
    nodes: ArenaVex<&'nodes Node<'nodes, T>>,
}

pub type Nodes<'a, T> = ArenaVexIter<'a, &'a Node<'a, T>>;

impl<'n, T> Graph<'n, T> {
    pub fn new() -> Self {
        let arena = ArenaMut::new();
        Graph { arena: arena, nodes: ArenaVex::new() }
    }

    pub fn nodes(&'n self) -> Nodes<'n, T> {
        self.nodes.iter()
    }

    pub fn add_node(&'n self, t: T) -> &Node<'n, T> {
        let r = self.make_node(t);
        self.include_node(r);
        r
    }

    pub fn include_node(&self, r: &'n Node<'n, T>) {
        self.nodes.push(r);
    }

    /// Most primitive constructor; only way to get exclusive
    /// access to the Node after allocating it.
    ///
    /// Note that you must later call the `fn include_node`
    /// method if you want the allocated node to actually
    /// be treated as part of the graph.
    ///
    /// In practice, most clients should instead call `fn add_node`
    /// method.
    pub fn make_node(&'n self, t: T) -> &'n mut Node<'n, T> {
        self.arena.alloc(Node::new(t))
    }
}

pub struct Node<'a, T: 'a> {
    children: ArenaVex<&'a Node<'a, T>>,
    pub data: T,
}

pub type Children<'a, T> = ArenaVexIter<'a, &'a Node<'a, T>>;

impl<'a, T> Node<'a, T> {
    fn new(t: T) -> Node<'a, T> {
        Node { children: ArenaVex::new(), data: t }
    }
    pub fn children(&'a self) -> Children<'a, T> {
        self.children.iter()
    }
    pub fn add_child(&'a self, child: &'a Node<'a, T>) {
        self.children.push(child);
    }
}

#[test]
fn demo_link() {
    // n1 -> n2
    let (n1, n2) = (Node::new(1), Node::new(2));
    n1.add_child(&n2);
}

#[test]
fn demo_arena() {
    // n1 -> n2 -> n3
    let g = Graph::new();
    let n1 = g.add_node(1);
    let n2 = g.add_node(2);
    let n3 = g.add_node(3);
    n2.add_child(n3);
    n1.add_child(n2);

    // n1 -> n2 -> n4
    // n1 -> n3 -> n4
    let g = Graph::new();
    let n1 = g.add_node(1);
    let n2 = g.add_node(2);
    let n3 = g.add_node(3);
    let n4 = g.add_node(4);
    n1.add_child(n2);
    n2.add_child(n4);
    n1.add_child(n3);
    n3.add_child(n4);
}

#[test]
fn demo_gss() {
    let g = Graph::new();
    let l_0_0 = g.add_node("L_0:0");
    let l_1_0 = g.add_node("L_1:0");
    let l_3_0 = g.add_node("L_3:0");
    let l_2_1 = g.add_node("L_2:1");
    let l_4_1 = g.add_node("L_4:1");
    let l_1_1 = g.add_node("L_1:1");
    let l_3_1 = g.add_node("L_3:1");
    let l_2_2 = g.add_node("L_2:2");
    let l_4_2 = g.add_node("L_4:2");

    l_4_2.add_child(l_2_1);
    l_4_2.add_child(l_4_1);
    l_2_2.add_child(l_2_1);
    l_2_2.add_child(l_4_1);
    l_3_1.add_child(l_4_1);
    l_3_1.add_child(l_2_1);
    l_1_1.add_child(l_4_1);
    l_1_1.add_child(l_2_1);
    l_4_1.add_child(l_0_0);
    l_2_1.add_child(l_0_0);
    l_3_0.add_child(l_0_0);
    l_1_0.add_child(l_0_0);
}
