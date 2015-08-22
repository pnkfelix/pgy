use arena::{ArenaMut};
use arena::{ArenaVex, ArenaVexIter};

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

    pub fn add_node(&'n self, t: T, child: &'n Node<'n, T>) -> &Node<'n, T> {
        let r = self.make_data(t, child);
        self.include_node(r);
        r
    }

    pub fn add_dummy(&'n self) -> &Node<'n, T> {
        let r = self.make_dummy();
        self.include_node(r);
        r
    }

    pub fn include_node(&self, r: &'n Node<'n, T>) {
        self.nodes.push(r);
    }

    /// Primitive constructor; only way to get exclusive
    /// access to a Node::Data after allocating it.
    ///
    /// Note that you must later call the `fn include_node`
    /// method if you want the allocated node to actually
    /// be treated as part of the graph.
    ///
    /// In practice, most clients should instead call `fn add_node`
    /// method.
    pub fn make_data(&'n self,
                     t: T,
                     child: &'n Node<'n, T>) -> &'n mut Node<'n, T> {
        self.arena.alloc(Node::Data(DataNode { child: child, data: t }))
    }

    pub fn make_dummy(&'n self) -> &Node<'n, T> {
        self.arena.alloc(Node::Dummy)
    }
}

pub enum Node<'a, T: 'a> {
    Dummy,
    Data(DataNode<'a, T>),
}

pub struct DataNode<'a, T: 'a> {
    child: &'a Node<'a, T>,
    pub data: T,
}

impl<'a, T:'a> DataNode<'a, T> {
    pub fn child(&self) -> &'a Node<'a, T> { self.child }
}

impl<'a, T:'a> Node<'a, T> {
    pub fn data(&'a self) -> Option<&'a T> {
        match *self {
            Node::Dummy => None,
            Node::Data(ref dn) => Some(&dn.data),
        }
    }
    pub fn child(&'a self) -> Option<&'a Node<'a, T>> {
        match *self {
            Node::Dummy => None,
            Node::Data(ref dn) => Some(&dn.child),
        }
    }
}


