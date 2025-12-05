import dagre from "@dagrejs/dagre";
import { type LayoutAlgorithm } from ".";

const dagreGraph = new dagre.graphlib.Graph().setDefaultEdgeLabel(() => ({}));

const dagreLayout: LayoutAlgorithm = async (nodes, edges, options) => {
  dagreGraph.setGraph({
    rankdir: options.direction,
    nodesep: options.spacing[0],
    ranksep: options.spacing[1],
  });

  // Remove nodes from dagre graph that are not in the react flow state anymore
  const existingNodeIds = nodes.map((node) => node.id);

  dagreGraph.nodes().forEach((node) => {
    if (!existingNodeIds.includes(node)) {
      dagreGraph.removeNode(node);
    }
  });

  for (const node of nodes) {
    dagreGraph.setNode(node.id, {
      width: node.measured?.width ?? 0,
      height: node.measured?.height ?? 0,
    });
  }

  for (const edge of edges) {
    dagreGraph.setEdge(edge.source, edge.target);
  }

  dagre.layout(dagreGraph);

  const nextNodes = nodes.map((node) => {
    const { x, y } = dagreGraph.node(node.id);
    const position = {
      x: x - (node.measured?.width ?? 0) / 2,
      y: y - (node.measured?.height ?? 0) / 2,
    };

    return { ...node, position };
  });

  return { nodes: nextNodes, edges };
};

export default dagreLayout;
