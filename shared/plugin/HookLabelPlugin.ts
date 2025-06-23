import {
  Decoration,
  DecorationSet,
  EditorView,
  ViewPlugin,
  ViewUpdate,
  WidgetType,
} from "@codemirror/view";
import { Range } from "@codemirror/state";
import { syntaxTree } from "@codemirror/language";

class HookLabelWidget extends WidgetType {
  constructor(readonly label: number) {
    super();
  }

  eq(other: HookLabelWidget) {
    return other.label == this.label;
  }

  toDOM() {
    const elem = document.createElement("sup");
    elem.textContent = `${this.label}`;
    return elem;
  }
}

function hookLabels(view: EditorView) {
  const widgets: Range<Decoration>[] = [];
  let cnt = 0;

  // const cursor = syntaxTree(view.state).cursor();
  // do {
  //   console.log(`Node ${cursor.name} from ${cursor.from} to ${cursor.to}`);
  // } while (cursor.next());

  for (const { from, to } of view.visibleRanges) {
    syntaxTree(view.state).iterate({
      from,
      to,
      enter: (node) => {
        if (node.name == "Comp") {
          // Reset the counter when we enter a new component
          cnt = 0;
        } else if (["useState", "useEffect"].includes(node.name)) {
          const deco = Decoration.widget({
            widget: new HookLabelWidget(cnt++),
            side: 1,
          });
          widgets.push(deco.range(node.to));
        }
      },
    });
  }
  return Decoration.set(widgets);
}

const hookLabelPlugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;

    constructor(view: EditorView) {
      this.decorations = hookLabels(view);
    }

    update(update: ViewUpdate) {
      if (
        update.docChanged ||
        update.viewportChanged ||
        syntaxTree(update.startState) != syntaxTree(update.state)
      ) {
        this.decorations = hookLabels(update.view);
      }
    }
  },
  {
    decorations: (v) => v.decorations,
  },
);

export { hookLabelPlugin as plugin };
