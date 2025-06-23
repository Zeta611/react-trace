import { parser } from "./parser.js";
import { indentNodeProp } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { LRLanguage } from "@codemirror/language";
import { completeFromList } from "@codemirror/autocomplete";
import { LanguageSupport } from "@codemirror/language";

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      Unit: t.unit,
      Bool: t.bool,
      Int: t.integer,
      String: t.string,
      Id: t.variableName,
      LineComment: t.lineComment,
      "let in useState useEffect": t.controlKeyword,
      "if then else": t.controlKeyword,
      "view not": t.operatorKeyword,
      "+ - '*'": t.arithmeticOperator,
      "= < > <= >=": t.compareOperator,
      "&& ||": t.logicOperator,
      ";": t.controlOperator,
      "( )": t.paren,
      "[ ]": t.squareBracket,
      ", ;;": t.separator,
    }),
    indentNodeProp.add({
      Fun: (context) => context.baseIndentFor(context.node) + context.unit,
      Let: (context) => context.baseIndentFor(context.node),
      Stt: (context) => context.baseIndentFor(context.node),
      Eff: (context) => context.baseIndentFor(context.node) + context.unit,
      View: (context) => context.baseIndentFor(context.node) + context.unit,
      Cond: (context) => context.baseIndentFor(context.node) + context.unit,
      Seq: (context) => context.baseIndentFor(context.node),
      Uop: (context) => context.baseIndentFor(context.node) + context.unit,
      Bop: (context) => context.baseIndentFor(context.node) + context.unit,
      ParenExpr: (context) =>
        context.baseIndentFor(context.node) + context.unit,
    }),
  ],
});

const coreLanguage = LRLanguage.define({
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: "#" },
  },
});

const completion = coreLanguage.data.of({
  autocomplete: completeFromList([
    { label: "useState", type: "function" },
    { label: "useEffect", type: "function" },
    { label: "fun", type: "keyword" },
    { label: "let", type: "keyword" },
    { label: "in", type: "keyword" },
    { label: "view", type: "keyword" },
    { label: "if", type: "keyword" },
    { label: "then", type: "keyword" },
    { label: "else", type: "keyword" },
    { label: "not", type: "keyword" },
    { label: "true", type: "constant" },
    { label: "false", type: "constant" },
  ]),
});

export function core() {
  return new LanguageSupport(coreLanguage, [completion]);
}
