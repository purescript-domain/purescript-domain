import { marked } from "marked";

export function parseMarkdown(md) {
  return function() {
    return marked.parse(md);
  };
}

export function setInnerHTML(el) {
  return function(html) {
    return function() {
      el.innerHTML = html;
    };
  };
}