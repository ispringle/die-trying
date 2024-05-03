/**
 * Copyright 2022 David Chester david@chester.cx
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 **/

export function html(s, ...e) {
  self.$e ||= (n, e, c = e.target) => {
    while (!c.tagName.match(/-/)) c = c.parentNode;
    c[e.target.getAttribute(n)]?.call(c, e);
  };
  return new String(
    s
      .reduce(
        (a, v, i) =>
          (a +=
            v +
            [e[i]]
              .flat()
              .map((x) =>
                x instanceof String
                  ? x
                  : x === 0
                    ? x
                    : String(x || "").replace(
                        /[<>'"]/g,
                        (c) => `&#${c.charCodeAt(0)}`,
                      ),
              ).join``),
        "",
      )
      .replace(
        /( (@(\w+))=["'])/g,
        (_, v, m, n) =>
          document.body.setAttribute(`on${n}`, `$e('${m}',event)`) || v,
      ),
  );
}
