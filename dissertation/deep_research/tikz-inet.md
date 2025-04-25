This guide provides an introduction to drawing interaction nets using the `tikz-inet` package in LaTeX, based on the provided documentation[cite: 1].

**1. Setting up Your Document**

To begin, you need to include the `tikz` and `tikz-inet` packages in the preamble of your LaTeX document[cite: 1]. You can also provide options to the `tikz-inet` package when loading it[cite: 4].

```latex
\documentclass{article}
\usepackage{tikz}
\usepackage[<options>]{tikz-inet}

\begin{document}

% Your diagrams will be created within a tikzpicture environment
\begin{tikzpicture}
% Drawing commands go here
\end{tikzpicture}

\end{document}
```

Notable options for the `tikz-inet` package include `fancy` for a more elaborate style and `color=<color>` to set a global color for the fancy style (requires `xcolor`)[cite: 4]. The `angle` option sets the default orientation of cells in degrees[cite: 4, 9].

**2. Drawing Cells**

Cells are drawn using the `\inetcell` macro[cite: 6].

```latex
\inetcell[<tikz display keys>](<node name>){<symbol>}[<angle>]
```

* `<tikz display keys>`: Optional TikZ styling options[cite: 6, 11].
* `<node name>`: An optional name to reference the cell[cite: 7]. If omitted and the symbol is simple, the symbol is used as the name[cite: 8].
* `<symbol>`: The text or symbol inside the cell (mandatory)[cite: 6].
* `<angle>`: The orientation angle in degrees or `U`, `D`, `L`, `R`[cite: 9, 10].

Cells are based on the `isosceles triangle` shape in TikZ[cite: 5].

**3. Cell Ports**

Ports are used to connect cells with wires. `tikz-inet` defines several anchors for accessing these ports[cite: 16]:

* `pal`: The principal port at the apex[cite: 16].
* `middle pax`: An auxiliary port aligned with the principal port[cite: 16].
* `left pax`, `right pax`: Auxiliary ports on the sides opposite the apex[cite: 16].
* `pax n`: Numbered auxiliary ports if the `arity` key is used[cite: 16].

Each anchor also has an `above <anchor>` version, slightly offset for smoother wire connections[cite: 17, 18].

**4. Drawing Wires**

Wires connect cell ports. The `\inetwire` macro draws a wire between two ports[cite: 21].

```latex
\inetwire[<tikz extra display keys>](<cell1.port1>)(<cell2.port2>)
```

* `<tikz extra display keys>`: Optional TikZ styling for the wire[cite: 21].
* `<cell1.port1>`, `<cell2.port2>`: The starting and ending points using the cell's node name and port anchor[cite: 21].

For unconnected ports (free ports), use `\inetwirefree`[cite: 25].

```latex
\inetwirefree[<tikz extra display keys>](<cell.port>)
```

This draws a wire from the specified port to its corresponding `above` anchor[cite: 25].

Wires are drawn on a layer below the cells[cite: 20].

**5. Examples from the Documentation**

Here are some examples directly from the `tikz-inet` documentation:

**Basic Example:**

This example shows simple cells and connections.

```latex
\begin{tikzpicture}
  \matrix [row sep=0.5cm]{
    \inetcell{A} & & \inetcell{B} \\
    & \inetcell{C} & \\
  };
  \inetwirefree (A.middle pax);
  \inetwirefree (B.middle pax);
  \inetwirefree (C.pal);
  \inetwire (A.pal) (C.right pax);
  \inetwire (B.pal) (C.left pax);
  \inetbox{(A) (B)}(b);
\end{tikzpicture}
```

**Style Variations:**

This example demonstrates applying different styles to cells using TikZ options.

```latex
\begin{tikzpicture}
  \matrix{
    \inetcell{A} & \inetcell[fancycellstyle green]{B} \\
    \inetcell[bottom color green]{C} & \inetcell[draw black]{D} \\
    \inetcell[very thick]{E} & \inetnofancy \inetcell{F} \inetfancy \\
  };
\end{tikzpicture}
```
The `\inetnofancy` and `\inetfancy` macros can be used to switch between styles[cite: 27].

**Special Cases:**

The documentation also includes examples of more complex arrangements and the use of `\inetprombox`.

For instance, a circular arrangement of cells can be created using loops and calculated positioning:

```latex
\begin{tikzpicture}
  \newcount\angle
  \foreach \x in {1,...,12} {
    \pgfmathsetcount{\angle}{360*\x/12+90}
    \inetcell [\inetcellstyle green!\x0,
    at=(\the\angle-90:1.5cm)]
    (c\x){A}[\angle]
  }
\end{tikzpicture}
```

This guide covers the fundamental aspects of using the `tikz-inet` package with examples from its documentation. For more detailed information and advanced features, please refer to the official `tikz-inet` documentation[cite: 1].