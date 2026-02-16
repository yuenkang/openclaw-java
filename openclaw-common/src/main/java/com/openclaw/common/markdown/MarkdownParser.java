package com.openclaw.common.markdown;

import com.openclaw.common.markdown.MarkdownIR.*;
import com.vladsch.flexmark.ast.*;
import com.vladsch.flexmark.ext.gfm.strikethrough.Strikethrough;
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension;
import com.vladsch.flexmark.ext.tables.*;
import com.vladsch.flexmark.parser.Parser;
import com.vladsch.flexmark.util.ast.Node;
import com.vladsch.flexmark.util.data.MutableDataSet;

import java.util.*;

/**
 * Markdown parser backed by
 * <a href="https://github.com/vsch/flexmark-java">flexmark-java</a>.
 * Produces {@link MarkdownIR} by walking the AST — same approach as the TS
 * version
 * (which uses <code>markdown-it</code> tokens).
 *
 * <p>
 * Supported elements: bold, italic, code, code blocks, strikethrough, links,
 * images (alt text), lists (nested), headings, blockquotes, tables (bullets /
 * code modes),
 * and horizontal rules.
 */
public final class MarkdownParser {

    private MarkdownParser() {
    }

    // -----------------------------------------------------------------------
    // Options
    // -----------------------------------------------------------------------

    public enum HeadingStyle {
        NONE, BOLD
    }

    /** How tables are rendered into the plain-text IR. */
    public enum TableMode {
        /** Tables are skipped (default). */
        OFF,
        /** Each row becomes bullet points with header:value pairs. */
        BULLETS,
        /** Table is preserved as padded ASCII table inside a code_block span. */
        CODE
    }

    public record ParseOptions(
            boolean linkify,
            String headingStyle,
            String blockquotePrefix,
            TableMode tableMode,
            boolean enableSpoilers) {
        public static ParseOptions defaults() {
            return new ParseOptions(true, "bold", "│ ", TableMode.OFF, false);
        }

        /** Resolved heading style as enum. */
        HeadingStyle resolvedHeadingStyle() {
            return "bold".equalsIgnoreCase(headingStyle) ? HeadingStyle.BOLD : HeadingStyle.NONE;
        }
    }

    /** Result of {@link #parseWithMeta} — includes the IR and metadata. */
    public record ParseResult(MarkdownIR ir, boolean hasTables) {
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    public static MarkdownIR parse(String markdown) {
        return parse(markdown, ParseOptions.defaults());
    }

    public static MarkdownIR parse(String markdown, ParseOptions options) {
        return parseWithMeta(markdown, options).ir();
    }

    /**
     * Parse markdown and return both the IR and metadata (e.g. whether tables
     * were present). Mirrors TS {@code markdownToIRWithMeta}.
     */
    public static ParseResult parseWithMeta(String markdown, ParseOptions options) {
        if (markdown == null || markdown.isEmpty()) {
            return new ParseResult(MarkdownIR.plainText(""), false);
        }

        MutableDataSet flexOpts = new MutableDataSet();
        List<com.vladsch.flexmark.util.misc.Extension> extensions = new ArrayList<>();
        extensions.add(StrikethroughExtension.create());
        if (options.tableMode() != TableMode.OFF) {
            extensions.add(TablesExtension.create());
        }
        flexOpts.set(Parser.EXTENSIONS, extensions);

        Parser parser = Parser.builder(flexOpts).build();
        Node document = parser.parse(markdown);

        RenderState state = new RenderState(options);
        renderNode(document, state);

        return new ParseResult(state.build(), state.hasTables);
    }

    // -----------------------------------------------------------------------
    // AST walker
    // -----------------------------------------------------------------------

    private static void renderNode(Node node, RenderState state) {
        for (Node child = node.getFirstChild(); child != null; child = child.getNext()) {
            renderSingle(child, state);
        }
    }

    private static void renderSingle(Node node, RenderState state) {
        // ---- Block-level ----
        if (node instanceof Heading heading) {
            boolean bold = state.options.resolvedHeadingStyle() == HeadingStyle.BOLD;
            if (bold)
                state.openStyle(Style.BOLD);
            renderNode(heading, state);
            if (bold)
                state.closeStyle(Style.BOLD);
            state.appendParagraphSeparator();

        } else if (node instanceof Paragraph) {
            renderNode(node, state);
            state.appendParagraphSeparator();

        } else if (node instanceof BlockQuote) {
            state.append(state.options.blockquotePrefix());
            renderNode(node, state);
            state.append("\n");

        } else if (node instanceof BulletList) {
            state.pushList(false);
            renderNode(node, state);
            state.popList();

        } else if (node instanceof OrderedList ol) {
            state.pushList(true);
            renderNode(node, state);
            state.popList();

        } else if (node instanceof BulletListItem || node instanceof OrderedListItem) {
            state.appendListPrefix();
            renderNode(node, state);

        } else if (node instanceof FencedCodeBlock fcb) {
            String code = fcb.getContentChars().toString();
            if (!code.endsWith("\n"))
                code += "\n";
            int start = state.position();
            state.append(code);
            state.addStyle(start, state.position(), Style.CODE_BLOCK);
            if (state.listDepth() == 0)
                state.append("\n");

        } else if (node instanceof IndentedCodeBlock icb) {
            String code = icb.getContentChars().toString();
            if (!code.endsWith("\n"))
                code += "\n";
            int start = state.position();
            state.append(code);
            state.addStyle(start, state.position(), Style.CODE_BLOCK);
            if (state.listDepth() == 0)
                state.append("\n");

        } else if (node instanceof ThematicBreak) {
            state.append("\n");

        } else if (node instanceof HtmlBlock hb) {
            state.append(hb.getChars().toString());

            // ---- Table handling ----
        } else if (node instanceof TableBlock) {
            state.startTable();
            renderNode(node, state);
            state.finishTable();

        } else if (node instanceof TableHead) {
            state.setTableHeader(true);
            renderNode(node, state);
            state.setTableHeader(false);

        } else if (node instanceof TableBody) {
            renderNode(node, state);

        } else if (node instanceof TableRow) {
            state.startTableRow();
            renderNode(node, state);
            state.finishTableRow();

        } else if (node instanceof TableCell) {
            state.startTableCell();
            renderNode(node, state);
            state.finishTableCell();

            // ---- Inline ----
        } else if (node instanceof Text text) {
            handleSpoilerText(text.getChars().toString(), state);

        } else if (node instanceof Code code) {
            int start = state.position();
            state.append(code.getText().toString());
            state.addStyle(start, state.position(), Style.CODE);

        } else if (node instanceof Emphasis) {
            state.openStyle(Style.ITALIC);
            renderNode(node, state);
            state.closeStyle(Style.ITALIC);

        } else if (node instanceof StrongEmphasis) {
            state.openStyle(Style.BOLD);
            renderNode(node, state);
            state.closeStyle(Style.BOLD);

        } else if (node instanceof Strikethrough) {
            state.openStyle(Style.STRIKETHROUGH);
            renderNode(node, state);
            state.closeStyle(Style.STRIKETHROUGH);

        } else if (node instanceof Link link) {
            String href = link.getUrl().toString();
            int start = state.position();
            renderNode(link, state);
            state.addLink(start, state.position(), href);

        } else if (node instanceof AutoLink al) {
            String href = al.getUrl().toString();
            int start = state.position();
            state.append(href);
            state.addLink(start, state.position(), href);

        } else if (node instanceof Image img) {
            // Render alt text only (like TS version)
            state.append(img.getText().toString());

        } else if (node instanceof SoftLineBreak || node instanceof HardLineBreak) {
            state.append("\n");

        } else if (node instanceof HtmlInline hi) {
            state.append(hi.getChars().toString());

        } else {
            // Unknown node — descend into children
            renderNode(node, state);
        }
    }

    // -----------------------------------------------------------------------
    // Spoiler detection (Discord ||spoiler|| syntax)
    // -----------------------------------------------------------------------

    private static void handleSpoilerText(String text, RenderState state) {
        if (!state.options.enableSpoilers() || !text.contains("||")) {
            state.append(text);
            return;
        }
        int index = 0;
        while (index < text.length()) {
            int next = text.indexOf("||", index);
            if (next == -1) {
                state.append(text.substring(index));
                break;
            }
            if (next > index) {
                state.append(text.substring(index, next));
            }
            if (state.spoilerOpen) {
                state.closeStyle(Style.SPOILER);
            } else {
                state.openStyle(Style.SPOILER);
            }
            state.spoilerOpen = !state.spoilerOpen;
            index = next + 2;
        }
    }

    // -----------------------------------------------------------------------
    // Render state — mirrors TS RenderState + RenderTarget
    // -----------------------------------------------------------------------

    private static class RenderState {
        final ParseOptions options;
        private final MarkdownIR.Builder builder = MarkdownIR.builder();
        private final Deque<OpenStyle> openStyles = new ArrayDeque<>();
        private final Deque<ListState> listStack = new ArrayDeque<>();
        boolean spoilerOpen = false;
        boolean hasTables = false;

        // Table state
        private TableState table = null;

        RenderState(ParseOptions options) {
            this.options = options;
        }

        int position() {
            return builder.position();
        }

        void append(String s) {
            builder.append(s);
        }

        void addStyle(int start, int end, Style style) {
            builder.addStyle(start, end, style);
        }

        void addLink(int start, int end, String href) {
            builder.addLink(start, end, href);
        }

        void openStyle(Style style) {
            openStyles.push(new OpenStyle(style, builder.position()));
        }

        void closeStyle(Style style) {
            Iterator<OpenStyle> it = openStyles.iterator();
            while (it.hasNext()) {
                OpenStyle os = it.next();
                if (os.style == style) {
                    int end = builder.position();
                    if (end > os.start) {
                        builder.addStyle(os.start, end, style);
                    }
                    it.remove();
                    return;
                }
            }
        }

        void appendParagraphSeparator() {
            if (!listStack.isEmpty())
                return;
            builder.append("\n\n");
        }

        void pushList(boolean ordered) {
            listStack.push(new ListState(ordered, 0));
        }

        void popList() {
            if (!listStack.isEmpty())
                listStack.pop();
        }

        int listDepth() {
            return listStack.size();
        }

        void appendListPrefix() {
            if (listStack.isEmpty())
                return;
            ListState top = listStack.peek();
            top.index++;
            String indent = "  ".repeat(Math.max(0, listStack.size() - 1));
            String prefix = top.ordered ? (top.index + ". ") : "• ";
            builder.append(indent + prefix);
        }

        // -- Table helpers --
        void startTable() {
            hasTables = true;
            if (options.tableMode() != TableMode.OFF) {
                table = new TableState();
            }
        }

        void setTableHeader(boolean inHeader) {
            if (table != null)
                table.inHeader = inHeader;
        }

        void startTableRow() {
            if (table != null)
                table.currentRow = new ArrayList<>();
        }

        void finishTableRow() {
            if (table == null || table.currentRow == null)
                return;
            if (table.inHeader) {
                table.headers = table.currentRow;
            } else {
                table.rows.add(table.currentRow);
            }
            table.currentRow = null;
        }

        void startTableCell() {
            if (table != null) {
                table.savedBuilder = this.builder;
                table.cellBuilder = MarkdownIR.builder();
            }
        }

        void finishTableCell() {
            if (table == null || table.cellBuilder == null)
                return;
            MarkdownIR cellIR = table.cellBuilder.build();
            table.currentRow.add(cellIR);
            table.cellBuilder = null;
        }

        void finishTable() {
            if (table == null)
                return;
            if (options.tableMode() == TableMode.BULLETS) {
                renderTableAsBullets(table);
            } else if (options.tableMode() == TableMode.CODE) {
                renderTableAsCode(table);
            }
            table = null;
        }

        private void renderTableAsBullets(TableState t) {
            boolean useFirstCol = t.headers.size() > 1 && !t.rows.isEmpty();
            for (List<MarkdownIR> row : t.rows) {
                if (row.isEmpty())
                    continue;
                if (useFirstCol && !row.get(0).text().isBlank()) {
                    int labelStart = position();
                    append(row.get(0).text());
                    addStyle(labelStart, position(), Style.BOLD);
                    append("\n");
                }
                int startCol = useFirstCol ? 1 : 0;
                for (int i = startCol; i < row.size(); i++) {
                    append("• ");
                    if (i < t.headers.size() && !t.headers.get(i).text().isBlank()) {
                        append(t.headers.get(i).text());
                        append(": ");
                    }
                    append(row.get(i).text());
                    append("\n");
                }
                append("\n");
            }
        }

        private void renderTableAsCode(TableState t) {
            int cols = t.headers.size();
            for (List<MarkdownIR> row : t.rows)
                cols = Math.max(cols, row.size());
            if (cols == 0)
                return;

            int[] widths = new int[cols];
            for (int i = 0; i < t.headers.size(); i++)
                widths[i] = Math.max(widths[i], t.headers.get(i).text().length());
            for (List<MarkdownIR> row : t.rows)
                for (int i = 0; i < row.size(); i++)
                    widths[i] = Math.max(widths[i], row.get(i).text().length());

            int codeStart = position();
            appendTableRow(t.headers, widths, cols);
            append("|");
            for (int i = 0; i < cols; i++) {
                append(" " + "-".repeat(Math.max(3, widths[i])) + " |");
            }
            append("\n");
            for (List<MarkdownIR> row : t.rows)
                appendTableRow(row, widths, cols);
            addStyle(codeStart, position(), Style.CODE_BLOCK);
            if (listDepth() == 0)
                append("\n");
        }

        private void appendTableRow(List<MarkdownIR> cells, int[] widths, int cols) {
            append("|");
            for (int i = 0; i < cols; i++) {
                append(" ");
                String text = i < cells.size() ? cells.get(i).text() : "";
                append(text);
                int pad = widths[i] - text.length();
                if (pad > 0)
                    append(" ".repeat(pad));
                append(" |");
            }
            append("\n");
        }

        MarkdownIR build() {
            // Close any remaining open styles
            while (!openStyles.isEmpty()) {
                OpenStyle os = openStyles.pop();
                int end = builder.position();
                if (end > os.start) {
                    builder.addStyle(os.start, end, os.style);
                }
            }
            MarkdownIR raw = builder.build();

            // Trim trailing whitespace — mirrors TS: state.text.trimEnd()
            String trimmed = stripTrailing(raw.text());
            int trimmedLen = trimmed.length();

            // But keep at least up to the end of the last code_block span
            int codeBlockEnd = 0;
            for (var span : raw.styles()) {
                if (span.style() == Style.CODE_BLOCK && span.end() > codeBlockEnd) {
                    codeBlockEnd = span.end();
                }
            }
            int finalLen = Math.max(trimmedLen, codeBlockEnd);
            if (finalLen >= raw.text().length()) {
                return raw;
            }
            String finalText = raw.text().substring(0, finalLen);

            // Clamp spans
            List<StyleSpan> clampedStyles = new ArrayList<>();
            for (var s : raw.styles()) {
                int cs = Math.min(s.start(), finalLen);
                int ce = Math.min(s.end(), finalLen);
                if (ce > cs) {
                    clampedStyles.add(new StyleSpan(cs, ce, s.style()));
                }
            }
            List<LinkSpan> clampedLinks = new ArrayList<>();
            for (var l : raw.links()) {
                int cs = Math.min(l.start(), finalLen);
                int ce = Math.min(l.end(), finalLen);
                if (ce > cs) {
                    clampedLinks.add(new LinkSpan(cs, ce, l.href()));
                }
            }
            return new MarkdownIR(finalText, clampedStyles, clampedLinks);
        }

        /** Java 11+ stripTrailing equivalent */
        private static String stripTrailing(String s) {
            int end = s.length();
            while (end > 0 && Character.isWhitespace(s.charAt(end - 1))) {
                end--;
            }
            return s.substring(0, end);
        }
    }

    private record OpenStyle(Style style, int start) {
    }

    private static class ListState {
        final boolean ordered;
        int index;

        ListState(boolean ordered, int index) {
            this.ordered = ordered;
            this.index = index;
        }
    }

    private static class TableState {
        List<MarkdownIR> headers = new ArrayList<>();
        List<List<MarkdownIR>> rows = new ArrayList<>();
        List<MarkdownIR> currentRow;
        MarkdownIR.Builder cellBuilder;
        MarkdownIR.Builder savedBuilder;
        boolean inHeader = false;
    }
}
