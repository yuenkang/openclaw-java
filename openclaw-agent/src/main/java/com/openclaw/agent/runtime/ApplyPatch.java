package com.openclaw.agent.runtime;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Parses and applies *** Begin Patch / *** End Patch format patches.
 * Mirrors {@code agents/apply-patch.ts}.
 */
public final class ApplyPatch {

    private ApplyPatch() {
    }

    // --- Markers ---
    private static final String BEGIN_PATCH = "*** Begin Patch";
    private static final String END_PATCH = "*** End Patch";
    private static final String ADD_FILE = "*** Add File: ";
    private static final String DELETE_FILE = "*** Delete File: ";
    private static final String UPDATE_FILE = "*** Update File: ";
    private static final String MOVE_TO = "*** Move to: ";
    private static final String EOF_MARKER = "*** End of File";
    private static final String CHANGE_CONTEXT = "@@ ";
    private static final String EMPTY_CHANGE_CONTEXT = "@@";
    private static final Pattern UNICODE_SPACES = Pattern.compile("[\\u00A0\\u2000-\\u200A\\u202F\\u205F\\u3000]");

    // --- Public types ---

    public record ApplyPatchSummary(
            List<String> added,
            List<String> modified,
            List<String> deleted) {
    }

    public record ApplyPatchResult(ApplyPatchSummary summary, String text) {
    }

    public record ApplyPatchOptions(String cwd, String sandboxRoot) {
    }

    // --- Hunk Types ---

    sealed interface Hunk {
    }

    record AddFileHunk(String path, String contents) implements Hunk {
    }

    record DeleteFileHunk(String path) implements Hunk {
    }

    record UpdateFileHunk(String path, String movePath,
            List<ApplyPatchUpdate.UpdateFileChunk> chunks) implements Hunk {
    }

    // --- Public API ---

    /**
     * Parse and apply a patch to the filesystem.
     *
     * @throws IOException              on file I/O failures
     * @throws IllegalArgumentException on invalid patch format
     */
    public static ApplyPatchResult applyPatch(String input, ApplyPatchOptions options)
            throws IOException {
        ParseResult parsed = parsePatchText(input);
        if (parsed.hunks().isEmpty()) {
            throw new IllegalArgumentException("No files were modified.");
        }

        List<String> addedList = new ArrayList<>();
        List<String> modifiedList = new ArrayList<>();
        List<String> deletedList = new ArrayList<>();
        Set<String> addedSeen = new HashSet<>();
        Set<String> modifiedSeen = new HashSet<>();
        Set<String> deletedSeen = new HashSet<>();

        for (Hunk hunk : parsed.hunks()) {
            if (hunk instanceof AddFileHunk add) {
                ResolvedPath target = resolvePatchPath(add.path(), options);
                ensureDir(target.resolved());
                Files.writeString(Path.of(target.resolved()), add.contents());
                record(addedList, addedSeen, target.display());
            } else if (hunk instanceof DeleteFileHunk del) {
                ResolvedPath target = resolvePatchPath(del.path(), options);
                Files.delete(Path.of(target.resolved()));
                record(deletedList, deletedSeen, target.display());
            } else if (hunk instanceof UpdateFileHunk upd) {
                ResolvedPath target = resolvePatchPath(upd.path(), options);
                String applied = ApplyPatchUpdate.applyUpdateHunk(target.resolved(), upd.chunks());
                if (upd.movePath() != null) {
                    ResolvedPath moveTarget = resolvePatchPath(upd.movePath(), options);
                    ensureDir(moveTarget.resolved());
                    Files.writeString(Path.of(moveTarget.resolved()), applied);
                    Files.delete(Path.of(target.resolved()));
                    record(modifiedList, modifiedSeen, moveTarget.display());
                } else {
                    Files.writeString(Path.of(target.resolved()), applied);
                    record(modifiedList, modifiedSeen, target.display());
                }
            }
        }

        ApplyPatchSummary summary = new ApplyPatchSummary(addedList, modifiedList, deletedList);
        return new ApplyPatchResult(summary, formatSummary(summary));
    }

    // --- Internal ---

    private static void record(List<String> list, Set<String> seen, String value) {
        if (seen.add(value)) {
            list.add(value);
        }
    }

    private static String formatSummary(ApplyPatchSummary summary) {
        List<String> lines = new ArrayList<>();
        lines.add("Success. Updated the following files:");
        for (String f : summary.added())
            lines.add("A " + f);
        for (String f : summary.modified())
            lines.add("M " + f);
        for (String f : summary.deleted())
            lines.add("D " + f);
        return String.join("\n", lines);
    }

    private static void ensureDir(String filePath) throws IOException {
        Path parent = Path.of(filePath).getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
    }

    private record ResolvedPath(String resolved, String display) {
    }

    private static ResolvedPath resolvePatchPath(String filePath, ApplyPatchOptions options)
            throws IOException {
        if (options.sandboxRoot() != null) {
            SandboxPaths.SandboxPathResult r = SandboxPaths.assertSandboxPath(filePath, options.cwd(),
                    options.sandboxRoot());
            return new ResolvedPath(r.resolved(),
                    r.relative().isEmpty() ? r.resolved() : r.relative());
        }
        String resolved = resolvePathFromCwd(filePath, options.cwd());
        return new ResolvedPath(resolved, toDisplayPath(resolved, options.cwd()));
    }

    private static String normalizeUnicodeSpaces(String value) {
        return UNICODE_SPACES.matcher(value).replaceAll(" ");
    }

    private static String expandPath(String filePath) {
        String normalized = normalizeUnicodeSpaces(filePath);
        if ("~".equals(normalized))
            return System.getProperty("user.home");
        if (normalized.startsWith("~/"))
            return System.getProperty("user.home") + normalized.substring(1);
        return normalized;
    }

    private static String resolvePathFromCwd(String filePath, String cwd) {
        String expanded = expandPath(filePath);
        Path p = Path.of(expanded);
        if (p.isAbsolute())
            return p.normalize().toString();
        return Path.of(cwd).resolve(expanded).normalize().toString();
    }

    private static String toDisplayPath(String resolved, String cwd) {
        Path rel = Path.of(cwd).relativize(Path.of(resolved));
        String s = rel.toString();
        if (s.isEmpty())
            return Path.of(resolved).getFileName().toString();
        if (s.startsWith("..") || Path.of(s).isAbsolute())
            return resolved;
        return s;
    }

    // --- Parser ---

    private record ParseResult(List<Hunk> hunks) {
    }

    static ParseResult parsePatchText(String input) {
        String trimmed = input.trim();
        if (trimmed.isEmpty()) {
            throw new IllegalArgumentException("Invalid patch: input is empty.");
        }
        String[] rawLines = trimmed.split("\\r?\\n");
        List<String> lines = checkPatchBoundariesLenient(rawLines);
        List<Hunk> hunks = new ArrayList<>();
        int lastLineIndex = lines.size() - 1;
        List<String> remaining = lines.subList(1, lastLineIndex);
        int lineNumber = 2;

        while (!remaining.isEmpty()) {
            HunkParseResult result = parseOneHunk(remaining, lineNumber);
            hunks.add(result.hunk());
            lineNumber += result.consumed();
            remaining = remaining.subList(result.consumed(), remaining.size());
        }
        return new ParseResult(hunks);
    }

    private static List<String> checkPatchBoundariesLenient(String[] lines) {
        String strictError = checkPatchBoundariesStrict(lines);
        if (strictError == null) {
            return List.of(lines);
        }
        if (lines.length < 4) {
            throw new IllegalArgumentException(strictError);
        }
        String first = lines[0];
        String last = lines[lines.length - 1];
        if (("<<EOF".equals(first) || "<<'EOF'".equals(first) || "<<\"EOF\"".equals(first))
                && last.endsWith("EOF")) {
            String[] inner = new String[lines.length - 2];
            System.arraycopy(lines, 1, inner, 0, inner.length);
            String innerError = checkPatchBoundariesStrict(inner);
            if (innerError == null) {
                return List.of(inner);
            }
            throw new IllegalArgumentException(innerError);
        }
        throw new IllegalArgumentException(strictError);
    }

    private static String checkPatchBoundariesStrict(String[] lines) {
        if (lines.length == 0)
            return "The first line of the patch must be '*** Begin Patch'";
        String first = lines[0].trim();
        String last = lines[lines.length - 1].trim();
        if (BEGIN_PATCH.equals(first) && END_PATCH.equals(last))
            return null;
        if (!BEGIN_PATCH.equals(first))
            return "The first line of the patch must be '*** Begin Patch'";
        return "The last line of the patch must be '*** End Patch'";
    }

    private record HunkParseResult(Hunk hunk, int consumed) {
    }

    private static HunkParseResult parseOneHunk(List<String> lines, int lineNumber) {
        if (lines.isEmpty()) {
            throw new IllegalArgumentException(
                    "Invalid patch hunk at line " + lineNumber + ": empty hunk");
        }
        String firstLine = lines.get(0).trim();

        if (firstLine.startsWith(ADD_FILE)) {
            String targetPath = firstLine.substring(ADD_FILE.length());
            StringBuilder contents = new StringBuilder();
            int consumed = 1;
            for (int i = 1; i < lines.size(); i++) {
                String addLine = lines.get(i);
                if (addLine.startsWith("+")) {
                    contents.append(addLine.substring(1)).append('\n');
                    consumed++;
                } else {
                    break;
                }
            }
            return new HunkParseResult(new AddFileHunk(targetPath, contents.toString()), consumed);
        }

        if (firstLine.startsWith(DELETE_FILE)) {
            return new HunkParseResult(
                    new DeleteFileHunk(firstLine.substring(DELETE_FILE.length())), 1);
        }

        if (firstLine.startsWith(UPDATE_FILE)) {
            String targetPath = firstLine.substring(UPDATE_FILE.length());
            List<String> rem = new ArrayList<>(lines.subList(1, lines.size()));
            int consumed = 1;
            String movePath = null;

            if (!rem.isEmpty()) {
                String moveCandidate = rem.get(0).trim();
                if (moveCandidate.startsWith(MOVE_TO)) {
                    movePath = moveCandidate.substring(MOVE_TO.length());
                    rem.remove(0);
                    consumed++;
                }
            }

            List<ApplyPatchUpdate.UpdateFileChunk> chunks = new ArrayList<>();
            while (!rem.isEmpty()) {
                if (rem.get(0).trim().isEmpty()) {
                    rem.remove(0);
                    consumed++;
                    continue;
                }
                if (rem.get(0).startsWith("***"))
                    break;
                ChunkParseResult cr = parseUpdateFileChunk(rem, lineNumber + consumed,
                        chunks.isEmpty());
                chunks.add(cr.chunk());
                for (int i = 0; i < cr.consumed(); i++)
                    rem.remove(0);
                consumed += cr.consumed();
            }

            if (chunks.isEmpty()) {
                throw new IllegalArgumentException(
                        "Invalid patch hunk at line " + lineNumber
                                + ": Update file hunk for path '" + targetPath + "' is empty");
            }
            return new HunkParseResult(
                    new UpdateFileHunk(targetPath, movePath, chunks), consumed);
        }

        throw new IllegalArgumentException(
                "Invalid patch hunk at line " + lineNumber + ": '" + lines.get(0)
                        + "' is not a valid hunk header.");
    }

    private record ChunkParseResult(ApplyPatchUpdate.UpdateFileChunk chunk, int consumed) {
    }

    private static ChunkParseResult parseUpdateFileChunk(
            List<String> lines, int lineNumber, boolean allowMissingContext) {
        if (lines.isEmpty()) {
            throw new IllegalArgumentException(
                    "Invalid patch hunk at line " + lineNumber
                            + ": Update hunk does not contain any lines");
        }

        String changeContext = null;
        int startIndex = 0;

        if (EMPTY_CHANGE_CONTEXT.equals(lines.get(0))) {
            startIndex = 1;
        } else if (lines.get(0).startsWith(CHANGE_CONTEXT)) {
            changeContext = lines.get(0).substring(CHANGE_CONTEXT.length());
            startIndex = 1;
        } else if (!allowMissingContext) {
            throw new IllegalArgumentException(
                    "Invalid patch hunk at line " + lineNumber
                            + ": Expected update hunk to start with a @@ context marker, got: '"
                            + lines.get(0) + "'");
        }

        if (startIndex >= lines.size()) {
            throw new IllegalArgumentException(
                    "Invalid patch hunk at line " + (lineNumber + 1)
                            + ": Update hunk does not contain any lines");
        }

        List<String> oldLines = new ArrayList<>();
        List<String> newLines = new ArrayList<>();
        boolean isEndOfFile = false;
        int parsedLines = 0;

        for (int i = startIndex; i < lines.size(); i++) {
            String line = lines.get(i);
            if (EOF_MARKER.equals(line)) {
                if (parsedLines == 0) {
                    throw new IllegalArgumentException(
                            "Invalid patch hunk at line " + (lineNumber + 1)
                                    + ": Update hunk does not contain any lines");
                }
                isEndOfFile = true;
                parsedLines++;
                break;
            }

            if (line.isEmpty()) {
                oldLines.add("");
                newLines.add("");
                parsedLines++;
                continue;
            }

            char marker = line.charAt(0);
            if (marker == ' ') {
                String content = line.substring(1);
                oldLines.add(content);
                newLines.add(content);
                parsedLines++;
            } else if (marker == '+') {
                newLines.add(line.substring(1));
                parsedLines++;
            } else if (marker == '-') {
                oldLines.add(line.substring(1));
                parsedLines++;
            } else {
                if (parsedLines == 0) {
                    throw new IllegalArgumentException(
                            "Invalid patch hunk at line " + (lineNumber + 1)
                                    + ": Unexpected line found in update hunk: '" + line + "'");
                }
                break;
            }
        }

        return new ChunkParseResult(
                new ApplyPatchUpdate.UpdateFileChunk(changeContext, oldLines, newLines, isEndOfFile),
                parsedLines + startIndex);
    }
}
