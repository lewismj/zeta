# Hold'em UI Production Plan

This plan upgrades `zeta/ui/holdem` from the current prototype into a
commercial-quality GTO Wizard-like desktop solver UI that exposes Zeta's
multiway solver capabilities instead of hiding them behind raw JSON.

The delivery is staged, but every stage ships usable functionality. JSON remains
available as an advanced view, not the main product workflow.

## Current Prototype Review

The current UI is a useful proof of concept, but it is not yet a production
solver workbench.

- `zeta/ui/holdem/src/main_window.cpp` owns almost all UI construction,
  styling, document wiring, and display logic in one file. This makes it hard to
  add real application surfaces without growing a fragile monolith.
- The main surface is still a raw JSON editor with a basic 13x13 grid, a static
  table summary, and a simple hand table. That is appropriate for validation,
  but not for commercial spot building or analysis.
- `solve_active_document()` does not run the solver. It transitions directly to
  `completed` and only reports that the spot is ready for solver execution.
- The prototype displays artifacts if present, but only as aggregated action
  buttons, per-hand best action text, and EV rows. There is no action-tree
  browser, node drilldown, hand detail inspector, or multi-seat comparison.
- `spot_document.cpp` still parses the UI document envelope with manual scanning
  and regex. The referenced `boost_json_plan.md` correctly identifies this
  category of JSON handling as a risk.
- Theme support is a single hard-coded dark stylesheet. There are no theme
  tokens, light/dark variants, density settings, persisted preferences, or visual
  regression checks.
- `solver_state_machine` is a good start for command enablement, but it is not
  connected to a real asynchronous solver session, progress stream,
  cancellation path, or solve history.
- Multiway fields exist in the document model and CLI boundary: 2 to 6 players,
  per-seat ranges, stacks, contributions, `root_actor`, `hero_seat`, street,
  board, rake, `bet_fraction`, `max_history`, `public_state_id`, and
  `samples_per_combo`. The UI should make these first-class controls.

## Product Target

The target is a dense, professional solver workbench:

- Fast spot setup for 2 to 6 players.
- Visual table state with positions, stacks, commitments, pot, board, root actor,
  and hero seat.
- Rich range editing and hand-matrix inspection.
- Real solver execution inside the app.
- Strategy, frequency, and EV views that can explain a multiway root decision.
- Solve history, metadata, import/export, and repeatable documents.
- Themeable commercial UI with dark, light, and high-contrast themes.
- A raw JSON view for power users and debugging, kept synchronized with the
  structured UI.

This should feel like a serious analysis tool, not a demo. The first screen
should be the workbench itself.

## Design Principles

- Structured UI is the source of day-to-day interaction. JSON is an expert tab.
- Every control edits real solver input or real persisted UI state.
- Every visualization is backed by current solver data or clearly unavailable.
- Multiway must not degrade into heads-up labels. Seat count, acting player,
  hero seat, and per-player ranges are always visible.
- Stages may limit scope, but they must not ship fake actions, dead controls, or
  placeholder panels.
- Keep UI state separate from solver/domain state. Views should render view
  models, not mutate solver structs directly.
- Prefer reusable widgets and theme tokens over ad hoc stylesheet fragments.
- Persist user choices such as theme, density, recent files, splitter sizes, and
  default solver settings.

## Proposed Architecture

Add these UI layers under `zeta/ui/holdem/src`:

```text
app/
  app_settings.*
  main_window.*
  command_registry.*
document/
  spot_document.*
  document_json.*
  document_store.*
solver/
  solver_session.*
  solver_worker.*
  solver_profile.*
theme/
  theme.*
  theme_registry.*
  theme_styles.*
widgets/
  card_picker.*
  board_editor.*
  seat_table_editor.*
  range_matrix.*
  range_text_editor.*
  strategy_matrix.*
  action_bar.*
  solve_console.*
  table_state_view.*
  hand_detail_panel.*
  artifact_summary.*
viewmodels/
  spot_view_model.*
  strategy_view_model.*
  solve_history_model.*
```

The immediate goal is not to create many files for its own sake. The goal is to
stop `main_window.cpp` from becoming the application and to give solver,
document, theme, and view code clean ownership boundaries.

## Dependency On Boost.JSON Plan

`doc/holdem/boost_json_plan.md` should be treated as a prerequisite for
production document handling.

Required UI follow-up:

- Move the UI document envelope parser and serializer in `spot_document.cpp` to
  Boost.JSON as well.
- Keep `spot_document` parsing compatible with legacy bare spot JSON and the
  richer UI document envelope.
- Preserve support for optional `artifact: null`.
- Add tests for escaped strings, reordered fields, metadata tags, solve history,
  nested artifact objects, missing spot object, and invalid history entries.

Acceptance: no regex or manual object scanning remains in UI document JSON
handling.

## Stage 1 - Real Solver Workbench Baseline

Goal: turn the prototype into a real local solver app while keeping the UI
surface small.

Deliverables:

- Replace the stubbed `solve_active_document()` path with a real asynchronous
  solver session that calls `cli::solve_spot`.
- Add a `solver_session` abstraction with:
  - input spot snapshot
  - iteration count
  - runtime metadata
  - terminal state: completed, failed, cancelled-before-start
  - timing output from `solve_output`
  - produced `solve_artifact`
- Run solving off the UI thread using `QThread`, `QtConcurrent`, or a dedicated
  worker object.
- Keep the app responsive during a solve. Disable conflicting document edits for
  the active run or require an explicit solve snapshot.
- Store the produced artifact in the active `spot_document`.
- Append a real history entry on completion or failure.
- Show a solve console with start time, settings, graph build time, CFR time,
  extraction time, final status, and error message when applicable.
- Add a solver settings strip with iteration count and output behavior.
- Keep the current JSON editor and 13x13 view, but make the solve button perform
  real work for 2 to 6 players.

Acceptance:

- A valid heads-up spot solves from the UI and renders the artifact.
- A valid 3-way or larger spot solves from the UI and renders the artifact.
- Invalid spots show the parser or validation error without starting a worker.
- The UI remains interactive while solving.
- Closing a window with an active solve prompts the user.
- Tests cover solver state transitions, completed solve persistence, failed
  solve reporting, and document dirty-state behavior after artifact replacement.

## Stage 2 - Production Shell And Theme System

Goal: establish the durable application frame and visual system before expanding
the feature surface.

Deliverables:

- Replace the hard-coded stylesheet with a theme registry:
  - Dark Pro
  - Light Pro
  - High Contrast
- Define theme tokens for:
  - background layers
  - borders
  - text levels
  - accent colors
  - action colors
  - EV positive/negative/neutral colors
  - range weight heat colors
  - warning/error/success states
- Add a theme menu and settings persistence through `QSettings`.
- Add density modes:
  - Compact
  - Comfortable
- Create a professional app shell:
  - top command bar for New, Open, Save, Validate, Solve, Cancel, Theme
  - left document/session rail
  - central analysis workspace
  - right inspector
  - bottom solve console
- Add icons for common commands through Qt theme icons or bundled SVG assets.
- Persist window geometry, splitter sizes, active theme, density, and recent
  files.
- Ensure all text, buttons, tabs, tables, and matrix cells are readable in all
  themes.

Acceptance:

- Theme changes apply live without restarting the app.
- Theme and density persist across app launches.
- No widget relies on a one-off color outside the theme token layer except
  deliberate chart/heatmap scales.
- Existing validation, save, open, and solve workflows still work after the shell
  refactor.
- Add a lightweight screenshot or widget-render smoke check for each theme.

## Stage 3 - Multiway Spot Builder

Goal: make solver input creation fast and safe without requiring users to write
JSON.

Deliverables:

- Add a structured spot editor with:
  - street selector: flop, turn, river
  - board card picker with duplicate-card prevention
  - player count selector from 2 to 6
  - editable seat labels
  - per-seat stack and contribution fields
  - root actor selector
  - hero seat selector
  - gross pot and rake fields
  - bet fraction controls
  - max history
  - public state id
  - samples per combo
- Add a table-state visualizer:
  - seats arranged around a table
  - active actor highlighted
  - hero seat highlighted
  - stack and committed amount per player
  - central board and pot summary
- Keep structured fields and the raw JSON tab synchronized.
- Add inline validation with field-level errors where possible.
- Add template actions:
  - New heads-up river
  - New 3-way flop
  - New 4-way turn
  - Duplicate current spot
- Add a spot summary header that shows player count, street, board, pot, root
  actor, hero, and solve status.

Acceptance:

- Users can create a valid 2, 3, 4, 5, or 6 player spot entirely through the
  structured UI.
- Changing player count resizes ranges, stacks, and contributions safely.
- Invalid card duplication, missing board cards, invalid hero/root actor, and
  mismatched array sizes are blocked or reported inline.
- JSON edits continue to update the structured view after validation.
- Tests cover player-count resizing, board validation, root/hero selection, and
  structured-to-JSON roundtrip.

## Stage 4 - Range Authoring And Combo Inspection

Goal: make range entry usable for real study instead of single-cell toggling.

Deliverables:

- Add a per-seat range editor with seat tabs or a seat selector.
- Support PokerStove syntax through the existing range parser.
- Show parse errors with position and message.
- Add a 13x13 matrix that supports:
  - click/drag selection
  - class selection such as pairs, suited, offsuit, broadways
  - clear seat range
  - copy range
  - paste range
  - normalize display
  - exact combo list view
- Show live range metrics:
  - combos before board blockers
  - live combos after board blockers
  - percent of total hands
  - blocked combos by board card
- Add board-aware disabled combo indicators.
- Add range weight visualization for weighted parser terms.
- Add import/export of seat ranges as text.

Acceptance:

- Users can author every seat range for a 2 to 6 player spot without opening raw
  JSON.
- Weighted ranges roundtrip through parser and serializer without losing
  intended weights.
- Board blockers update live when board cards change.
- Range validation prevents starting a solve with an empty live range.
- Tests cover exact combos, class syntax, weighted syntax, board blockers, and
  per-seat range persistence.

## Stage 5 - Strategy Explorer V1

Goal: make solved artifacts useful immediately after a solve.

Deliverables:

- Replace the basic strategy grid with a proper strategy matrix:
  - one cell per hand class
  - stacked action-frequency bands
  - EV tint
  - best action label
  - unavailable hands muted
- Add action tabs or filters:
  - all actions
  - fold
  - check/call
  - bet/raise sizes
  - all-in when present
- Add a hand detail inspector:
  - exact combos represented by the class
  - action frequencies
  - EV
  - board blockers
  - range weight
- Add aggregate action cards:
  - weighted frequency per action
  - average EV
  - strategy entropy or mix indicator
- Add sortable hand table with columns:
  - hand
  - best action
  - action frequencies
  - EV
  - range weight
- Add artifact metadata summary:
  - algorithm
  - iterations
  - timestamp
  - git revision
  - player count
  - hero seat
  - street and board
- Allow changing `hero_seat` and re-solving to inspect another seat's root EVs.

Acceptance:

- A solved artifact opens directly into the strategy explorer.
- The matrix, hand table, aggregate action cards, and inspector agree on
  frequencies and EV values.
- Multiway artifacts clearly show player count, hero seat, root actor, and seat
  ranges.
- Users can filter by action and sort by EV or best action.
- Tests cover strategy aggregation, hand-class mapping, action filtering, and EV
  formatting.

## Stage 6 - Solver Results Model V2 For Commercial Analysis

Goal: expose more of the solver's multiway value than the current root-only
artifact can carry.

Current limitation: `solve_artifact` stores root action strategy and hero-combo
EV rows. That is enough for a first result view, but not enough for a
GTO Wizard-like node explorer.

Deliverables:

- Define a richer solution model that can represent:
  - action tree nodes
  - node path
  - acting seat per node
  - legal actions per node
  - average strategy per node
  - per-seat or selected-seat EV where available
  - aggregate diagnostics
- Add schema versioning and compatibility handling for old artifacts.
- Add an internal `solution_store` layer so the UI can support both old
  root-only artifacts and richer future artifacts.
- Add an action-tree browser:
  - root node
  - child actions
  - current node breadcrumb
  - active player per node
  - pot and commitment summary where available
- Add node-level strategy matrix and action summary views.
- Add a "root-only artifact" fallback state that remains useful and honest when
  old artifacts are loaded.

Acceptance:

- New solves can save and reopen the richer result format.
- Old artifacts still load and render in Strategy Explorer V1 mode.
- The action-tree browser renders real nodes and real action frequencies from
  the solution model.
- Multiway nodes show the acting seat and table state for the selected node.
- Tests cover schema migration, old artifact fallback, and node navigation.

## Stage 7 - Run Management, Progress, And Cancellation

Goal: make long solves operationally usable.

Deliverables:

- Extend the solver API used by the UI to support progress callbacks at
  iteration batches.
- Add a cancellation token checked between iteration batches.
- Add solver profiles:
  - quick sanity
  - regular analysis
  - high confidence
  - custom
- Add run queue behavior:
  - one active run
  - queued run list
  - cancel queued run
  - cancel active run when the solver API supports interruption
- Add progress display:
  - elapsed time
  - iteration count
  - player update phase if exposed
  - graph build/CFR/extraction stage
  - recent errors
- Add solve history panel:
  - timestamp
  - profile
  - iterations
  - result
  - duration
  - artifact presence
- Persist run settings with the document and global defaults.

Acceptance:

- Long runs report real progress instead of a fake spinner.
- Cancellation stops queued runs immediately and active runs at the next solver
  cancellation checkpoint.
- Solve profiles modify real solver options.
- Solve history survives save/open.
- Tests cover progress events, cancellation, failed runs, and profile
  serialization.

## Stage 8 - Study Workflows And Export

Goal: add the commercial workflows users expect once the core workbench is
solid.

Deliverables:

- Recent studies and pinned documents in the left rail.
- Study notes per document and per solved node.
- Tags and search over open/recent documents.
- Export options:
  - artifact JSON
  - spot JSON
  - strategy CSV
  - hand table CSV
  - screenshot of current matrix/table state
- Compare two runs of the same spot:
  - frequency delta by action
  - EV delta by hand
  - changed best action count
  - settings differences
- Add copyable share summary:
  - players
  - board
  - pot
  - root actor
  - hero
  - iterations
  - top aggregate actions

Acceptance:

- Users can reopen recent studies, filter by tag, and export usable result files.
- Run comparison works for two compatible artifacts and rejects incompatible
  spots with a clear reason.
- CSV exports are stable and covered by tests.
- Screenshot export captures the current selected view, not an unrelated window
  area.

## Stage 9 - Release Quality And Packaging

Goal: make the app shippable.

Deliverables:

- Add automated tests for:
  - document JSON envelope
  - structured spot editing
  - range editing
  - solver session success/failure
  - strategy view models
  - theme registry invariants
- Add UI smoke tests where practical:
  - app launches
  - theme switches
  - sample spot opens
  - sample solve starts or mocked solve completes
- Add visual regression captures for key themes and view states.
- Add performance checks for:
  - opening large artifacts
  - matrix repaint
  - hand-table sorting/filtering
  - solve console log growth
- Add accessibility checks:
  - keyboard navigation
  - focus order
  - contrast
  - readable labels/tooltips
- Harden packaging:
  - runtime dependency deployment
  - icons
  - app metadata
  - version display
  - crash-safe save behavior
- Document supported platforms and known solver/runtime limits.

Acceptance:

- CI builds `zeta-ui-holdem` and runs UI-adjacent tests.
- A clean Windows build includes the Qt runtime dependencies needed to launch.
- The app can open, edit, save, solve, reopen, and inspect a sample multiway
  document without manual setup.
- Visual checks cover at least dark, light, and high-contrast themes.

## Functional Priority Order

If scope must be cut, preserve this order:

1. Real asynchronous solving and artifact persistence.
2. Structured multiway spot builder.
3. Range editor with board-aware validation.
4. Strategy explorer for current artifact data.
5. Theme system and durable app shell.
6. Richer solution model and action-tree exploration.
7. Run progress/cancellation.
8. Study/export workflows.

Theme work is still required for commercial quality, but functionality should
not wait on decorative polish. The theme system should be built early enough to
avoid rewriting every widget later.

## First Implementation Slice

The first code slice should be small enough to land cleanly:

1. Add Boost.JSON-backed `document_json.*` for the UI document envelope.
2. Add `solver_session.*` and run `cli::solve_spot` off the UI thread.
3. Replace `solve_active_document()` with a real session launch.
4. Store the returned artifact and history entry in `spot_document`.
5. Refresh the existing artifact views after solve completion.
6. Add tests for document roundtrip and solver-session state.

That slice does not deliver the full commercial UI, but it removes the largest
prototype gap: the app becomes a real solver workbench instead of a JSON viewer
with a disabled backend.
