# dhis2API (development version)

- `dhis2_store_snapshot()` copies the database through a connection, so the copy includes committed changes still in the write-ahead log. It refuses to copy a store onto itself and stops with an error when another process holds the store open. An optional `con` argument reuses an open connection.
- `dhis2_sync_analytics()` replaces the stored rows for each successful request's data elements, periods and organisation units, so values the server no longer returns are removed, including after an empty response. Failed requests leave their old rows unchanged. The units a selection covers are resolved for UIDs, `USER_ORGUNIT`, `USER_ORGUNIT_CHILDREN`, `USER_ORGUNIT_GRANDCHILDREN`, `LEVEL-<n>`, `LEVEL-<n>-<uid>` and `OU_GROUP-<uid>` items, using `api/me` and the organisation unit metadata, and level and group items are limited to the account's data view organisation units so that rows of units the account cannot read are never removed. A new `org_units` argument, placed after `session`, supplies the metadata. An unrecognised item keeps old rows and gives a warning; unreadable data view units stop the run before anything changes.
- `dhis2_plan_run()` checks that logged full downloads cover every date from `window_start` to `window_end`, and `dhis2_sync_group()` downloads only the missing dates while still sending updates for units with existing history. The download log records the data set and data element group, so changing them under the same group name starts a new full download. Existing stores gain the new `selection` column when opened. Log rows written before this column existed have no selection, so the first `dhis2_sync_group()` run on such a store downloads the history once more; the earlier rows and values are kept. `dhis2_plan_run()` gains `window_end` and `selection` arguments and returns `gaps` and `partial`; `dhis2_log_run()` gains `selection`.
- Saved analytics and unit downloads are bound to the server, API version, account, table, period type and request they came from. Files from another server, account or request are not reused, and only the files of the current run are loaded into the database. No password or token is written to these files. Unit downloads are now saved under `backfill/<start>_<end>` folders; files saved under the earlier year folders are not reused.
- `dhis2_store_tbl()`, `dhis2_store_values()`, `dhis2_store_subannual()` and `dhis2_store_element_index()` read a store again after it is written or replaced, instead of returning the rows seen on first read. A missing or locked store is an error rather than an empty result.
- The metadata cache separates results by account, including token accounts, so one account never receives metadata cached by another.
- `dhis2_delta_window()` and `dhis2_sync_group()` use the session's `delta_days` setting. `dhis2_delta_window()` gains a `session` argument.
- `dhis2_store_values()` returns exactly the columns in `cols`, with or without `value`.
- `dhis2_event_values()` returns zero rows for events without data values, or for a tracker result with no events, and handles events that mix empty and filled data values.
- `dhis2_cache_clear()` uses the default session's `cache_dir`, or the R option when no default session is set, without opening a session.
- `dhis2_store_load()` gains a `files` argument to load named files.

# dhis2API 0.1.0

- First release.
- Sessions support credentials supplied as arguments, environment variables or system keyring entries, and an optional API version. Later calls can use a default session.
- Request settings can be supplied to a session or through R options, with package defaults as a fallback.
- Metadata functions support filters, organisation unit hierarchy columns, a local cache and names added with `dhis2_add_names()`.
- Analytics requests can be split by data element, period and organisation unit. Failed requests are split into smaller period ranges and retried. Persistent failures produce a warning and are not saved as empty results.
- Period identifiers are available for all five weekly period types.
- `dhis2_server_info()` and `dhis2_server_version()` cache server details; `dhis2_ping()` reports the version. A rejected token login explains that tokens require DHIS2 2.38 or later.
- Requests stop with an error if the server returns a web page instead of data, even with HTTP status 200. CSV responses missing required columns produce an error suggesting `format = "json"`.
- `dhis2_check_weekly_periods()` checks whether the server accepts the weekly calendar used by the package.
- `dhis2_tracker()` reads events, enrollments, tracked entities and relationships using organisation unit parameters appropriate to the resource and server version. `dhis2_event_values()` extracts event data values.
- Revised documentation and messages to explain download resumption, updates and database tables in plain language. Corrected the README's tracker support and analytics refresh descriptions.
