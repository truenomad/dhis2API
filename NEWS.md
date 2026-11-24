# dhis2API 0.1.1

- `dhis2_store_snapshot()` includes all committed changes, accepts an existing connection through `con`, and rejects copying a store onto itself.
- `dhis2_sync_analytics()` removes values the server no longer returns, including after an empty response. Refreshes affect only the requested data and units the account can access; failed requests preserve stored values.
- `dhis2_sync_group()` downloads missing history when the date range, units or data selection changes. Units with existing data continue receiving updates.
- Saved downloads are checked against their server, account and request before reuse. Metadata caches also separate accounts.
- Store readers detect updated or replaced databases. Missing or locked stores produce errors instead of empty results.
- Update downloads honour the session's `delta_days` setting. `dhis2_cache_clear()` uses the default session's cache directory.
- `dhis2_store_values()` honours `cols` with or without `value`; `dhis2_event_values()` handles empty and mixed event data.
- Added optional arguments for loading specific files with `dhis2_store_load()` and checking date coverage and data selection with `dhis2_plan_run()`.
- Existing stores may repeat their initial download once because older logs do not identify the data selection. Older interim files are not reused.

# dhis2API 0.1.0

- First release of the DHIS2 Web API client for R.
- Sessions support passwords or tokens, environment variables and the system keyring, with configurable request settings.
- Metadata filters and caching; analytics, raw data value and tracker downloads; monthly and weekly period helpers.
- Resumable parallel downloads and local DuckDB storage for aggregate data.
