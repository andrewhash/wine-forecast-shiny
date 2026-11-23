### About this app

**Data.** AustralianWines.csv (monthly wine sales by varietal, 1980 to 1994), from the ADS 506 course page. The file is read from `data/` with a relative path and turned into a monthly tsibble keyed by `Varietal`. A few blank months in Rose are filled with the nearest value so ETS can fit.

**Models.** For each selected varietal the app fits three auto specified models with fable:
`TSLM(Sales ~ trend() + season())`, `ETS(Sales)` and `ARIMA(Sales)`.

**Split.** Everything up to *Training ends* is training. Everything after it (inside the date range) is validation. Metrics for both windows are on the Accuracy tab. The Future tab refits on all selected data and forecasts *h* months ahead.

**Reproduce a figure.** Set the sidebar inputs listed in the figure caption of the write up, then open the tab named in the caption.

**Packages.** shiny, bslib, fpp3 (tsibble, fable, feasts). See `renv.lock` for versions.

**Acknowledgement.** An LLM (Claude, Anthropic) was used as a supplementary resource to assist with troubleshooting and resolving coding challenges when I encountered difficulties. 
