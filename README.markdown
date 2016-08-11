This [Haskell](http://haskell.org)-based project converts a gitit-friendly markdown page to HTML for my WordPress blog. It contains several gitit plugins.

*   Remove gitit page meta-data if present (before parsing)
*   Remove "Introduction" section header
*   Remove "`<!-- references -->`"
*   Deepen all headers by two levels (temporarily off)
*   Remove comments surrounded by `<!--[ ... ]-->`
*   Fix source links: "src/" --> "/blog/src/"
